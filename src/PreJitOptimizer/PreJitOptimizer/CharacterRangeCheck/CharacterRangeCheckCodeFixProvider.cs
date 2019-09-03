using System.Collections.Immutable;
using System.Composition;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CodeFixes;
using Microsoft.CodeAnalysis.CodeActions;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using System;
using System.Collections.Generic;

namespace PreJitOptimizer
{
    [ExportCodeFixProvider(LanguageNames.CSharp, Name = nameof(CharacterRangeCheckCodeFixProvider)), Shared]
    public class CharacterRangeCheckCodeFixProvider : CodeFixProvider
    {
        private const string title = "Generate optimized code";

        public sealed override ImmutableArray<string> FixableDiagnosticIds
        {
            get { return ImmutableArray.Create(CharacterRangeCheckAnalyzer.DiagnosticId); }
        }

        public sealed override FixAllProvider GetFixAllProvider()
        {
            // See https://github.com/dotnet/roslyn/blob/master/docs/analyzers/FixAllProvider.md for more information on Fix All Providers
            return WellKnownFixAllProviders.BatchFixer;
        }

        public sealed override async Task RegisterCodeFixesAsync(CodeFixContext context)
        {
            var diagnostic = context.Diagnostics.First();
            var properties = diagnostic.Properties;

            var root = await context.Document.GetSyntaxRootAsync(context.CancellationToken).ConfigureAwait(false);

            var method = root.FindNode(diagnostic.Location.SourceSpan) as MethodDeclarationSyntax;

            context.RegisterCodeFix(
                CodeAction.Create(
                    title: title,
                    createChangedDocument: c => WriteOptimizedCodeAsync(context.Document, method, properties, c),
                    equivalenceKey: title),
                diagnostic);
        }

        private async Task<Document> WriteOptimizedCodeAsync(Document document, MethodDeclarationSyntax oldMethod, ImmutableDictionary<string, string> properties, CancellationToken cancellationToken)
        {
            int min = int.Parse(properties[nameof(min)]);
            int max = int.Parse(properties[nameof(max)]);
            long bitmap1 = long.Parse(properties[nameof(bitmap1)]);
            long bitmap2 = long.Parse(properties[nameof(bitmap2)]);
            string characterName = properties[nameof(characterName)];
            int range = max - min;

            List<StatementSyntax> statements = new List<StatementSyntax>(3);

            string testIdentifierNameString = "testValue";
            if (testIdentifierNameString.OrdinalEquals(characterName))
                testIdentifierNameString = "_" + testIdentifierNameString;

            var testIdentifier = SyntaxFactory.Identifier(testIdentifierNameString);
            IdentifierNameSyntax testIdentifierName = SyntaxFactory.IdentifierName(testIdentifier);
            IdentifierNameSyntax characterIdentifierName = SyntaxFactory.IdentifierName(characterName);

            // int testValue = characterName - min;
            if (min == 0 && range < 64)
            {
                testIdentifierName = characterIdentifierName;
            }
            else
            {
                ExpressionSyntax initializerValue;

                if (min == 0)
                {
                    initializerValue = characterIdentifierName;
                }
                else
                {
                    initializerValue = SyntaxFactory.BinaryExpression(
                        SyntaxKind.SubtractExpression,
                        left: characterIdentifierName,
                        right: NumericLiteral(min));
                }

                statements.Add(SyntaxFactory.LocalDeclarationStatement(
                    SyntaxFactory.VariableDeclaration(
                        SyntaxFactory.PredefinedType(
                            SyntaxFactory.Token(SyntaxKind.IntKeyword)),
                        SyntaxFactory.SingletonSeparatedList(
                            SyntaxFactory.VariableDeclarator(
                                testIdentifier,
                                argumentList: null,
                                initializer: SyntaxFactory.EqualsValueClause(initializerValue))))));
            }

            // if (testValue > range) return false;
            statements.Add(SyntaxFactory.IfStatement(
                SyntaxFactory.BinaryExpression(
                    SyntaxKind.GreaterThanExpression,
                    left: min == 0 ? (ExpressionSyntax)testIdentifierName : testIdentifierName.CastToUInt32(),
                    right: NumericLiteral(max - min)),
                SyntaxFactory.ReturnStatement(
                    SyntaxFactory.LiteralExpression(
                        SyntaxKind.FalseLiteralExpression))));

            var endOfLineTrivia = SyntaxFactory.EndOfLine(Environment.NewLine);

            // return ((bitmap >> testValue) & 1) != 0;
            // or
            // return testValue < 64 ? ((bitmap1 >> testValue) & 1) != 0 : ((bitmap2 >> (testValue - 64)) & 1) != 0
            ExpressionSyntax returnExpression;
            if (range < 64)
            {
                returnExpression = CreateBitmapCheckExpression(
                    bitmap1,
                    testIdentifierName,
                    use32bitBitmap: range < 32);
            }
            else
            {
                returnExpression = SyntaxFactory.ConditionalExpression(
                    SyntaxFactory.BinaryExpression(
                        SyntaxKind.LessThanExpression,
                        left: testIdentifierName,
                        right: NumericLiteral(64)),
                    whenTrue: CreateBitmapCheckExpression(
                        bitmap1,
                        testIdentifierName,
                        use32bitBitmap: false)
                            .WithLeadingTrivia(endOfLineTrivia),
                    whenFalse: CreateBitmapCheckExpression(
                        bitmap2,
                        SyntaxFactory.ParenthesizedExpression(
                            SyntaxFactory.BinaryExpression(
                                SyntaxKind.SubtractExpression,
                                left: testIdentifierName,
                                right: NumericLiteral(64))),
                        use32bitBitmap: range < 96)
                            .WithLeadingTrivia(endOfLineTrivia));
            }
            statements.Add(SyntaxFactory.ReturnStatement(returnExpression));

            var oldBody = (SyntaxNode)oldMethod.Body ?? oldMethod.ExpressionBody;
            var oldBodyCommentedOutBlock = SyntaxFactory.Comment(
                "/* " + string.Concat(oldBody.ChildNodes().Select(node => node.GetText())).Trim(' ', '\n', '\r') + " */");

            statements[0] = statements[0].WithLeadingTrivia(oldBodyCommentedOutBlock, endOfLineTrivia, endOfLineTrivia);

            var newBody = SyntaxFactory.Block(statements);

            var newMethod = oldMethod.WithBody(newBody).WithExpressionBody(null).WithSemicolonToken(default);

            var oldRoot = await document.GetSyntaxRootAsync(cancellationToken);
            var newRoot = oldRoot.ReplaceNode(oldMethod, newMethod);
            return document.WithSyntaxRoot(newRoot);
        }

        static LiteralExpressionSyntax NumericLiteral(int value)
        {
            return SyntaxFactory.LiteralExpression(
                SyntaxKind.NumericLiteralExpression,
                SyntaxFactory.Literal(value));
        }
        static LiteralExpressionSyntax NumericLiteral(long value)
        {
            return SyntaxFactory.LiteralExpression(
                SyntaxKind.NumericLiteralExpression,
                SyntaxFactory.Literal(value));
        }
        static BinaryExpressionSyntax CreateBitmapCheckExpression(long bitmap, ExpressionSyntax testValueExpression, bool use32bitBitmap)
        {
            return SyntaxFactory.BinaryExpression(
                SyntaxKind.NotEqualsExpression,
                left: SyntaxFactory.ParenthesizedExpression(
                    SyntaxFactory.BinaryExpression(
                        SyntaxKind.BitwiseAndExpression,
                        left: SyntaxFactory.ParenthesizedExpression(
                            SyntaxFactory.BinaryExpression(
                                SyntaxKind.RightShiftExpression,
                                left: use32bitBitmap ? NumericLiteral((int)bitmap) : NumericLiteral(bitmap),
                                right: testValueExpression)),
                        right: NumericLiteral(1))),
                right: NumericLiteral(0));
        }
    }
}
