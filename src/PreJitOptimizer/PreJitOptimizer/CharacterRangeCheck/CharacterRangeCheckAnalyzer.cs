using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Diagnostics;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Diagnostics;
using System.Linq;

namespace PreJitOptimizer
{
    [DiagnosticAnalyzer(LanguageNames.CSharp)]
    public class CharacterRangeCheckAnalyzer : DiagnosticAnalyzer
    {
        public const string DiagnosticId = "CharacterListCheck";

        private static readonly DiagnosticDescriptor Rule = new DiagnosticDescriptor(
            DiagnosticId, nameof(CharacterRangeCheckAnalyzer), "This can be faster", "Performance", DiagnosticSeverity.Info, isEnabledByDefault: true);

        public override ImmutableArray<DiagnosticDescriptor> SupportedDiagnostics { get { return ImmutableArray.Create(Rule); } }

        public override void Initialize(AnalysisContext context)
        {
            context.RegisterSyntaxNodeAction(AnalyzeMethod, SyntaxKind.MethodDeclaration);
        }

        private static void AnalyzeMethod(SyntaxNodeAnalysisContext context)
        {
            var method = context.Node as MethodDeclarationSyntax;

            if (!IsPredefinedType(method.ReturnType, SyntaxKind.BoolKeyword))
                return;

            var parameters = method.ParameterList.ChildNodes().Cast<ParameterSyntax>().ToList();
            if (parameters.Count != 1)
                return;

            var param = parameters[0];

            if (!IsPredefinedType(param.Type, SyntaxKind.CharKeyword))
                return;

            var characterName = param.Identifier.ValueText;
            List<Range> ranges = new List<Range>();

            ExpressionSyntax expression;

            if (method.Body is null)
            {
                expression = method.ExpressionBody.Expression;
            }
            else
            {
                var statements = method.Body.Statements;
                if (statements.Count != 1 && statements.Count != 2)
                    return;

                if (!(statements[0] is ReturnStatementSyntax returnStatement))
                {
                    if (!(statements[0] is SwitchStatementSyntax switchStatement))
                        return;

                    if (!IsCharacterListSwitchStatement(switchStatement, characterName, ranges, out bool hasDefaultReturnFalse))
                        return;

                    if (hasDefaultReturnFalse)
                    {
                        if (statements.Count != 1)
                            return;
                    }
                    else
                    {
                        if (statements.Count != 2)
                            return;

                        if (!statements[1].IsReturnBoolStatement(false))
                            return;
                    }

                    ReportDiagnosticIfInRange(context, characterName, ranges);
                    return;
                }

                if (statements.Count != 1)
                    return;

                expression = returnStatement.Expression;
            }

            if (!(expression is BinaryExpressionSyntax characterCheckExpression))
                return;

            if (!IsAggregatedCharacterRangeCheckExpression(characterCheckExpression, characterName, ranges))
            {
                if (ranges.Count != 0)
                    ranges = new List<Range>();

                if (!IsStringLiteralIndexOfCharacterNotMinusOneExpression(characterCheckExpression, characterName, ranges))
                {
                    return;
                }
            }
                

            ReportDiagnosticIfInRange(context, characterName, ranges);
        }

        static void ReportDiagnosticIfInRange(SyntaxNodeAnalysisContext context, string characterName, List<Range> ranges)
        {
            Debug.Assert(ranges != null);

            int min = int.MaxValue, max = int.MinValue;

            foreach (var range in ranges)
            {
                min = Math.Min(min, range.Min);
                max = Math.Max(max, range.Max);
            }

            int checkRange = max - min + 1;

            if (checkRange > 128)
                return;

            if (max < 64 || (checkRange > 64 && max < 128))
                min = 0;

            long bitmap1 = 0, bitmap2 = 0;
            foreach (var range in ranges)
            {
                for (int i = range.Min; i <= range.Max; i++)
                {
                    int shift = i - min;
                    if (shift < 64)
                        bitmap1 |= 1L << shift;
                    else
                        bitmap2 |= 1L << (shift - 64);
                }
            }

            var properties = new Dictionary<string, string>
            {
                { nameof(min), min.ToString() },
                { nameof(max), max.ToString() },
                { nameof(bitmap1), bitmap1.ToString() },
                { nameof(bitmap2), bitmap2.ToString() },
                { nameof(characterName), characterName }
            };
            var diagnostic = Diagnostic.Create(Rule, context.Node.GetLocation(), properties: ImmutableDictionary.CreateRange(properties));

            context.ReportDiagnostic(diagnostic);
        }

        static bool IsPredefinedType(TypeSyntax type, SyntaxKind kind)
        {
            return type is PredefinedTypeSyntax predefinedType
                && predefinedType.Keyword.IsKind(kind);
        }

        struct Range
        {
            public int Min, Max;
            public Range(int min, int max)
            {
                Min = min;
                Max = max;
            }
            public Range(int minMax)
            {
                Min = Max = minMax;
            }
        }

        static bool IsCharacterListSwitchStatement(SwitchStatementSyntax switchStatement, string characterName, List<Range> ranges, out bool hasDefaultReturnFalse)
        {
            hasDefaultReturnFalse = default;

            if (!(switchStatement.Expression is IdentifierNameSyntax identifier))
                return false;

            if (!identifier.IsNamed(characterName))
                return false;

            var sections = switchStatement.Sections;

            if (sections.Count != 1 && sections.Count != 2)
                return false;

            if (sections.Count == 2)
            {
                var defaultCase = sections[1];

                if (!(defaultCase.Labels.Count == 1 && defaultCase.Labels[0] is DefaultSwitchLabelSyntax))
                    return false;

                if (!(defaultCase.Statements.Count == 1 && defaultCase.Statements[0].IsReturnBoolStatement(false)))
                    return false;

                hasDefaultReturnFalse = true;
            }

            var charactersCase = sections[0];

            if (!(charactersCase.Statements.Count == 1 && charactersCase.Statements[0].IsReturnBoolStatement(true)))
                return false;

            foreach (var label in charactersCase.Labels)
            {
                if (!(label is CaseSwitchLabelSyntax caseLabel))
                    return false;

                if (!(caseLabel.Value is LiteralExpressionSyntax literalExpression))
                    return false;

                if (!literalExpression.IsInt32OrCharLiteral(out int value))
                    return false;

                ranges.Add(new Range(value));
            }

            return true;
        }
        static bool IsAggregatedCharacterRangeCheckExpression(BinaryExpressionSyntax expression, string characterName, List<Range> ranges)
        {
            if (!expression.IsKind(SyntaxKind.LogicalOrExpression))
                return false;

            var left = expression.Left;

            if (!IsSingleCharacterRangeCheckExpression(expression.Right, characterName, out Range range))
                return false;

            ranges.Add(range);

            if (left.IsKind(SyntaxKind.LogicalOrExpression))
                return IsAggregatedCharacterRangeCheckExpression(left as BinaryExpressionSyntax, characterName, ranges);

            if (!IsSingleCharacterRangeCheckExpression(left, characterName, out range))
                return false;

            ranges.Add(range);

            return true;
        }
        static bool IsSingleCharacterRangeCheckExpression(ExpressionSyntax expression, string characterName, out Range range)
        {
            if (expression.IsKind(SyntaxKind.EqualsExpression))
            {
                return IsCharacterEqualsLiteralExpression(expression as BinaryExpressionSyntax, characterName, out range);
            }
            else if (expression is ParenthesizedExpressionSyntax parenthesizedExpression)
            {
                return IsCharacterInLiteralRangeExpression(parenthesizedExpression.Expression, characterName, out range);
            }
            else
            {
                range = default;
                return false;
            }
        }
        static bool IsCharacterEqualsLiteralExpression(BinaryExpressionSyntax expression, string characterName, out Range range)
        {
            Debug.Assert(expression.IsKind(SyntaxKind.EqualsExpression));

            if (IsComparisonBetweenCharacterAndLiteralExpression(expression, characterName, out int literalValue))
            {
                range = new Range(literalValue);
                return true;
            }
            else
            {
                range = default;
                return false;
            }
        }
        static bool IsCharacterInLiteralRangeExpression(ExpressionSyntax expression, string characterName, out Range range)
        {
            range = default;

            if (!expression.IsKind(SyntaxKind.LogicalAndExpression))
                return false;

            var andExpression = expression as BinaryExpressionSyntax;

            if (!(andExpression.Left is BinaryExpressionSyntax left))
                return false;

            if (!(andExpression.Right is BinaryExpressionSyntax right))
                return false;

            BinaryExpressionSyntax greaterThan;
            BinaryExpressionSyntax lessThan;

            if (left.IsKind(SyntaxKind.GreaterThanExpression) || left.IsKind(SyntaxKind.GreaterThanOrEqualExpression))
            {
                greaterThan = left;
                lessThan = right;
            }
            else if (right.IsKind(SyntaxKind.GreaterThanExpression) || right.IsKind(SyntaxKind.GreaterThanOrEqualExpression))
            {
                greaterThan = right;
                lessThan = left;
            }
            else return false;

            if (!lessThan.IsKind(SyntaxKind.LessThanExpression) && !lessThan.IsKind(SyntaxKind.LessThanOrEqualExpression))
                return false;

            if (!IsComparisonBetweenCharacterAndLiteralExpression(greaterThan, characterName, out int greaterThanValue))
                return false;

            if (!IsComparisonBetweenCharacterAndLiteralExpression(lessThan, characterName, out int lessThanValue))
                return false;

            if (greaterThan.IsKind(SyntaxKind.GreaterThanExpression))
                greaterThanValue++;

            if (lessThan.IsKind(SyntaxKind.LessThanExpression))
                lessThanValue--;

            if (greaterThanValue > lessThanValue)
                return false;

            range = new Range(greaterThanValue, lessThanValue);
            return true;
        }
        static bool IsComparisonBetweenCharacterAndLiteralExpression(BinaryExpressionSyntax comparison, string characterName, out int literalValue)
        {
            literalValue = default;

            var left = comparison.Left;
            var right = comparison.Right;

            IdentifierNameSyntax identifier;
            LiteralExpressionSyntax literal;

            if (left.IsKind(SyntaxKind.CharacterLiteralExpression) || left.IsKind(SyntaxKind.NumericLiteralExpression))
            {
                identifier = right as IdentifierNameSyntax;
                literal = left as LiteralExpressionSyntax;
            }
            else if (right.IsKind(SyntaxKind.CharacterLiteralExpression) || right.IsKind(SyntaxKind.NumericLiteralExpression))
            {
                identifier = left as IdentifierNameSyntax;
                literal = right as LiteralExpressionSyntax;
            }
            else return false;

            if (identifier is null)
                return false;

            if (!identifier.IsNamed(characterName))
                return false;

            return literal.IsInt32OrCharLiteral(out literalValue);
        }
        static bool IsStringLiteralIndexOfCharacterNotMinusOneExpression(BinaryExpressionSyntax expression, string characterName, List<Range> ranges)
        {
            var left = expression.Left;
            var right = expression.Right;

            LiteralExpressionSyntax literal;
            InvocationExpressionSyntax invocation;

            if (left.IsKind(SyntaxKind.NumericLiteralExpression))
            {
                literal = left as LiteralExpressionSyntax;
                invocation = right as InvocationExpressionSyntax;
            }
            else if (right.IsKind(SyntaxKind.NumericLiteralExpression))
            {
                literal = right as LiteralExpressionSyntax;
                invocation = left as InvocationExpressionSyntax;
            }
            else return false;

            if (invocation is null)
                return false;

            var arguments = invocation.ArgumentList.Arguments;

            if (arguments.Count != 1)
                return false;

            if (!(arguments[0].Expression is IdentifierNameSyntax argumentName))
                return false;

            if (!argumentName.IsNamed(characterName))
                return false;

            if (!(invocation.Expression is MemberAccessExpressionSyntax memberAccess))
                return false;

            if (!memberAccess.IsKind(SyntaxKind.SimpleMemberAccessExpression))
                return false;

            if (!memberAccess.Name.Identifier.ValueText.OrdinalEquals("IndexOf"))
                return false;

            if (!(memberAccess.Expression is LiteralExpressionSyntax stringLiteral))
                return false;

            if (!stringLiteral.IsKind(SyntaxKind.StringLiteralExpression))
                return false;

            string searchLiteral = (string)stringLiteral.Token.Value;

            if (!literal.IsInt32Literal(out int value))
                return false;

            if (expression.IsKind(SyntaxKind.NotEqualsExpression))
            {
                if (value != -1)
                    return false;
            }
            else if (expression.IsKind(SyntaxKind.GreaterThanExpression))
            {
                if (literal == left || value != -1)
                    return false;
            }
            else if (expression.IsKind(SyntaxKind.GreaterThanOrEqualExpression))
            {
                if (literal == left || value != 0)
                    return false;
            }
            else if (expression.IsKind(SyntaxKind.LessThanExpression))
            {
                if (literal == right || value != -1)
                    return false;
            }
            else if (expression.IsKind(SyntaxKind.LessThanOrEqualExpression))
            {
                if (literal == right || value != 0)
                    return false;
            }
            else return false;

            foreach (var c in searchLiteral)
            {
                ranges.Add(new Range(c));
            }

            return true;
        }
    }
}
