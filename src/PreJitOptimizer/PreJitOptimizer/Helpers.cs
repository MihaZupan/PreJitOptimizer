using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using System;

namespace PreJitOptimizer
{
    internal static class Helpers
    {
        public static bool IsInt32OrCharLiteral(this LiteralExpressionSyntax literal, out int value)
        {
            object tokenValue = literal.Token.Value;

            if (tokenValue is int intValue)
            {
                value = intValue;
            }
            else if (tokenValue is char charValue)
            {
                value = charValue;
            }
            else
            {
                value = default;
                return false;
            }

            return true;
        }
        public static bool IsInt32Literal(this LiteralExpressionSyntax literal, out int value)
        {
            object tokenValue = literal.Token.Value;

            if (tokenValue is int intValue)
            {
                value = intValue;
                return true;
            }
            else
            {
                value = default;
                return false;
            }
        }

        public static bool IsReturnBoolStatement(this StatementSyntax statement, bool value)
        {
            return statement is ReturnStatementSyntax returnStatement
                && returnStatement.Expression is LiteralExpressionSyntax literalExpression
                && literalExpression.Token.Value is bool boolLiteral
                && boolLiteral == value;
        }

        public static CastExpressionSyntax CastToUInt32(this IdentifierNameSyntax identifierName)
        {
            return SyntaxFactory.CastExpression(
                SyntaxFactory.PredefinedType(
                    SyntaxFactory.Token(SyntaxKind.UIntKeyword)),
                identifierName);
        }

        public static bool IsNamed(this IdentifierNameSyntax identifierName, string name)
            => identifierName.Identifier.ValueText.OrdinalEquals(name);

        public static bool OrdinalEquals(this string a, string b)
            => a.Equals(b, StringComparison.Ordinal);
    }
}
