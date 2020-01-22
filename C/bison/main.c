/*
 * main.c file
 * Wikipedia GNU Bison example
 */

#include "Expression.h"
#include "Parser.h"
#include "Lexer.h"

#include <stdio.h>
#include <string.h>
#include <assert.h>

int yyparse(SExpression ** expression, yyscan_t scanner);

SExpression *getAST(const char *expr)
{
    SExpression *expression;
    yyscan_t scanner;
    YY_BUFFER_STATE state;

    if (yylex_init(&scanner)) {
	/* could not initialize */
	return NULL;
    }

    state = yy_scan_string(expr, scanner);

    if (yyparse(&expression, scanner)) {
	/* error parsing */
	return NULL;
    }

    yy_delete_buffer(state, scanner);

    yylex_destroy(scanner);

    return expression;
}

int evaluate(SExpression * e)
{
    switch (e->type) {
    case eVALUE:
	return e->value;
    case eMULTIPLY:
	return evaluate(e->left) * evaluate(e->right);
    case eADD:
	return evaluate(e->left) + evaluate(e->right);
    default:
	/* should not be here */
	return 0;
    }
}

#define STR_S 3

char *extend(char *indent)
{
    size_t size = strlen(indent) + 2 + STR_S;
    char *new = malloc(sizeof(char) * size);
    assert(new != NULL);
    strcpy(new, indent);
    strcat(indent, "  ");
    return new;
}

void recur(SExpression *e, char *op);

void _plisp(SExpression * e, int outer)
{
    switch (e->type) {
    case eVALUE:
	printf("%d", e->value);
	break;
    case eADD:
	recur(e, "+");
	break;
    case eMULTIPLY:
	recur(e, "*");
	break;
    }

    if (outer)
	putchar('\n');
}

void recur(SExpression *e, char *op)
{
    printf("(%s ", op);
    _plisp(e->left, 0);
    printf(" ");
    _plisp(e->right, 0);
    printf(")");
}

void plisp(SExpression *e)
{
    _plisp(e, 1);
}

int main(void)
{
    char test[] = " 4 + 2*10 + 3*( 5 + 1 )";
    SExpression *e = getAST(test);
    plisp(e);
    int result = evaluate(e);
    printf("Result of '%s' is %d\n", test, result);
    deleteExpression(e);
    return 0;
}
