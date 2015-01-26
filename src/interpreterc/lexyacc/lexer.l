/* definitions section */

spaces                 [ \t\n]+
digits                 [0-9]+
letters                [a-zA-Z]+
openpar                \(
closepar               \)

%{
/* declarations */

#include "y.tab.h"

void yyerror(char*);

%}

%%

{digits}              { yylval.integer = atoi(yytext); return(INT); }
{letters}             { yylval.string = yytext; return(SYMBOL); }
{openpar}             return(OPENPAR);
{closepar}            return(CLOSEPAR);
{spaces}              return(SPACE);
.                     { char msg[100]; sprintf(msg, "misplaced token %s", yytext); yyerror(msg); }

%%

/* code section */

/*
void yyerror(char* msg)
{
    fprintf(stderr, "ERROR: %s\n", msg);
    exit(1);
}

int yywrap()
{
    return 1;
}

int main()
{
    yylex();
    printf("openpars: %d\n", openpars);
    return 0;
}
*/
