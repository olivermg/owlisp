/* definitions section */

digits                 [0-9]+
letter                 [a-zA-Z]+
openpar                \(
closepar               \)

%{
/* declarations */

#include "y.tab.h"

void yyerror(char*);

%}

%%

{digits}              { yylval = atoi(yytext); return(NUM); }
{openpar}             return(OPENPAR);
{closepar}            return(CLOSEPAR);
[\n\t ]               ;
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
