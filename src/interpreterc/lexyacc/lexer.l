/* definitions section */

spaces                 [ \t\n]+
digits                 [0-9]+
letters                [a-zA-Z]+
openpar                \(
closepar               \)

%{
/* declarations */

#include "parser.h"
#include "y.tab.h"

void yyerror(char*);

%}

%%

nil                                  { yylval = nil; return(NIL); }
car                                  { return(CAR); }
cdr                                  { return(CDR); }
cons                                 { return(CONS); }
if                                   { return(IF); }
lambda                               { return(LAMBDA); }
quote                                { return(QUOTE); }
funcall                              { return(FUNCALL); }
{digits}+                            { yylval = mkint(atoi(yytext)); return(INT); }
({letters}|{digits})+                { yylval = mksym(yytext); return(SYMBOL); }
{openpar}                            { return(OPENPAR); }
{closepar}                           { return(CLOSEPAR); }
{spaces}                             ;
.                                    { char msg[100]; sprintf(msg, "misplaced token %s", yytext); yyerror(msg); }

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
