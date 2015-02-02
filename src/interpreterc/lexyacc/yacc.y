%{
#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <string.h>
#include "parser.h"
#include "y.tab.h"


//#define YYSTYPE obj_t*

int yylex();
int yyerror();

%}

%parse-param {obj_t* env}

/*
%union {
    obj_t* obj;
    char* string;
}
*/

%token                  NIL
%token                  CAR
%token                  CDR
%token                  CONS
%token                  IF
%token                  LAMBDA
%token                  QUOTE
%token                  FUNCALL
%token			INT
%token                  SYMBOL

%start program

%%

program:	exprseq { program = $1; }
	;

exprseq:	{ $$ = nil; }
	|	expr exprseq { $$ = cons($1, $2); }
		;

expr:		atom
	|	cons
		;

atom: 		NIL
	|	INT
	|	SYMBOL
		;

cons:		'(' primopexpr ')' { $$ = $2; }
		;

primopexpr: 	carexpr
	|	cdrexpr
	|	consexpr
	|	ifexpr
	|	lambdaexpr
	|	quoteexpr
	|	funcallexpr
		;

carexpr:	CAR expr { $$ = car($2); }
		;

cdrexpr: 	CDR expr { $$ = cdr($2); }
		;

consexpr:	CONS expr expr { $$ = cons($2, $3); }
		;

ifexpr:		IF expr expr expr { $$ = mkif($2, $3, $4); }
		;

lambdaexpr:	LAMBDA lambdalist exprseq { $$ = mkproc($2, $3, env); }
		;

quoteexpr:	QUOTE expr { $$ = cons(quote, $2); }
		;

funcallexpr:	FUNCALL expr exprseq { $$ = mkapply($2, $3); }
		;

lambdalist:	'(' symbolseq ')' { $$ = $2; }
		;

symbolseq:      { $$ = nil; }
	|	SYMBOL symbolseq { $$ = cons($1, $2); } // TODO: right-associative parsing may exhaust the parser stack
		;

%%

obj_t* new_obj(type_t type, unsigned long numargs, ...)
{
    va_list va;

    obj_t* newobj = malloc(sizeof(obj_t) + (numargs - 1)*sizeof(obj_t*));
    newobj->type = type;
    va_start(va, numargs);
    for (unsigned long i = 0; i < numargs; i++) {
	newobj->objs[i] = va_arg(va, obj_t*);
    }
    va_end(va);

    return newobj;
}

obj_t* multiple_extend(obj_t* env, obj_t* syms, obj_t* vals)
{
  return null(syms)
    ? env
    : multiple_extend(extend(env, car(syms), car(vals)),
		      cdr(syms), cdr(vals));
}

obj_t* find_symbol(char* name)
{
  if (!strcmp(name, "nil"))
    return nil;
  obj_t* symlist;
  for (symlist = interned_syms; !null(symlist); symlist = cdr(symlist)) {
    obj_t* sym = car(symlist);
    if (!strcmp(name, symname(sym)))
      break;
  }
  if (!null(symlist)) {
    return car(symlist);
  } else {
    return nil;
  }
}

obj_t* intern(char* name)
{
  obj_t* sym = find_symbol(name);
  if (!null(sym))
    return sym;
  sym = mksym(name);
  interned_syms = cons(sym, interned_syms);
  return sym;
}

obj_t* assoc(obj_t* key, obj_t* alist)
{
  for (obj_t* l = alist; !null(l); l = cdr(l)) {
    if (eq(car(car(l)), key))
      return car(l);
  }
  return nil;
}

obj_t* progn(obj_t* exprs, obj_t* env)
{
  obj_t* ret = nil;
  for (obj_t* restexprs = exprs; !null(restexprs); restexprs = cdr(restexprs)) {
    ret = eval(car(restexprs), env);
  }
  return ret;
}

obj_t* apply(obj_t* proc, obj_t* vals, obj_t* env)
{
  switch (proc->type) {
  case TSYM:
    break;
  case TPROC:
    return progn(proccode(proc), multiple_extend(proclenv(proc), procparams(proc), vals));
    break;
  default:
    error("unknown type for apply");
    break;
  }
  return nil;
}

obj_t* evlis(obj_t* exprs, obj_t* denv)
{
  if (null(exprs))
    return nil;
  return cons(eval(car(exprs), denv),
	      evlis(cdr(exprs), denv));
}

obj_t* eval(obj_t* expr, obj_t* denv)
{
  obj_t* tmp;
  switch (expr->type) {
  case TSYM:
    tmp = assoc(expr, denv);
    if (null(tmp))
      error("unbound symbol");
    return cdr(tmp);
  case TINT:
    return expr;
  case TCONS:
    return apply(car(expr), evlis(cdr(expr), denv), denv);
    break;
  case TPROC:
    return expr;
    break;
  case TAPPLY:
    return apply(car(expr), evlis(cdr(expr), denv), denv);
    break;
  case TIF:
    tmp = eval(ifpred(expr), denv);
    if (!null(tmp))
	return eval(ifthen(expr), denv);
    else
	return eval(ifelse(expr), denv);
    break;
  }
  return nil;
}

void print_obj(obj_t* obj)
{
  switch (obj->type) {
  case TSYM:
    printf("SYM(%s)", symname(obj));
    break;
  case TINT:
    printf("INT(%d)", intvalue(obj));
    break;
  case TCONS:
    printf("CONS(");
    print_obj(car(obj));
    printf(", ");
    print_obj(cdr(obj));
    printf(")");
    break;
  case TPROC:
    printf("PROC(");
    print_obj(procparams(obj));
    printf(", ");
    print_obj(proccode(obj));
    printf(", ");
    print_obj(proclenv(obj));
    printf(")");
    break;
  case TAPPLY:
    printf("APPLY(");
    print_obj(applyproc(obj));
    printf(", ");
    print_obj(applyargs(obj));
    printf(")");
    break;
  case TIF:
    printf("IF(");
    print_obj(ifpred(obj));
    printf(", ");
    print_obj(ifthen(obj));
    printf(", ");
    print_obj(ifelse(obj));
    printf(")");
    break;
  default:
    printf("unknown object of type %d!\n", obj->type);
    break;
  }
}

void init()
{
  nil = new_obj(TSYM, 1, 0);
  setcar(nil, nil);

  quote = mksym("quote");

  interned_syms = cons(nil, nil);
  global_env = cons(cons(nil, nil), nil);
}


int yyerror(char* msg)
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
    init();

    /*
    obj_t* o = cons(mksym("a"), cons(mksym("b"), nil));
    print_obj(o);
    */

    yyparse(global_env);
    printf("PROGRAM:");
    print_obj(program);
    printf("\n");

    obj_t* evald = eval(car(program), global_env);
    printf("EVALD:");
    print_obj(evald);
    printf("\n");

    return 0;
}
