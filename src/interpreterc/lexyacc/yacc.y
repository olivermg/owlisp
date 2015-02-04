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

/* %define api.pure full */
%parse-param {obj_t* frames}

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
	|	symbolref
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

lambdaexpr:	LAMBDA lambdalist { frames = register_frame(frames, $2); }
		    exprseq { $$ = mklambda($4); frames = drop_frame(frames); }
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

symbolref:	SYMBOL { $$ = find_symbol_address(frames, $1, 0); }
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

obj_t* multiple_extend(obj_t* env, obj_t* vals)
{
    obj_t* newenv = nil;
    for (obj_t* val = vals; !null(val); val = cdr(val)) {
	newenv = cons(car(val), newenv);
    }
    return cons(newenv, env);
    /*
  return null(vals)
    ? env
    : multiple_extend(extend(env, car(vals)), cdr(vals));
    */
}

obj_t* register_frame(obj_t* frame, obj_t* syms)
{
  return cons(syms, frame);
}

obj_t* drop_frame(obj_t* frame)
{
  return cdr(frame);
}

obj_t* find_symbol_address(obj_t* frame, obj_t* sym, int frameidx)
{
  if (null(frame))
    error("unbound symbol!");
  if (null(sym))
    return nil;
  int varidx;
  obj_t* cursym;
  for (varidx = 0, cursym = car(frame); !null(cursym); varidx += 1, cursym = cdr(cursym)) {
    if (eq(car(cursym), sym))
      return mkref(frameidx, varidx);
  }
  return find_symbol_address(cdr(frame), sym, frameidx+1);
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
    printf("applying on "); print_obj(proc, 0); printf("\n");
  switch (proc->type) {
  case TSYM:
    break;
  case TPROC:
      printf("proclenv: "); print_obj(proclenv(proc), 0); printf("\n");
    return progn(proccode(proc), multiple_extend(proclenv(proc), vals));
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
  case TREF:
    return expr;
  case TINT:
    return expr;
  case TCONS:
    return apply(eval(car(expr), denv), evlis(cdr(expr), denv), denv);
    break;
  case TLAMBDA:
    return mkproc(lambdacode(expr), denv);
    break;
  case TPROC:
    return expr;
    break;
  case TAPPLY:
    return apply(eval(car(expr), denv), evlis(cdr(expr), denv), denv);
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

void indent(int indentation)
{
    for (int i = 0; i < indentation; i++) {
	printf(" ");
    }
}

void print_cons(obj_t* obj, int indentation)
{
    indent(indentation); printf("(");
    char* separator = "";
    for (obj_t* cur = obj; !null(cur); cur = cdr(cur)) {
	printf("%s", separator);
	separator = " ";
	print_obj(car(cur), 0);
    }
    printf(")");
}

void print_obj(obj_t* obj, int indentation)
{
  switch (obj->type) {
  case TSYM:
    indent(indentation); printf("SYM[%s]", symname(obj));
    break;
  case TREF:
    indent(indentation); printf("REF[%d,%d]", refframe(obj), refvar(obj));
    break;
  case TINT:
    indent(indentation); printf("INT[%d]", intvalue(obj));
    break;
  case TCONS:
    print_cons(obj, indentation);
    break;
  case TLAMBDA:
    indent(indentation); printf("LAMBDA[");
    print_obj(lambdacode(obj), indentation);
    printf("]");
    break;
  case TPROC:
    indent(indentation); printf("PROC[c:");
    //    print_obj(procparams(obj), indentation);
    //    printf(",\n"); indent(indentation); printf("     c:");
    print_obj(proccode(obj), indentation);
    printf(",\n"); indent(indentation); printf("     e:");
    print_obj(proclenv(obj), indentation);
    printf("]");
    break;
  case TAPPLY:
    indent(indentation); printf("APPLY[");
    print_obj(applyproc(obj), indentation);
    printf(", ");
    print_obj(applyargs(obj), indentation);
    printf("]");
    break;
  case TIF:
    indent(indentation); printf("IF[");
    print_obj(ifpred(obj), indentation);
    printf(", ");
    print_obj(ifthen(obj), indentation);
    printf(", ");
    print_obj(ifelse(obj), indentation);
    printf("]");
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

    obj_t* frame1 = cons(intern("a"), cons(intern("b"), nil));
    obj_t* frame2 = cons(intern("c"), cons(intern("d"), cons(intern("e"), nil)));
    obj_t* frames = cons(frame1, cons(frame2, nil));
    obj_t* addr = find_symbol_address(frames, intern("d"), 0);
    printf("found address: "); print_obj(addr, 0); printf("\n");

    obj_t* e = cons(nil, nil);
    print_obj(e, 0); printf("\n");
    obj_t* e1 = extend(e, mkint(11));
    print_obj(e1, 0); printf("\n");
    obj_t* e2 = extend(e1, mkint(22));
    print_obj(e2, 0); printf("\n\n");

    obj_t* env = nil;
    obj_t* vals1 = cons(mkint(11), cons(mkint(22), nil));
    obj_t* vals2 = cons(mkint(33), cons(mkint(44), nil));
    obj_t* env1 = multiple_extend(env, vals1);
    obj_t* env2 = multiple_extend(env1, vals2);
    printf("env: "); print_obj(env2, 0); printf("\n");

    obj_t* parse_frames = nil;
    yyparse(parse_frames);
    printf("PROGRAM:");
    print_obj(program, 0);
    printf("\n");

    obj_t* evald = eval(car(program), global_env);
    printf("EVALD:");
    print_obj(evald, 0);
    printf("\n");

    return 0;
}
