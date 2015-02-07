#include <stdio.h>
#include <stdlib.h>
//#include <ctype.h>
#include <string.h>
#include <stdarg.h>


#define STREAMCHUNKSIZE 1024
#define TOKENCHUNKSIZE 16
#define TOKENLISTCHUNKSIZE 16


#define mksym(x) new_obj(SYM, 1, (x))
#define symname(x) (null(x) ? "nil" : (char*)((x)->objs[0]))

#define mkselfeval(x) new_obj(SELFEVAL, 1, (x)) // TODO: don't put value in ptr memory location
#define value(x) (int)((x)->objs[0])

#define mkproc(args,code,env) new_obj(PROC, 3, (args), (code), (env))
#define procargs(p) ((p)->objs[0])
#define proccode(p) ((p)->objs[1])
#define procenv(p) ((p)->objs[2])

#define eq(x,y) ((x) == (y))
#define null(x) eq(x, nil)

#define cons(x,y) new_obj(CONS, 2, (x), (y))
#define car(x) ((x)->objs[0])
#define cdr(x) ((x)->objs[1])
#define setcar(x,v) ((x)->objs[0] = (v))
#define setcdr(x,v) ((x)->objs[1] = (v))

#define extend(env,sym,val) cons(cons((sym), (val)), (env))

#define error(x) do { fprintf(stderr, "ERROR: %s\n", x); exit(1); } while (0)

/*
 *
 * implement the following lisp special forms natively:
 * car, cdr, quote, cond, cons, atom, eq, lambda
 *
 */

/*
enum tokentype {
	ATOM,
	CONS
};

struct _expr_t;
struct _token_t;

typedef char* token_p;

typedef struct _token_t {
	tokentype type;
	union {
		token_p atom
	};
} token_t;

typedef struct _expr_t {
	token_p token;
	struct _expr_t* next;
} expr_t;
*/

typedef struct _token_t {
  size_t size;
  size_t used;
  char* value;
  struct _token_t* next;
  struct _token_t* prev;
} token_t;


typedef enum _type_t {
  SYM,
  SELFEVAL,
  CONS,
  PROC
} type_t;

typedef struct _obj_t {
  type_t type;
  struct _obj_t* objs[1];
} obj_t;

/*
expr_t* new_expr(char* tokencontent, expr_t* next)
{
	expr_t* newexpr = malloc(sizeof(expr_t));
	newexpr->token = tokencontent;
	newxepr->next = next;

	return newexpr;
}
*/

obj_t* interned_syms;
obj_t* global_env;
obj_t* nil;


obj_t* new_obj(type_t type, unsigned long numargs, ...);
obj_t* intern(char* name);
obj_t* eval(obj_t* expr, obj_t* env);
obj_t* readobj(FILE* stream);


/*
char* read_stream(FILE* stream)
{
  char* buf = NULL;
  size_t bufsize = 0;

  do {
    buf = realloc(buf, bufsize + STREAMCHUNKSIZE);
    size_t len = fread(buf + bufsize, sizeof(char), STREAMCHUNKSIZE - sizeof(char), stream);
    bufsize += len;
  } while (!feof(stream));
  buf[bufsize] = '\0';

  return buf;
}
*/


char tokenbuf[256];

char* readtoken(FILE* stream)
{
  size_t idx = 0;
  while (!feof(stream)) {
    char c = fgetc(stream);
    if (c == EOF)
      break;
    if (c == ' ' || c == '\t' || c == '\n') {
      if (idx == 0)
	continue;
      else
	break;
    }
    if (c == '(' || c == ')') {
      if (idx > 0) {
	ungetc(c, stream);
      } else {
	tokenbuf[idx++] = c;
      }
      break;
    }
    tokenbuf[idx++] = c;
  }
  tokenbuf[idx] = '\0';
  return strdup(tokenbuf);
}

obj_t* readlist(FILE* stream)
{
  printf("readlist...");
  char* token = readtoken(stream);
  printf(" token: %s\n", token);
  if (strlen(token) == 0)
    error("incomplete list expression (0)");
  if (!strcmp(token, ")"))
    return nil;
  obj_t* nextobj = readobj(stream);
  if (NULL == nextobj)
    error("incomplete list expression (null)");
  return cons(nextobj, readlist(stream));
}

obj_t* readobj(FILE* stream)
{
  obj_t* obj = NULL;
  printf("readobj...");
  char* token = readtoken(stream);
  printf(" token: %s\n", token);
  if (strlen(token) > 0) {
    if (!strcmp(token, "("))
      return readlist(stream);
    return intern(token);
  }
  return obj;
}

/*
unsigned char token_empty(token_t* token)
{
  return NULL == token || 0 == token->used;
}

token_t* token_head(token_t* token)
{
  token_t* head = token;
  while (NULL != head && NULL != head->prev) {
    head = head->prev;
  }

  return head;
}

void free_token(token_t* token)
{
  free(token->value);
  free(token);
}

token_t* trim_token(token_t* token)
{
  if (NULL == token)
    return NULL;

  token_t* returntoken = token;
  if (0 == token->used) {
    if (NULL != token->prev) {
      returntoken = token->prev;
      token->prev->next = token->next;
    }
    if (NULL != token->next) {
      token->next->prev = token->prev;
    }
    free_token(token);
  }

  return returntoken;
}

token_t* new_token(token_t* previous)
{
  if (NULL != previous && token_empty(previous))
    return previous;

  token_t* newtoken = malloc(sizeof(token_t));
  newtoken->value = malloc(sizeof(char) * TOKENCHUNKSIZE);
  newtoken->size = TOKENCHUNKSIZE;
  newtoken->used = 0;
  newtoken->next = NULL;
  newtoken->prev = previous;

  if (NULL != previous)
    previous->next = newtoken;

  return newtoken;
}

token_t* token_append(token_t* token, char append)
{
  if (token->used + 1 > token->size) {
    size_t newsize = token->size + TOKENCHUNKSIZE;
    token->value = realloc(token->value, newsize * sizeof(char));
    token->size = newsize;
  }

  token->value[token->used++] = append;

  return token;
}

void print_token(token_t* token)
{
  if (NULL == token)
    return;
  printf("(token, %lu, %lu, \"", token->size, token->used);
  for (size_t i = 0; i < token->used; i++) {
    printf("%c", token->value[i]);
  }
  printf("\")\n");
  print_token(token->next);
}

token_t* tokenize(char* content)
{
  token_t* token = new_token(NULL);

  size_t contentlen = strlen(content);
  for (size_t i = 0; i < contentlen; i++) {
    //char c = toupper(content[i]);
    char c = content[i];
    if ((c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z')) {
      token = token_append(token, c);
    } else if (c == '(') {
      token = new_token(token);
      token = token_append(token, c);
      token = new_token(token);
    } else if (c == ')') {
      token = new_token(token);
      token = token_append(token, c);
      token = new_token(token);
    } else if (c == ' ' || c == '\t' || c == '\n') {
      token = new_token(token);
    }
  }

  return token_head(trim_token(token));
}
*/

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
  if (strcmp(name, "nil"))
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
  case SYM:
    break;
  case PROC:
    return progn(proccode(proc), multiple_extend(procenv(proc), procargs(proc), vals));
    break;
  default:
    error("unknown type for apply");
    break;
  }
  return nil;
}

obj_t* evlis(obj_t* exprs, obj_t* env)
{
  if (null(exprs))
    return nil;
  return cons(eval(car(exprs), env),
	      evlis(cdr(exprs), env));
}

obj_t* eval(obj_t* expr, obj_t* env)
{
  obj_t* tmp;
  switch (expr->type) {
  case SYM:
    tmp = assoc(expr, env);
    if (null(tmp))
      error("unbound symbol");
    return cdr(tmp);
  case SELFEVAL:
    return expr;
  case CONS:
    return apply(car(expr), evlis(cdr(expr), env), env);
    break;
  case PROC:
    return expr;
    break;
  }
  return nil;
}

void print_obj(obj_t* obj)
{
  switch (obj->type) {
  case SYM:
    printf("SYM(%s)", symname(obj));
    break;
  case SELFEVAL:
    printf("SELFEVAL(%d)", value(obj));
    break;
  case CONS:
    printf("CONS(");
    print_obj(car(obj));
    printf(", ");
    print_obj(cdr(obj));
    printf(")");
    break;
  case PROC:
    printf("PROC(");
    print_obj(procargs(obj));
    printf(", ");
    print_obj(proccode(obj));
    printf(", ");
    print_obj(procenv(obj));
    printf(")");
    break;
  default:
    printf("unknown object of type %d!\n", obj->type);
    break;
  }
}

void init()
{
  nil = new_obj(SYM, 1, 0);
  setcar(nil, nil);

  interned_syms = cons(nil, nil);
  global_env = cons(cons(nil, nil), nil);
}


int main(int argc, char* argv[])
{
  init();

  /*
  char* token = readtoken(stdin);
  printf("read token: %s\n", token);
  token = readtoken(stdin);
  printf("read token: %s\n", token);

  char* expr = read_stream(stdin);
  printf("\nREAD:\n%s\n", expr);
  token_t* tokens = tokenize(expr);
  print_token(tokens);
  */
  obj_t* or = readobj(stdin);
  if (NULL != or)
    print_obj(or);
  printf("\n===\n");
  /*
  char* token = readtoken(stdin);
  printf("TOKEN: %s\n", token);
  token = readtoken(stdin);
  printf("TOKEN: %s\n", token);
  token = readtoken(stdin);
  printf("TOKEN: %s\n\n", token);
  */

  obj_t* o1 = intern("xxx");
  obj_t* o2 = intern("yyy");
  obj_t* o3 = cons(o1, cons(o2, nil));
  print_obj(o3);
  printf("\n\n");
  print_obj(car(o3));
  printf("\n\n");
  print_obj(cdr(o3));
  printf("\n");

  obj_t* n = nil;
  if (null(n)) {
    printf("yes!\n");
  }

  obj_t* o4 = intern("ttt");
  obj_t* o5 = intern("ttt");
  if (eq(o4, o5)) {
    printf("yes2!\n");
  }

  obj_t* evaluated_nil = eval(nil, global_env);
  print_obj(evaluated_nil);
  printf("\n");

  obj_t* evaluated_123 = eval(mkselfeval(123), global_env);
  print_obj(evaluated_123);
  printf("\n");

  obj_t* evaluated_proc = eval(mkproc(cons(mksym("a"), nil),
				      cons(mkselfeval(123), nil),
				      global_env),
			       global_env);
  print_obj(evaluated_proc);
  printf("\n");

  obj_t* evaluated_apply = eval(cons(evaluated_proc,
				     cons(mkselfeval(555), nil)),
				global_env);
  print_obj(evaluated_apply);
  printf("\n");

  return 0;
}

