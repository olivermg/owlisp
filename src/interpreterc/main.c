#include <stdio.h>
#include <stdlib.h>
//#include <ctype.h>
#include <string.h>
#include <stdarg.h>

#define STREAMCHUNKSIZE 1024
#define TOKENCHUNKSIZE 16
#define TOKENLISTCHUNKSIZE 16

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
} token_t;

typedef struct _tokenlist_t {
  size_t size;
  size_t used;
  token_t* tokens;
} tokenlist_t;


typedef enum _type_t {
  ATOM,
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

token_t* new_token()
{
  token_t* newtoken = malloc(sizeof(token_t));
  newtoken->value = malloc(sizeof(char) * TOKENCHUNKSIZE);
  newtoken->size = TOKENCHUNKSIZE;
  newtoken->used = 0;

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
  printf("\ntoken size: %lu\n", token->size);
  printf("token used: %lu\n", token->used);
  printf("token value: ");
  for (size_t i = 0; i < token->used; i++) {
    printf("%c", token->value[i]);
  }
  printf("\n");
}

tokenlist_t* new_tokenlist()
{
  tokenlist_t* tokenlist = malloc(sizeof(tokenlist_t));
  tokenlist->tokens = malloc(sizeof(token_t) * TOKENLISTCHUNKSIZE);
  tokenlist->size = TOKENLISTCHUNKSIZE;
  tokenlist->used = 0;

  return tokenlist;
}

tokenlist_t* tokenlist_append(tokenlist_t* list, token_t* append)
{
  if (list->size + 1 > list->size) {
    size_t newsize = list->size + TOKENLISTCHUNKSIZE;
    list->tokens = realloc(list->tokens, newsize * sizeof(token_t));
    list->size = newsize;
  }

  list->tokens[list->used++] = *append;

  return list;
}

void print_tokenlist(tokenlist_t* list)
{
  printf("\ntokenlist size: %lu\n", list->size);
  printf("tokenlist used: %lu\n", list->used);
  printf("tokenlist tokens:\n");
  for (size_t i = 0; i < list->used; i++) {
    print_token(&list->tokens[i]);
  }
  printf("\n");
}

tokenlist_t* tokenize(char* content)
{
  tokenlist_t* tokenlist = new_tokenlist();

  size_t contentlen = strlen(content);
  token_t* curtoken = NULL;
  for (size_t i = 0; i < contentlen; i++) {
    //char c = toupper(content[i]);
    char c = content[i];
    if ((c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z')) {
      if (!curtoken) {
	curtoken = new_token();
      }
      curtoken = token_append(curtoken, c);
    } else if (c == '(') {
      if (curtoken) {
	tokenlist_append(tokenlist, curtoken);
      }
      curtoken = new_token();
      curtoken = token_append(curtoken, c);
      if (curtoken) {
	tokenlist_append(tokenlist, curtoken);
      }
      curtoken = NULL;
    } else if (c == ')') {
      if (curtoken) {
	tokenlist_append(tokenlist, curtoken);
      }
      curtoken = new_token();
      curtoken = token_append(curtoken, c);
      if (curtoken) {
	tokenlist_append(tokenlist, curtoken);
      }
      curtoken = NULL;
    } else if (c == ' ' || c == '\t' || c == '\n') {
      if (curtoken) {
	tokenlist_append(tokenlist, curtoken);
      }
      curtoken = NULL;
    }
  }

  return tokenlist;
}

obj_t* interned_syms;
obj_t* global_env;

obj_t* nil;

#define mksym(x) new_obj(ATOM, 1, (x))
#define symname(x) (char*)((x)->objs[0])
#define mkproc(args,code,env) new_obj(PROC, 3, (args), (code), (env))
#define eq(x,y) ((x) == (y))
#define null(x) eq(x, nil)
#define cons(x,y) new_obj(CONS, 2, (x), (y))
#define car(x) ((x)->objs[0])
#define cdr(x) ((x)->objs[1])
#define extend(env,sym,val) cons(cons((sym), (val)), (env))
#define error(x) do { fprintf(stderr, "ERROR: %s\n", x); exit(1); } while (0)

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

obj_t* find_symbol(char* name)
{
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

obj_t* objectify(tokenlist_t* tokens)
{
  return NULL;
}

obj_t* eval(obj_t* expr, obj_t* env)
{
  obj_t* tmp;
  switch (expr->type) {
  case ATOM:
    tmp = assoc(expr, env);
    if (null(tmp)) error("unbound symbol");
    return cdr(tmp);
  case CONS:
    break;
  case PROC:
    break;
  }
  return nil;
}

void print_obj(obj_t* obj)
{
  switch (obj->type) {
  case ATOM:
    printf("ATOM(%s)", (char*)obj->objs[0]);
    break;
  case CONS:
    printf("CONS(");
    print_obj(obj->objs[0]);
    printf(", ");
    print_obj(obj->objs[1]);
    printf(")");
    break;
  default:
    printf("unknown object of type %d!\n", obj->type);
    break;
  }
}

void init()
{
  nil = mksym("nil");

  interned_syms = cons(nil, nil);
  global_env = cons(cons(nil, nil), nil);
}


int main(int argc, char* argv[])
{
  init();

  char* expr = read_stream(stdin);
  printf("\nREAD:\n%s\n", expr);
  tokenlist_t* tokenlist = tokenize(expr);
  print_tokenlist(tokenlist);

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

  return 0;
}

