#include <stdio.h>
#include <stdlib.h>
//#include <ctype.h>
#include <string.h>

#define STREAMCHUNKSIZE 1024
#define TOKENCHUNKSIZE 16
#define TOKENLISTCHUNKSIZE 16

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

int main(int argc, char* argv[])
{
	char* expr = read_stream(stdin);
	printf("\nREAD:\n%s\n", expr);
	tokenlist_t* tokenlist = tokenize(expr);
	print_tokenlist(tokenlist);
	
	return 0;
}

