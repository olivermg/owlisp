LEX=lex
YACC=yacc
CC=clang

parser: y.tab.c lex.yy.c
	$(CC) -Wall -o $@ $^

lex.yy.c: lexer.l
	$(LEX) $^

y.tab.c: yacc.y
	$(YACC) -d $^

clean:
	rm -vf parser y.tab.c lex.yy.c y.tab.h

.PHONY: clean
