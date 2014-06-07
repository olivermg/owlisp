CC = clang
CLLFLAGS = -emit-llvm -S -Wall
CFLAGS = -c -Wall -g
LDFLAGS = -Wall -g
LIBS = -lpthread -lm -lrt -lcheck
SWIG = swig
SWIGFLAGS = -includeall



all: runtime.ll runtime.lisp



runtime.ll: error.ll types.ll frame.ll
%.ll: %.c
	$(CC) $(CLLFLAGS) -o $@ $<



runtime.lisp: swig.i runtime.h frame.h types.h error.h
	$(SWIG) $(SWIGFLAGS) -cffi swig.i



test-run: test
	@echo "\n=== TESTS START ==="
	@./test
	@echo "=== TESTS END ===\n"

test: error.o types.o frame.o test.o
	$(CC) $(LDFLAGS) -o $@ $^ $(LIBS)

%.o: %.c
	$(CC) $(CFLAGS) -o $@ $<



clean:
	rm -vf *.ll *.o *.bc *.s runtime runtime.lisp test
