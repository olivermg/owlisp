CC = clang
CLLFLAGS = -emit-llvm -S -Wall
CFLAGS = -c -Wall -g
LDFLAGS = -Wall -g
LIBS = -lpthread -lm -lrt -lcheck
SWIG = swig
SWIGFLAGS = -includeall
LLVM_AS = llvm-as
LLVM_LINK = llvm-link

.PHONY: all clean test-run



all: runtime.bc



runtime.bc: error.bc types.bc frame.bc
	$(LLVM_LINK) -o $@ $^

%.bc: %.ll
	$(LLVM_AS) $<

%.ll: %.c
	$(CC) $(CLLFLAGS) -o $@ $<



runtime_api_low.lisp: swig.i frame.h types.h error.h
	$(SWIG) $(SWIGFLAGS) -cffi swig.i



libowlisp-rt.so: error.o types.o frame.o
	$(CC) $(LDFLAGS) -shared -Wl,-soname,$@.0 -o $@.0 $^
	ln -sf $@.0 $@

%.o: %.c
	$(CC) $(CFLAGS) -fpic -o $@ $<



test-run: test
	@echo "\n=== TESTS START ==="
	@./test
	@echo "=== TESTS END ===\n"

test: error.o types.o frame.o test.o
	$(CC) $(LDFLAGS) -o $@ $^ $(LIBS)



clean:
	rm -vf *.ll *.o *.bc *.s runtime runtime_api_low.lisp libowlisp-rt.so* test

