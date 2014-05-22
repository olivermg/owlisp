CC = clang
CLLFLAGS = -emit-llvm -S -Wall
CFLAGS = -c -Wall -g
LDFLAGS = -Wall -g
LIBS = -lpthread -lm -lrt -lcheck



runtime.ll: error.ll frame.ll
%.ll: %.c
	$(CC) $(CLLFLAGS) -o $@ $<



test-run: test
	@echo "\n=== TESTS START ==="
	@./test
	@echo "=== TESTS END ===\n"

test: error.o frame.o test.o
	$(CC) $(LDFLAGS) -o $@ $^ $(LIBS)

%.o: %.c
	$(CC) $(CFLAGS) -o $@ $<



clean:
	rm -vf *.ll *.o runtime test
