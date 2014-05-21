CC = clang
CFLAGS = -emit-llvm -S

runtime.ll: frame.ll
%.ll: %.c
	$(CC) $(CFLAGS) -o $@ $<

clean:
	rm -vf *.ll runtime
