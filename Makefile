DIRS = src/llvm/cffi src/llvm/crt
CL = sbcl
COMPILE_LOG = build/lisp-compile.log

.PHONY: clean



build/owlisp:
	mkdir -p build
	for D in $(DIRS); do \
		make -C $$D ; \
	done
	@echo "COMPILATION LOG IN $(COMPILE_LOG)"
	$(CL) --load "make.lisp" >$(COMPILE_LOG) 2>&1

clean:
	for D in $(DIRS); do \
		make -C $$D clean ; \
	done
	rm -rvf build

