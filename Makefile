DIRS = src/llvm/cffi src/llvm/crt
CL = sbcl
COMPILE_LOG = build/lisp-compile.log
PREFIX = installed

.PHONY: clean install uninstall



build/owlisp-frontend:
	mkdir -p build
	for D in $(DIRS); do \
		make -C $$D ; \
	done
	@echo "COMPILATION LOG IN $(COMPILE_LOG)"
	$(CL) --load "make.lisp" >$(COMPILE_LOG) 2>&1

install: build/owlisp-frontend
	mkdir -p $(PREFIX)
	mkdir -p $(PREFIX)/runtime
	install build/owlisp-frontend $(PREFIX)
	install src/llvm/crt/*.ll $(PREFIX)/runtime
	install toolchain/* $(PREFIX)

uninstall:
	rm -rvf $(PREFIX)

clean:
	for D in $(DIRS); do \
		make -C $$D clean ; \
	done
	rm -rvf build

