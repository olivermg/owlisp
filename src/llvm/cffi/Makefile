SWIG=swig

SWIGFLAGS=
ifdef LLVM_INCLUDE_PATH
	SWIGFLAGS+=-I$(LLVM_INCLUDE_PATH)
endif
SWIGFLAGS+=-I/usr/local/include -I/usr/include



.PHONY: clean



llvm_api_low.lisp: swig.i
	$(SWIG) $(SWIGFLAGS) -cffi swig.i

clean:
	rm -vf llvm_api_low.lisp

