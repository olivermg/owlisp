SWIG=swig
SWIGFLAGS=-I/usr/local/include -I/usr/include
#SWIGFLAGS=-I/home/oliver/opt/llvm/current/include

.PHONY: clean



llvm_api_low.lisp: swig.i
	$(SWIG) $(SWIGFLAGS) -cffi swig.i

clean:
	rm -vf llvm_api_low.lisp

