SWIG=swig
SWIGFLAGS=-I/usr/include
#SWIGFLAGS=-I/usr/local/include
#SWIGFLAGS=-I/home/oliver/opt/llvm/current/include

llvm_api_low.lisp: swig.i
	$(SWIG) $(SWIGFLAGS) -cffi swig.i

clean:
	rm -vf llvm_api_low.lisp

