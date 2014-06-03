SWIG=swig
#SWIGFLAGS=-I/usr/local/include
SWIGFLAGS=-I/home/oliver/opt/llvm/current/include

cffi: swig.i
	$(SWIG) $(SWIGFLAGS) -cffi swig.i

clean:
	rm -vf llvmcffi.lisp

