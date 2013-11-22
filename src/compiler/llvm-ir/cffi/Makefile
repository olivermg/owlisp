SWIG=swig2.0
SWIGFLAGS=-I/usr/local/include

cffi: swig.i
	$(SWIG) $(SWIGFLAGS) -cffi swig.i

clean:
	rm -vf llvmcffi.lisp

