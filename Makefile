CL = sbcl

owlispc:
	mkdir -p build
	$(CL) --load "make.lisp"

clean:
	rm -rvf build

