all: binary

submission:
	rm -rf icfpc-2015.tar.gz
	tar -czf icfpc-2015.tar.gz game buildapp-1.5.5 build.lisp Makefile

binary:
	wget https://beta.quicklisp.org/quicklisp.lisp -O /tmp/quicklisp.lisp
	sbcl --no-userinit --load ./build.lisp 
