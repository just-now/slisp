all: clean
	(echo "("; cat lisp-src/lib.lisp lisp-src/test.lisp; echo ")") \
		| sed "s/;;.*//" | python3 main.py
	make clean

clean:
	rm -rf out.xxx

calc:
	(echo "("; cat lisp-src/lib.lisp lisp-src/calc.lisp; echo ")") \
		| sed "s/;;.*//" | python3 main.py
