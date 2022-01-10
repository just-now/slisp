all: clean
	(echo "("; cat lib.lisp test.lisp; echo ")") \
		| sed "s/;;.*//" | ./python3.10 main.py
clean:
	rm -rf out.xxx

calc:
	(echo "("; cat lib.lisp calc.lisp; echo ")") \
		| sed "s/;;.*//" | ./python3.10 main.py
