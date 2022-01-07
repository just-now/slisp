all:
	rm -rf out.xxx
	cat test.lisp | sed "s/;;.*//" | python3.10 main.py
