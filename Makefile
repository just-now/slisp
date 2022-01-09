all: clean
	cat test.lisp | sed "s/;;.*//" | ./python3.10 main.py

clean:
	rm -rf out.xxx
