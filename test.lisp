(
 (setq b 0)
 (setq a 10)

 (defun foo2 (a b c)
   (+ a (+ b c)))

 (defun Con-st ()
   10)

 (print "foo2={}" (foo2 1 (+ b a) (foo2 1 2 3)))
 (print "Con-st={}" (Con-st))

 ;; Structs
 (defstruct List
     data
     next)

 (print "--------")
 (setq x (List (foo2 1 2 3) (List 1 nil)))
 (print "x={}\ndata={}\nnext={}"
	x
	(List.data x)
	(List.next x))

 (print "--------")
 (while x
   ((print "@{}" (List.data x))
    (setq x (List.next x))))

 ;; Imports
 ;; (require "test.lisp")

 (print "--------")
 (if a
     (setq c (+ "1\"{}-{}23" "-456"))
     (setq d 44))

 (while a
   ((setq a (- a 1))
    (setq b (+ b 2))))

 (print "a={} \t b={} \n \" c={}" 1 c 3))
