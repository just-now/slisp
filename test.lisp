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

 (print "---xy----")
 (defun foo3 (xy)
   (
    ;;(print "xy={}" xy)
    (while xy
      ((print "@{}" (List.data xy))
       (setq xy (List.next xy))))))

 (foo3 x)

 ;; Imports
 ;; (require "test.lisp")

 (print "--------")
 (if (and (> a 0) (== b 0))
     (setq c (+ "1\"{}-{}23" "-456"))
     (setq d 44))

 (while (> a 0)
   ((setq a (- a 1))
    (setq b (+ b 2))))

 (print "b={} \t c={} \n \" const={}\t d={}"
	b c 3 (str[] 3 c))

 (print "--------")
 (defun foo5 (x y z &rest p)
   ((print "{}-{}-{}" x y z)
    (foo3 p)))

 (foo5 1 2 3 4 5 6 7 8 9 0)

 (print "--list!-")
 (defun list (&rest l)
   l)
 (foo3 (list 1 2 3 4 5))
)
