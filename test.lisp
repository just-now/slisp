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

 (print "--prlist-")
 (defun prlist (xy)
   (
    ;;(print "xy={}" xy)
    (while xy
      ((print "@{}" (List.data xy))
       (setq xy (List.next xy))))))

 (prlist x)

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
    (prlist p)))

 (foo5 1 2 3 4 5 6 7 8 9 0)

 (print "--list!-")
 (defun list (&rest l)
   l)
 (prlist (list 1 2 3 4 5))

 (print "--not----")
 (defun not (x)
   (if x false true))

 (print "f:{}, t:{}" (not false) (not true))

 (print "--apply--")
 (defun plus10 (a) (+ a 10))

 (defun mapr (foox l)
   (while l
     ((print "##{}" (funcall foox (List.data l)))
      (setq l (List.next l)))))

 (defun map (foox l)
   (if l
       (List (funcall foox (List.data l))
	     (map foox (List.next l)))
	nil))

 (mapr #plus10 (list 1 2 3 4 5))
 (prlist (map #plus10 (list 1 2 3 4 5)))

)
