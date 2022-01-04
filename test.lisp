(
 (setq b 0)
 (setq a 10)

 (defun foo2 (a b c)
   (+ a (+ b c)))

 (defun Con-st ()
   10)

 (print "foo2={}" (foo2 1 (+ b a) (foo2 1 2 3)))
 (print "Con-st={}" (Con-st))

 (print "--struct-")
 (defstruct List
     data
     next)

 (print "--prlist-")
 (setq x (List (foo2 1 2 3) (List 1 nil)))
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

 (print "--map--")
 (defun plus10 (a) (+ a 10))
 (defun map (foox l)
   (if l
       (List (funcall foox (List.data l))
	     (map foox (List.next l)))
	nil))

 (prlist (map #plus10 (list 1 2 3 4 5)))

 (print "--lambda-")
 (setq lam1 (lambda (x) (+ 10 x)))
 (print "@@{}" (funcall lam1 10))
 (print "@@{}"
	(funcall (lambda (x y) (+ y (+ 10 x))) 10 20))

 (funcall (lambda (&rest p) (prlist p)) 1 2 3 4 5)
 (prlist (map (lambda (x) (+ x 10))
		(list 1 2 3 4 5)))

 (print "--append(ffi)--")
 (prlist (append
	  (list 1 2 3 4 5)
	  (list 10 20 30)))

 (print "--curry--")
 (defun curry (fn &rest args)
   (lambda (&rest remaining-args)
     (apply fn (append args remaining-args))))

 (defun addx (a b)
   (+ a b))

 ;; (print "@@{}" (curry #addx 2))
 (print "@@{}" (funcall (curry #addx 2) (+ 2 3)))
)
