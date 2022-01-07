(
 (setq b 0)
 (setq a 10)

 (defun foo2 (a b c)
   (+ a (+ b c)))

 (defun Con-st ()
   10)

 (print "foo2={}\n" (foo2 1 (+ b a) (foo2 1 2 3)))
 (print "Con-st={}\n" (Con-st))

 (print "--struct-\n")
 (defstruct List
     data
     next)

 (print "--prlist-\n")
 (setq x (List (foo2 1 2 3) (List 1 nil)))
 (defun prlist (xy)
   (
    ;;(print "xy={}" xy)
    (while xy
      ((print "@{} " (List.data xy))
       (setq xy (List.next xy))))
    (print "\n")))

 (prlist x)

 (print "--------\n")
 (if (and (> a 0) (== b 0))
     (setq c (+ "1\"{}-{}23" "-456"))
     (setq d 44))

 (while (> a 0)
   ((setq a (- a 1))
    (setq b (+ b 2))))

 (print "b={} \t c={} \n \" const={}\t d={}\n"
        b c 3 (str[] 3 c))

 (print "--------\n")
 (defun foo5 (x y z &rest p)
   ((print "{}-{}-{}\n" x y z)
    (prlist p)))

 (foo5 1 2 3 4 5 6 7 8 9 0)

 (print "--list!-\n")
 (defun list (&rest l)
   l)
 (prlist (list 1 2 3 4 5))

 (print "--not----\n")
 (defun not (x)
   (if x false true))

 (print "f:{}, t:{}\n" (not false) (not true))

 (print "--map--\n")
 (defun plus10 (a) (+ a 10))
 (defun map (pred l)
   (if l
       (List (funcall pred (List.data l))
             (map pred (List.next l)))
        nil))
 (prlist (map #plus10 (list 1 2 3 4 5)))

 (print "--filter--\n")
 (defun filter (pred l)
   (if l
       (if (funcall pred (List.data l))
           (List (List.data l) (filter pred (List.next l)))
           (filter pred (List.next l)))
        nil))
 (defun gt3 (a) (> a 3))
 (prlist (filter #gt3 (list 1 2 3 4 5)))

 (print "--lambda-\n")
 (setq lam1 (lambda (x) (+ 5 x)))
 (print "@@{}\n" (funcall lam1 10))
 (print "@@{}\n"
        (funcall (lambda (x y) (+ y (+ 10 x))) 10 20))

 (funcall (lambda (&rest p) (prlist p)) 1 2 3 4 5)
 (prlist (map (lambda (x) (+ x 10))
                (list 1 2 3 4 5)))

 (print "--append(ffi)--\n")
 (prlist (append
          (list 1 2 3 4 5)
          (list 10 20 30)))

 (print "--curry--\n")
 (defun curry (fn &rest args)
   (lambda (&rest remaining-args)
     (apply fn (append args remaining-args))))

 (defun addx (a b)
   (+ a b))

 (print "@@{}\n" (funcall (curry #addx 2) (+ 2 3)))

 (print "--compose--\n")
 (defun compose (&rest foos)
   (lambda (x)
     ((while foos
        ((setq foo  (List.data foos))
         (setq x    (funcall foo x))
         (setq foos (List.next foos))))
      x)))

 (print "@@{}\n" (funcall (compose
                         (lambda (x) (* x 10))
                         ;;(lambda (x) (+ x 10))
                         (curry #addx 10)
                         (lambda (x) (/ x 10)))
                        13))

 (print "----tree---\n")
 (defstruct Tree
   data
   left
   right)
 (setq tree (Tree 1
		  (Tree 2
			(Tree 22 nil nil)
			(Tree 23 nil nil))
		  (Tree 3
			(Tree 32 nil nil)
			(Tree 33 nil nil))))
 (defun prtree (node)
   (if (not node)
       nil
       ((print "@@{} " (Tree.data node))
	(prtree (Tree.left  node))
	(prtree (Tree.right node)))))

 (prtree tree)
 (print "\n")
)
