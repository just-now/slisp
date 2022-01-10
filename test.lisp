(setq b 0)
(setq a 10)

(defun foo2 (a b c)
  (+ a (+ b c)))

(defun const () 10)

(print "foo2={}\n" (foo2 1 (+ b a) (foo2 1 2 3)))
(print "const={}\n" (const))

(print "--prlist-\n")
(setq g_x (List (foo2 1 2 3) (List 1 nil)))
(list-print g_x)

(print "--------\n")
(if (and (> a 0) (== b 0))
    (setq c (+ "1\"{}-{}23" "-456"))
  (setq d 44))

(while (> a 0)
  ((setq a (- a 1))
   (setq b (+ b 2))))

(print "b={} \t c={} \n \" const={}\n" b c 3)

(print "--------\n")
(defun foo5 (x y z &rest p)
  ((print "{}-{}-{}\n" x y z)
   (list-print p)))

(foo5 1 2 3 4 5 6 7 8 9 0)

(print "--list!-\n")
(list-print (list 1 2 3 4 5))

(print "--not----\n")
(print "f:{}, t:{}\n" (not false) (not true))

(print "--map--\n")
(defun plus10 (a) (+ a 10))
(list-print (map #plus10 (list 1 2 3 4 5)))

(print "--filter--\n")
(defun gt3 (a) (> a 3))
(list-print (filter #gt3 (list 1 2 3 4 5)))

(print "--lambda-\n")
(setq lam1 (lambda (x) (+ 5 x)))
(print "@@{}\n" (funcall lam1 10))
(print "@@{}\n"
       (funcall (lambda (x y) (+ y (+ 10 x))) 10 20))

(funcall (lambda (&rest p) (list-print p)) 1 2 3 4 5)
(list-print (map (lambda (x) (+ x 10))
                 (list 1 2 3 4 5)))

(print "--append(ffi)--\n")
(list-print (append
             (list 1 2 3 4 5)
             (list 10 20 30)))

(print "--curry--\n")
(defun addx (a b)
  (+ a b))

(print "@@{}\n" (funcall (curry #addx 2) (+ 2 3)))

(print "--compose--\n")
(print "@@{}\n" (funcall (compose (lambda (x) (* x 10))
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

(print "---- io/r ---\n")
(setq file (file-open "test.lisp" "r"))
(setq cont (file-read file 64))
(print "{}\n" cont)
(file-close file)

(print "---- io/w ---\n")
(setq file (file-open "out.xxx" "w"))
(print "err?={}\n" (err? file))
(file-write file cont)
(file-write file (format "\n~Q.E.D~{}\n" 7012022))
(file-close file)

(print "---- io-fail ---\n")
(setq file (file-open "xxx" "r"))
(print "err?={}\n" (err? file))
(print "result=-{}\n" file)

(print "---- str/list ---\n")
(list-print (filter (lambda (x) (> x 5))
		    (map (compose #int #float)
			 (to-list "1234567890"))))

(print "@{}\n" (str-join "" (to-list "1234567890")))

(list-print (str-split "one1 two2 three3"))

(print "---- int/list ---\n")
(print "@{}\n" (str-join "" (map #str (list 1 2.2 3 4 5 6))))

(print "---- drop/take ---\n")
(setq str1 "     (a 1 2)")
;;(setq str1 "          ")
(print "@'{}'\n" (nth 6 (to-list str1)))
(print "dw='{}'\n" (str-join "" (dropwhile #str-isspace
					   (to-list str1))))

(print "s='{}'\n" str1)
(print "tw='{}'\n" (str-join "" (takewhile #str-isspace
					   (to-list str1))))

(print "from='{}'\n" (str-join "" (from 1
					(to-list "-@-1@2-"))))

(print "---- io-new/r ---\n")
(setq file (file-open "calc.test.lisp" "r"))
(setq cont (file-read file))
(print "{}\n" cont)
(file-close file)


(print "---- if-skip ---\n")
(if false
    (print "1\n"))

(print "---- type ---\n")
(print "@ {}\n" (type-of (List 1 nil)))

(print "---- nth() ---\n")
(print "@ {}\n" (nth 3 (to-list "123")))

(print "---- lazy and/or () ---\n")
(defun foo (nr val)
  ((print "--{}\n" nr)
   val))
(print "b:{}\n" (foo 2 true))
(print "---\n")
(and (foo 1 false)
     (foo 2 true))
(print "---\n")
(or (foo 1 true)
    (foo 2 true))

(defun streqq (left right) (== left right))

(print "{}\n" (list-lookup #streqq "1" (to-list "123")))

(print "{}\n" (list-lookup #keq (KV "2" nil)
			   (list (KV "2" 2)
				 (KV "1" 1)
				 (KV "3" 3))))

(print "===> {}\n" (List.data.set (to-list "123") "@@@"))


(setq rep-list (list (KV "2" 2)
		     (KV "1" 1)
		     (KV "3" 3)))

(KV.value.set (list-lookup #keq
			   (KV "2" nil)
			   rep-list)
	      "###")

(print "===> {}\n" rep-list)

(setq heap-test nil)
(setq heap-test (heap-put heap-test (KV "a" 1)))
(setq heap-test (heap-put heap-test (KV "b" 2)))
(setq heap-test (heap-put heap-test (KV "a" 3)))

(print "heap-test={}\n" heap-test)
