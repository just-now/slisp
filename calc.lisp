(defstruct Const  c)
(defstruct Var    v)
(defstruct Op     op left right)
(defstruct Fun    fun seq)
(defstruct Assign left right)
(defstruct While  cond body)
(defstruct If     cond if_ else_)
(defstruct Seq    seq)

(defstruct LList  seq)
(defstruct LAtom  val)
(defstruct Ret    parsed rest)

(defun isatom(c)
  (not (or (str-isspace c)
	   (or (== c "(")
	       (== c ")")))))

(defun sexp(s)
  (if (not s)
      (Ret nil "")

      (if (str-isspace (nth 0 s))
	  (sexp (dropwhile #str-isspace s))

	  (if (== (nth 0 s) ")")
	      (Ret nil (from 1 s))

  	      (if (== (nth 0 s) "(")
		  ((setq seq nil)
		   (setq re  (from 1 s))
		   (setq ret (sexp re))
		   (setq x   (Ret.parsed ret))
		   (setq re  (Ret.rest ret))
		   (while x
		     ((setq seq (append seq (List x nil)))
		      (setq ret (sexp re))
		      (setq x   (Ret.parsed ret))
		      (setq re  (Ret.rest ret))))
		   ;;(print "@@{}\n" (Ret (LList seq) re))
		   (Ret (LList seq) re))

	          (Ret (LAtom (str-join "" (takewhile #isatom s)))
		       (dropwhile #isatom s)))))))


(setq file (file-open "calc.test.lisp" "r"))
(setq cont (file-read file))
(print "{}\n" cont)
(file-close file)

(setq la_tree (Ret.parsed (sexp (to-list cont))))
;; (print "@@ {}\n" la_tree)

(defun is (e type)
  (== (type-of e) type))

(defun ast(e)
  (
   ;; (print "\n\n======>e={}\n\n" e)
   (setq ret nil)

   ;; case List([List(l1), *l2] as seq):
   (if (and (not ret)
	    (and (is e "LList")
		 (and (LList.seq e)
		      (is (nth 0 (LList.seq e)) "LList"))))
       (setq ret (Seq (map #ast (LList.seq e)))))

   ;; case List([Atom("setq"), Atom(v), exp]):
   ;; case List([Atom("while"), cond, exp]):
   ;; case List([Atom("if"), cond, texp, fexp]):
   ;; case List([Atom(fun), *params]):
   (if (and (not ret)
	    (and (is e "LList")
		 (and (LList.seq e)
		      (is (nth 0 (LList.seq e)) "LAtom"))))

       (
	(if (and (not ret)
		 (== "setq" (LAtom.val (nth 0 (LList.seq e)))))
	    (
	     ;; (print "\n\n ##> LEFT  {}\n\n" (LAtom.val (nth 1 (LList.seq e))))
	     ;; (print "\n\n ##> RIGHT {}\n\n" (ast (nth 2 (LList.seq e))))
	     (setq ret (Assign (LAtom.val (nth 1 (LList.seq e)))
			       (ast (nth 2 (LList.seq e)))))
	     ))

	(if (and (not ret)
		 (== "while" (LAtom.val (nth 0 (LList.seq e)))))
	    (
	     (setq ret (While (ast (nth 1 (LList.seq e)))
			      (ast (nth 2 (LList.seq e)))))
	     ))

	(if (and (not ret)
		 (== "if" (LAtom.val (nth 0 (LList.seq e)))))
	    (
	     (setq ret (If (ast (nth 1 (LList.seq e)))
			   (ast (nth 2 (LList.seq e)))
			   (ast (nth 3 (LList.seq e)))))
	     ))

	(if (and (not ret)
		 (str-isalpha (nth 0 (to-list (Var.v (ast (nth 0 (LList.seq e))))))))
	    (
	     ;; (print "\n\n ##> FUN    {}\n\n" (ast (nth 0 (LList.seq e))))
	     (setq ret (Fun (Var.v (ast (nth 0 (LList.seq e))))
			    (map #ast (from 1 (LList.seq e)))))
	     ))

	(if (not ret)
	    (
	     ;; (print "\n\n ##> OP    {}\n\n" (ast (nth 0 (LList.seq e))))
	     ;; (print "\n\n ##> LEFT  {}\n\n" (ast (nth 1 (LList.seq e))))
	     ;; (print "\n\n ##> RIGHT {}\n\n" (ast (nth 2 (LList.seq e))))
	     (setq ret (Op (ast (nth 0 (LList.seq e)))
			   (ast (nth 1 (LList.seq e)))
			   (ast (nth 2 (LList.seq e)))))
	     ))

	))

   ;; case Atom(v):
   (if (and (not ret)
	    (is e "LAtom"))
       (
	(setq ret (if (str-isnumeric (LAtom.val e))
		      (Const (LAtom.val e))
		      (Var   (LAtom.val e))))
	))


   ret))



(setq e_tree (ast la_tree))
;; (print "{}\n" e_tree)
(setq heap nil)

;; (print "-------------------\n")

(defun intrp (e)
  (
   ;; (print "\n\ntop={}\n\n" e)
   (setq ret nil)

   (if (and (not ret) (is e "Seq"))
       (
	(map #intrp (Seq.seq e))
	(setq ret true)
	))

   (if (and (not ret) (is e "Assign"))
       (
	(setq heap (heap-put heap (KV (Assign.left e)
				      (intrp (Assign.right e)))))
	(setq ret true)
	))

   (if (and (not ret) (is e "Op"))
       (
	;; (print "## op={}\n"    (Var.v (Op.op e)))
	;; (print "## left={}\n"  (intrp (Op.left e)))
	;; (print "## right={}\n" (intrp (Op.right e)))
	(setq op (Var.v (Op.op e)))

	(if (== op "print")
	    ((setq ret true)
	     (print "{}, {}\n" (intrp (Op.left e)) (intrp (Op.right e)))))

	(if (== op "+")		(setq ret (+  (intrp (Op.left e)) (intrp (Op.right e)))))
	(if (== op "-")		(setq ret (-  (intrp (Op.left e)) (intrp (Op.right e)))))
	(if (== op ">")		(setq ret (>  (intrp (Op.left e)) (intrp (Op.right e)))))
	(if (== op "==")	(setq ret (== (intrp (Op.left e)) (intrp (Op.right e)))))

	))

   (if (and (not ret) (is e "Fun"))
       (
	;; (print "## fun={}\n"     (Fun.fun e))
	;; (print "## rest={}\n"    (Fun.seq e))
	(setq ret true)
	(setq fun (Fun.fun e))
	(setq seq (Fun.seq e))

	(if (== fun "print")		(list-print (map #intrp seq)))

	))

   (if (and (not ret) (is e "While"))
       (
	(setq condition (While.cond e))
	(setq body      (While.body e))

	(while (intrp condition)
	  (intrp body))

	(setq ret true)
	))

   (if (and (not ret) (is e "If"))
       (
	(setq condition (If.cond  e))
	(setq if_       (If.if_   e))
	(setq else_     (If.else_ e))

	(if (intrp condition)
	    (intrp if_)
	    (intrp else_))

	(setq ret true)
	))

   (if (and (not ret) (is e "Const"))
       (
	(setq ret (int (Const.c e)))
	))

   (if (and (not ret) (is e "Var"))
       (
	(setq ret (int (KV.value (heap-get heap (KV (Var.v e) nil)))))
	))

   ret
   ))


(intrp e_tree)
