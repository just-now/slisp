(defstruct Const  c)
(defstruct Var    v)
(defstruct Op     op left right)
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
  ((print "\n\n======>e={}\n\n" e)
   (if (and (and (is e "LList") (LList.seq e))
	    (is (nth 0 (LList.seq e)) "LList"))
      ;; if
      (Seq (map #ast (LList.seq e)))
      ;; else
      (if (and (and (is e "LList") (LList.seq e))
 	       (is (nth 0 (LList.seq e)) "LAtom"))
	  ;; if
	  (if (== "setq" (LAtom.val (nth 0 (LList.seq e))))
	      ;; if
	      ((print "\n\n ##### {}\n\n" (LAtom.val (nth 1 (LList.seq e))))
	       (print "\n\n @@@@@ {}\n\n" (ast (nth 2 (LList.seq e))))
	       e)
	      ;; else
	      (if (== "while" (LAtom.val (nth 0 (LList.seq e))))
		  ;; if
		  e
		  ;; else
		  (if (== "if" (LAtom.val (nth 0 (LList.seq e))))
		      ;; if
		      e
  		      ;; else
		      (if (and (LAtom.val (nth 0 (LList.seq e)))
			       (nth 1 (LList.seq e)))
			  ;; if
			  e
  			  ;; else
			  (if (str-isalpha (nth 0 (LAtom.val e)))
			      ;; if
			      e
  			      ;; else
			      e)))))

	  ;; else
	  e))))

(print "==>{}\n" (ast la_tree))

;; (defun ast(e)
;;   (if (and (and (is e "LList") (LList.seq e))
;; 	   (is (nth 0 (LList.seq e)) "LList"))

;;       ;; assert(LList.seq == List)
;;       ((print "LList\n")
;;        (Seq (map #ast (LList.seq e))))

;;       (if (and (and (is e "LList") (LList.seq e))
;; 	       (is (nth 0 (LList.seq e)) "LAtom"))

;; 	  ;; if
;; 	  ((setq ret nil)
;; 	   (if (== "setq" (LAtom.val (nth 0 (LList.seq e))))
;; 	       ((print "setq\n")
		;; (setq ret (Assign (LAtom.val (nth 1 (LList.seq e)))
		;; 		  (ast (nth 2 (LList.seq e)))))))

;; 	   (if (== "while" (LAtom.val (nth 0 (LList.seq e))))
;; 	       ((print "while\n")
;; 		setq ret (While (ast (nth 1 (LList.seq e)))
;; 				(ast (nth 2 (LList.seq e))))))

;; 	   (if (== "if" (LAtom.val (nth 0 (LList.seq e))))
;; 	       ((print "if\n")
;; 		setq ret (If (ast (nth 1 (LList.seq e)))
;; 			     (ast (nth 2 (LList.seq e)))
;; 			     (ast (nth 3 (LList.seq e))))))

;; 	   ret)

;; 	   ;; else
;; 	   (if (is e "LAtom")
;; 	       ;; if
;; 	       ((print "atom\n")
;; 		(if (str-isalpha (nth 0 (LAtom.val e)))
;; 		    (Var (LAtom.val e))
;; 		    (Const (LAtom.val e))))
;; 	        ;; else
;; 	        ((print "setq\n")
;; 	         e))


;; (print "@{}\n" (ast la_tree))
