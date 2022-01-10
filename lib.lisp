;; ==================== List library ====================
(defstruct List
  data
  next)

(defun list-print (xy)
  (
   (print "[ ")
   (while xy
     ((print "{} " (List.data xy))
      (setq xy (List.next xy))))
   (print "]\n")))

;; ==================== std library ====================
(defun not (x)
  (if x false true))

(defun map (pred l)
  (if l
      (List (funcall pred (List.data l))
            (map pred (List.next l)))
    nil))

(defun filter (pred l)
  (if l
      (if (funcall pred (List.data l))
          (List (List.data l) (filter pred (List.next l)))
        (filter pred (List.next l)))
    nil))

(defun curry (fn &rest args)
  (lambda (&rest remaining-args)
    (apply fn (append args remaining-args))))

(defun compose (&rest foos)
  (lambda (x)
    ((while foos
       ((setq foo  (List.data foos))
        (setq x    (funcall foo x))
        (setq foos (List.next foos))))
     x)))

(defun dropwhile (pred ls)
  (if ls
      (if (funcall pred (List.data ls))
	  (dropwhile pred (List.next ls))
	  ls)
      nil))

(defun takewhile (pred ls)
  (if ls
      (if (funcall pred (List.data ls))
	  (List (List.data ls) (takewhile pred (List.next ls)))
   	  nil)
      nil))

(defun nth (i ls)
  (if ls
      (if (== i 0)
	  (List.data ls)
	  (nth (- i 1) (List.next ls)))
      nil))

(defun from (i ls)
  (if (not ls)
      nil
      (if (== i 0)
	  ls
	  (from (- i 1) (List.next ls)))))
