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

(defun list-lookup (pred val ls)
  ((setq ret nil)
   (while (and (not ret) ls)
     (
      (if (funcall pred (List.data ls) val)
	  (setq ret (List.data ls))
	  (setq ls (List.next ls)))
      ))
   ret))

(defun list (&rest l) l)

(defstruct KV
  key
  value)

(defun keq (left right) (== (KV.key left) (KV.key right)))

(defun heap-get (heap kv)
  (list-lookup #keq kv heap))

(defun heap-put (heap kv)
  (if (heap-get heap kv)
      ((KV.value.set (heap-get heap kv)
		     (KV.value kv))
       heap)
      (append heap (list kv))))

(defun list-head (l) l)

(defun list-tail (l)
  ((while (List.next l)
     (setq l (List.next l)))
   l))

(defun list-len (l)
  ((setq len 0)
   (while l
     ((setq l (List.next l))
      (setq len (+ 1 len))))
   len))

(defun list-append (l1 l2)
  ((setq ret nil)

   (if (and (not ret) (and (not l1)
			   (not l2)))
       (setq ret nil))

   (if (and (not ret) (not l1))
       (setq ret l2))

   (if (and (not ret) (not l2))
       (setq ret l1))

   (if (not ret)
       ((List.next.set (list-tail l1) (list-head l2))
	(setq ret l1)))

   ret))

(defun append (l1 l2)
  (list-append l1 l2))

(defun len (l)
  (list-len l))

;; ==================== std library ====================

(defun not (x)
  (if x false true))

(defun inc (i)
  (+ i 1))

(defun dec (i)
  (- i 1))

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
