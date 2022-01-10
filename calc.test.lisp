(
 (setq b 0)
 (setq a 10)

 (while (> a 0)
   ((setq a (- a 1))
    (setq b (+ b 2))))

 (if (== b 20)
     ((print a b)
      (print a)
      (print b)
      (print 10)
      )
     (print 0 0))
)
