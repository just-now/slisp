(
 (setq b 0)
 (setq a 10)

 (defun foo (a b c)
   (+ a (+ b c)))

 (defun fooA ()
   10)

 (print "foo={}" (foo 1 (+ b a) (foo 1 2 3)))
 (print "fooA={}" (fooA))

 (if a
     (setq c (+ "1\"{}-{}23" "-456"))
     (setq d 44))

 (while a
   ((setq a (- a 1))
    (setq b (+ b 2))))

 (print "a={} b={} \n \t \" c={}" 1 c 3))
