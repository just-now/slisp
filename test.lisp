(
 (let b 0)
 (let a 10)

 (while a
   ((let a (- a 1))
    (let b (+ b 2)))))
