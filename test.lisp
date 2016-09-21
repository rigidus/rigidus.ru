(print
 (sb-cltl2:macroexpand-all
  '(dotimes (x 3)
    (dotimes (y 3) (print (cons x y))))))

(let ((state nil)
      (x 0)
      (y 0))
  (defun test ()
    (tagbody
       (if (null state)
           (go label-test-x)
           (go label-next))
     label-iteration-x
       (go label-test-y)
     label-iteration-y
       (setq state t)
       (return-from test (print (cons x y)))
     label-next
       (setq y (1+ y))
     label-test-y
       (unless (>= y 3)
         (go label-iteration-y))
       (setq y 0)
     exit-y
       (setq x (1+ x))
     label-test-x
       (unless (>= x 3)
         (go label-iteration-x))))
  (test)
  )

(test)
