

;; myeval
(defun myeval (exp env block-env go-env catch-env errcont cont)
  ;;(print exp)
  (cond ((numberp exp)                    (apply-continuation cont exp))
        ((member exp '(+ * car cdr cons null print or and))
         (apply-continuation cont exp))
        ((equal 't exp)                   (apply-continuation cont 't))
        ((equal 'nil exp)                 (apply-continuation cont 'nil))
        ((symbolp exp)                    (lookup exp env errcont cont))
        ((equal (car exp) 'quote)         (apply-continuation cont (cadr exp)))
        ((equal (car exp) 'if)            (myeval (cadr exp) env block-env go-env catch-env errcont
                                                  (lambda (x)
                                                    (if x
                                                        (myeval (caddr exp)  env block-env go-env catch-env errcont cont)
                                                        (myeval (cadddr exp) env block-env go-env catch-env errcont cont)))))
        ((equal (car exp) 'cond)          (evcond (cdr exp) env block-env go-env catch-env errcont cont)) ; ?
        ((equal (car exp) 'let)           (evlet (mapcar #'car (cadr exp))
                                                 (mapcar #'cadr (cadr exp))
                                                 nil
                                                 (cddr exp)
                                                 env
                                                 block-env
                                                 go-env
                                                 catch-env
                                                 errcont
                                                 cont))
        ((equal (car exp) 'progn)         (evprogn (cdr exp) env block-env go-env catch-env errcont cont))
        ((equal (car exp) 'let*)          (evletstar (cadr exp)
                                                     (cddr exp)
                                                     env
                                                     block-env
                                                     go-env
                                                     catch-env
                                                     errcont cont))
        ((equal (car exp) 'defun)         (progn
                                            (push (cons (cadr exp)
                                                        (make-closure :body (cadddr exp)
                                                                      :block-env block-env
                                                                      :env env
                                                                      :go-env go-env
                                                                      :args (caddr exp)))
                                                  *glob-env*)
                                            (apply-continuation cont (cadr exp))))
        ((equal (car exp) 'setq)          (myeval (caddr exp) env block-env go-env catch-env errcont
                                                  (lambda (val)
                                                    (let ((it (lookup (cadr exp) env errcont cont)))
                                                      (if (null it)
                                                          (push (cons (cadr exp) val)
                                                                *glob-env*)
                                                          (rplacd it val))
                                                      (apply-continuation cont val)))))
        ((equal (car exp) 'lambda)        (apply-continuation cont (make-closure :body (caddr exp)
                                                                                 :block-env block-env
                                                                                 :env env
                                                                                 :go-env go-env
                                                                                 :args (cadr exp))))
        ((equal (car exp) 'block)         (myeval (caddr exp)
                                                  env
                                                  (acons (cadr exp)
                                                         cont
                                                         block-env)
                                                  go-env catch-env errcont cont))
        ((equal (car exp) 'return-from)   (if (not (symbolp (cadr exp)))
                                              (apply-continuation errcont (format nil "return-from: first argument not a symbol"))
                                              (myeval (caddr exp) env block-env go-env catch-env errcont
                                                      (lambda (x)
                                                        (assoc-2 (cadr exp) block-env
                                                                 (lambda (y) (apply-continuation y x))
                                                                 (lambda (y) (apply-continuation errcont (format nil "return-from: undefined return block ~A" y))))))))
        ((equal (car exp) 'catch)         (myeval (cadr exp) env block-env go-env catch-env errcont
                                                  (lambda (symb-res)
                                                    (if (not (symbolp symb-res))
                                                        (apply-continuation errcont (format nil "catch: first argument not a symbol"))
                                                        (myeval (caddr exp)
                                                                env
                                                                block-env
                                                                (acons symb-res
                                                                       cont
                                                                       catch-env)
                                                                go-env
                                                                errcont
                                                                cont)))))
        ((equal (car exp) 'throw)         (myeval (cadr exp) env block-env go-env catch-env errcont
                                                  (lambda (symb-res)
                                                    (myeval (caddr exp) env block-env go-env catch-env errcont
                                                            (lambda (exp-res)
                                                              (assoc-2 symb-res catch-env
                                                                       (lambda (cont-res)
                                                                         (apply-continuation cont-res exp-res))
                                                                       (lambda (key)
                                                                         (apply-continuation errcont (format nil "throw: matching ~A catch is not found" key)))))))))
        ((equal (car exp) 'tagbody)       (tagbody-check-tag (cdr exp)
                                                             (lambda ()
                                                               (setq go-env
                                                                     (append (mapcar #'(lambda (x)
                                                                                         (cons (car x)
                                                                                               (lambda ()
                                                                                                 (evtagbody x env block-env go-env catch-env errcont cont))))
                                                                                     (tagbody-slice (cdr exp) nil))
                                                                             go-env))
                                                               (evtagbody (cdr exp) env block-env
                                                                          go-env
                                                                          catch-env errcont cont))
                                                             (lambda (x)
                                                               (apply-continuation errcont (format nil "tagbody: The tag ~A appears more than once in a tagbody" x)))))
        ((equal (car exp) 'go)            (assoc-2 (cadr exp) go-env
                                                   (lambda (x)
                                                     (apply-continuation x 'NOT-A-PARAM))
                                                   (lambda (x)
                                                     (apply-continuation errcont (format nil "go: wrong target ~A" x)))))
        ((equal (car exp) 'labels)        (let* ((alist (mapcar (lambda (label)
                                                                  (cons (car label) nil))
                                                                (cadr exp)))
                                                 (new-env (append alist env))
                                                 (closures (mapcar (lambda (label)
                                                                     (make-closure :body (caddr label)
                                                                                   :block-env block-env
                                                                                   :env new-env
                                                                                   :go-env go-env
                                                                                   :args (cadr label)))
                                                                   (cadr exp))))
                                            (loop :for aelt     :in alist
                                               :for closure  :in closures
                                               :do (rplacd aelt closure))
                                            (evprogn (cddr exp) new-env block-env go-env catch-env errcont cont)))
        (t                                (myeval (car exp) env block-env go-env catch-env errcont
                                                  (lambda (x)
                                                    (evlis  x  (cdr exp) nil env block-env go-env catch-env errcont cont))))))


(defun apply-continuation (cont arg)
  (print arg)
  (cond ((functionp cont)       (funcall cont arg))
        ((evcond-cont-p cont)   (if arg
                                    (myeval (cadar (evcond-cont-clauses cont))
                                            (evcond-cont-env cont)
                                            (evcond-cont-block-env cont)
                                            (evcond-cont-go-env cont)
                                            (evcond-cont-catch-env cont)
                                            (evcond-cont-errcont cont)
                                            (evcond-cont-cont cont))
                                    (evcond (cdr (evcond-cont-clauses cont))
                                            (evcond-cont-env cont)
                                            (evcond-cont-block-env cont)
                                            (evcond-cont-go-env cont)
                                            (evcond-cont-catch-env cont)
                                            (evcond-cont-errcont cont)
                                            (evcond-cont-cont cont))))
        ((evlis-cont-p cont)    (evlis (evlis-cont-fn cont)
                                       (cdr (evlis-cont-unevaled cont))
                                       (cons arg (evlis-cont-evaled cont))
                                       (evlis-cont-env cont)
                                       (evlis-cont-block-env cont)
                                       (evlis-cont-go-env cont)
                                       (evlis-cont-catch-env cont)
                                       (evlis-cont-errcont cont)
                                       (evlis-cont-cont cont)))
        (t (error 'bad-cont))))


;;(ass (equal 3 (myeval
;;'(labels ((len (xs)
;;            (cond ((null xs) 0)
;;                  (t (+ 1 (len (cdr xs)))))))
;;  (len '(a b c)))
;;  nil nil nil nil #'err #'ok)))

;;(labels ((a () ..) ...) ...)

;; test number eval
;; (assert (equal 123 (myeval 123 nil #'err #'ok)))

;; test autoreferenced functions
;; (assert (equal '+ (myeval '+  nil #'err #'ok)))

;; test booleans
;; (assert (equal 't   (myeval 't    nil #'err #'ok)))
;; (assert (equal 'nil (myeval 'nil  nil #'err #'ok)))

;; test lookup symbols in local environment
;; (assert (equal nil (myeval 'alfa  nil              (lambda (x) (format t "~%err: ~A" x) nil)  #'ok)))
;; (assert (equal 345 (myeval 'alfa  '((alfa . 345))  #'err                                      #'ok)))
;; test lookup symbols in global environment
;;(let ((*glob-env* '((alfa . 111))))
;; (assert (equal 111 (myeval 'alfa  nil (lambda (x) (format t "~%err: ~A" x) nil)  #'ok)))
;; (assert (equal nil (myeval 'beta  nil (lambda (x) (format t "~%err: ~A" x) nil)  #'ok))))

;; test quote
;; (assert (equal 'zzz (myeval '(quote zzz)  nil #'err #'ok)))

;; test if
;; (assert (equal 1 (myeval '(if t 1 2)  nil #'err #'ok)))
;; (assert (equal 2 (myeval '(if nil 1 2)  nil #'err #'ok)))


;; (assert (equal 1 (myeval '(car (quote (1 2 3))) nil #'err #'ok)))
;; (assert (equal 42 (myeval '(cond ((null '()) 42) (42 666)) nil #'err #'ok)))
;; (assert (equal 55 (myeval '((lambda (x y) (+ x y)) 42 13) nil #'err #'ok)))
;; (assert (equal '(3 . 42)  (myeval '(let ((x (+ 1 2))
;;                                         (y 42))
;;                                    (cons x y))
;;                                  nil #'err #'ok)))
;; (assert (equal 2 (myeval '(progn (print (+ 1 4))
;;                           2)
;;                         nil #'err #'ok)))

;; (assert (equal '(3 . 42)  (myeval '(let ((x (+ 1 2))
;;                                         (y 42))
;;                                    (print x)
;;                                    (print y)
;;                                    (cons x y))
;;                                  nil #'err #'ok)))
;;
;; (assert (equal 42 (myeval '(let* ((x 42) (y x))
;;                            y)
;;                          nil #'err #'ok)));
;;
;; (assert (equal 84 (myeval '(let* ((x 42) (y (* 2 x)))
;;                            y)
;;                          nil #'err #'ok)));
;;
;; (assert (equal 42 (myeval '(let* ((x 42) (y (* 2 x)))
;;                            y
;;                            x)
;;                          nil #'err #'ok)))
;;
;; (assert (equal 42 (myeval '(let ((x 42) (y 777))
;;                            y
;;                            x)
;;                          nil #'err #'ok)))


(defun repl ()
  (princ "microlisp>")
  (princ (myeval (read) nil nil nil nil #'identity #'identity))
  (terpri)
  (finish-output)
  (repl))

;; (repl)
