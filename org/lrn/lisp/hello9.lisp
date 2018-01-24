;(labels ((a () ( ..... (b)))
;         (b () ( ..... (a))))
;        (a))



(setf *print-circle* t)

(defun is-cont-subset (target-cont cont)
  (cond ((equal target-cont cont) t)
        ((functionp cont) nil)
        (t (is-cont-subset target-cont (cdr cont)))))

(defun tagbody-check-tag (exp cont errcont)
  (cond ((null exp) (funcall cont))
        ((and (symbolp (car exp))
              (member (car exp) (cdr exp)))
         (funcall errcont (car exp)))
        (t (tagbody-check-tag (cdr exp) cont errcont))))

#|
(block test
    (defun foo ()
          (return-from test 666)))

(foo)
|#

; one-shot continuation

;; test function ok
(defun ok (x)
  (format t "~%ok: ~A" x)
  x)

;; test function ok
(defun err (x)
  (format t "~%err: ~A" x)
  x)

;; base assoc
(defun assoc-1 (key alist)
  (cond ((null alist) nil)
        ((equal key (caar alist)) (car alist))
        (t (assoc-1 key (cdr alist)))))

;; semipredicate problem
(defun assoc-2 (key alist cont errcont) ;; NB!: inverted order of continuations (for lookup comfort)
  (cond ((null alist)              (funcall errcont key))
        ((equal key (caar alist))  (funcall cont    (cdar alist)))
        (t                         (assoc-2 key (cdr alist) cont errcont))))

;; test assoc-2
;; (assert (equal "ok:123"
;;               (assoc-2 'alfa '((alfa . 123)) (lambda (x) (format nil "ok:~A" x)) (lambda (x) (format nil "err:~A" x)))))
;; (assert (equal "err:ALFA"
;;               (assoc-2 'alfa '((beta . 123)) (lambda (x) (format nil "ok:~A" x)) (lambda (x) (format nil "err:~A" x)))))

;; global environment
(defparameter *glob-env* nil)

;; lookup
(defun lookup (symb env errcont cont)
  (assoc-2 symb env
           (lambda (x)
             (apply-continuation cont x))
           (lambda (key)
             (assoc-2 key *glob-env*
                      (apply-continuation cont nil)
                      (lambda (key)
                        (apply-continuation errcont (format nil "UNBOUD VARIABLE [~A] ~%LOCAL ENV: [~A] ~%GLOBAL ENV: [~A]"
                                                            key env *glob-env*)))))))

;; test lookup
;; (assert (equal "ok:123" (lookup 'aaa '((aaa . 123))
;;                                (lambda (x) (format nil "err:~A" x))
;;                                (lambda (x) (format nil "ok:~A" x)))))
;; (assert (equal nil      (lookup 'aaa '((bbb . 123))
;;                                (lambda (x) (declare (ignore x)) nil)
;;                                (lambda (x) (format nil "ok:~A" x)))))


;; closure struct
(defstruct closure
  body
  env
  block-env
  go-env
  args)


(defstruct unicont
  block-env
  go-env
  catch-env
  errcont
  cont)

(defstruct (evcond-cont (:include unicont))
  clauses
  env)

;; evcond
(defun evcond (clauses env block-env go-env catch-env errcont cont)
  (cond ((null clauses)  (apply-continuation cont nil))
        (t               (myeval (caar clauses) env block-env go-env catch-env errcont
                                 (make-evcond-cont
                                  :clauses clauses
                                  :env env
                                  :block-env block-env
                                  :go-env go-env
                                  :catch-env catch-env
                                  :errcont errcont
                                  :cont cont)))))

;; tests for envcond
;; (assert (equal 2 (evcond '((t 2) (t 1)) nil #'err #'ok)))
;; (assert (equal 1 (evcond '((nil 2) (t 1)) nil #'err #'ok)))
;; (assert (equal nil (evcond '((nil 2) (nil 1)) nil #'err #'ok)))


;; mypairlis
(defun mypairlis (lst1 lst2 alist)
  (cond ((and (null lst1) (null lst2)) alist)
        ((or  (null lst1) (null lst2)) (error 'mypairlis-error))
        (t (cons (cons (car lst1) (car lst2)) (mypairlis (cdr lst1) (cdr lst2) alist)))))

(defstruct (evlis-cont (:include unicont))
  fn
  unevaled
  evaled
  env)

;; более эффективный вариант evlis
(defun evlis (fn unevaled evaled env block-env go-env catch-env errcont cont)
  (cond ((null unevaled)  (myapply fn (reverse evaled) catch-env errcont cont))
        (t                (myeval (car unevaled) env block-env go-env catch-env errcont
                                  (make-evlis-cont
                                   :fn fn
                                   :unevaled unevaled
                                   :evaled evaled
                                   :env env
                                   :block-env block-env
                                   :go-env go-env
                                   :catch-env catch-env
                                   :errcont errcont
                                   :cont cont)))))

;; test for evlis
;; (assert (equal 4 (evlis '+ '(1 (+ 1 2)) nil nil #'err #'ok)))

(defstruct (evtagbody-cont (:include unicont))
  body
  env)

(defun evtagbody (body env block-env go-env catch-env errcont cont)
  (cond ((null (car body))      (apply-continuation cont nil))
        ((symbolp (car body))   (evtagbody (cdr body) env block-env go-env catch-env errcont cont))
        (t                      (myeval (car body) env block-env go-env catch-env errcont
                                        (make-evtagbody-cont
                                          :body (cdr body)
                                          :env  env
                                          :block-env block-env
                                          :go-env go-env
                                          :catch-env catch-env
                                          :errcont errcont
                                          :cont cont)))))

(defun tagbody-slice (exp)
  (cond ((null exp)           nil)
        ((symbolp (car exp))  (cons exp  (tagbody-slice (cdr exp))))
        (t                    (tagbody-slice (cdr exp)))))

(defun tagbody-slice (exp res)
  (cond ((null exp) res)
        ((symbolp (car exp))  (tagbody-slice (cdr exp) (cons exp res)))
        (t                   (tagbody-slice (cdr exp) res))))





(defstruct (evprogn-cont (:include unicont))
  env
  forms)

;; evprogn
(defun evprogn (forms env block-env go-env catch-env errcont cont)
  (cond ((null forms)         (apply-continuation cont nil))
        ((null (cdr forms))   (myeval (car forms) env block-env go-env catch-env errcont cont))
        (t                    (myeval (car forms) env block-env go-env catch-env errcont
                                      (make-evprogn-cont
                                        :block-env block-env
                                        :go-env go-env
                                        :catch-env catch-env
                                        :errcont errcont
                                        :cont cont
                                        :env env
                                        :forms (cdr forms))))))

;; test for evprogn
;; (assert (equal 2 (evprogn '(1 2) nil #'err #'ok)))

(defstruct (evletstar-cont (:include unicont))
  varpairs
  exp
  env)

;; evletstar
(defun evletstar (varpairs exp env block-env go-env catch-env errcont cont)
  ;;(print varpairs)
  (cond ((null varpairs)  (evprogn exp env block-env go-env catch-env errcont cont))
        (t                (myeval (cadar varpairs) env block-env go-env catch-env errcont
                                  (make-evletstar-cont
                                    :block-env block-env
                                        :go-env go-env
                                        :catch-env catch-env
                                        :errcont errcont
                                        :cont cont
                                        :varpairs varpairs
                                        :exp exp
                                        :env env)))))


;; test for evletstar
;; (assert (equal 2 (evletstar '((a 1) (b a)) '(4 (+ a b)) nil  #'err #'ok)))

(defstruct (evlet-cont (:include unicont))
  vars
  exps
  evald-exps
  exp
  env)

;; evlet
(defun evlet (vars exps evald-exps exp env block-env go-env catch-env errcont cont)
  (cond ((null exps)  (evprogn exp
                               (pairlis vars (reverse evald-exps) env)
                               block-env
                               catch-env
                               go-env
                               errcont
                               cont))
        (t            (myeval (car exps) env block-env go-env catch-env errcont
                              (make-evlet-cont
                                :vars vars
                                :exps exps
                                :evald-exps evald-exps
                                :exp exp
                                :env env
                                :block-env block-env
                                :go-env go-env
                                :catch-env catch-env
                                :errcont errcont
                                :cont cont)))))

;; test for evlet
;; (assert (equal 3 (evlet '(a b) '(1 2) nil '(4 (+ a b)) nil #'err #'ok)))


(defstruct (or-cont (:include unicont))
  exps
  env)

(defun evor (exps env block-env go-env catch-env errcont cont)
  (cond ((null exps) (apply-continuation cont nil))
        ((myeval (car exps) env block-env go-env catch-env errcont
                 (make-or-cont
                   :exps (cdr exps)
                   :env env
                   :block-env block-env
                   :go-env go-env
                   :catch-env catch-env
                   :errcont errcont
                   :cont cont)))))


(defstruct (go-cont (:include unicont))
  slice
  env)

(defun make-go-env (tagbody-body env block-env go-env catch-env errcont cont)
  (let* ((conts (mapcar #'(lambda (x)
                            (make-go-cont
                              :slice x
                              :env env
                              :block-env block-env
                              :go-env go-env
                              :catch-env  catch-env
                              :errcont errcont
                              :cont cont))
                          (tagbody-slice tagbody-body nil)))
         (new-go-env (append (mapcar #'(lambda (go-cont)
                                         (cons (car (go-cont-slice go-cont))
                                               go-cont))
                                     conts)
                             go-env)))
    (loop :for elt-cont :in conts :do
      (setf (go-cont-go-env elt-cont)
            new-go-env))
    new-go-env))

(defun apply-go-continuation (go-cont)
  (evtagbody (go-cont-slice go-cont)
             (go-cont-env go-cont)
             (go-cont-block-env go-cont)
             (go-cont-go-env go-cont)
             (go-cont-catch-env go-cont)
             (go-cont-errcont go-cont)
             (go-cont-cont go-cont)))

;; myapply
(defun myapply (fn args catch-env errcont cont)
  (cond ((equal fn '+)             (apply-continuation cont (+ (car args) (cadr args))))
        ((equal fn '*)             (apply-continuation cont (* (car args) (cadr args))))
        ((equal fn 'car)           (apply-continuation cont (car (car args))))
        ((equal fn 'cdr)           (apply-continuation cont (cdr (car args))))
        ((equal fn 'cons)          (apply-continuation cont (cons (car args) (car (cdr args)))))
        ((equal fn 'null)          (apply-continuation cont (null (car args))))
        ((equal fn 'print)         (apply-continuation cont (print (car args))))
        ((closure-p fn)            (myeval (closure-body fn)
                                           (pairlis (closure-args fn)
                                                    args
                                                    (closure-env fn))
                                           (closure-block-env fn)
                                           catch-env
                                           (closure-go-env fn)
                                           errcont
                                           cont))))

;; myeval
(defun myeval (exp env block-env go-env catch-env errcont cont)
  ;; (print exp)
  (cond ((numberp exp)                    (apply-continuation cont exp))
        ((member exp '(+ * car cdr cons null print))
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
                                                                 (lambda (y)
                                                                   (if (is-cont-subset y cont)
                                                                       (apply-continuation y x)
                                                                       (apply-continuation errcont
                                                                         (format nil "return-from: attempt to RETURN-FROM to ~A that no longer exists" (cadr exp)))))
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
                                                               (evtagbody (cdr exp) env block-env
                                                                          (make-go-env (cdr exp) env block-env go-env catch-env errcont cont)
                                                                          catch-env errcont cont))
                                                             (lambda (x)
                                                               (apply-continuation errcont (format nil "tagbody: The tag ~A appears more than once in a tagbody" x)))))
        ((equal (car exp) 'go)            (assoc-2 (cadr exp) go-env
                                                   (lambda (go-cont)
                                                     (apply-go-continuation go-cont))
                                                   (lambda (go-label)
                                                     (apply-continuation errcont (format nil "go: wrong target ~A" go-label)))))
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
                                            (loop :for aelt  :in alist
                                               :for closure  :in closures
                                               :do (rplacd aelt closure))
                                            (evprogn (cddr exp) new-env block-env go-env catch-env errcont cont)))
        ((equal (car exp) 'or)            (evor (cdr exp) env block-env go-env catch-env errcont cont))
        (t                                (myeval (car exp) env block-env go-env catch-env errcont
                                                  (lambda (x)
                                                    (evlis  x  (cdr exp) nil env block-env go-env catch-env errcont cont))))))


(defun apply-continuation (cont arg)
  (cond ((functionp cont)       (funcall cont arg))
        ((evcond-cont-p cont)   (if arg
                                    (myeval (cadar (evcond-cont-clauses cont))
                                            (evcond-cont-env cont)
                                            (unicont-block-env cont)
                                            (unicont-go-env cont)
                                            (unicont-catch-env cont)
                                            (unicont-errcont cont)
                                            (unicont-cont cont))
                                    (evcond (cdr (evcond-cont-clauses cont))
                                            (evcond-cont-env cont)
                                            (unicont-block-env cont)
                                            (unicont-go-env cont)
                                            (unicont-catch-env cont)
                                            (unicont-errcont cont)
                                            (unicont-cont cont))))
        ((evlis-cont-p cont)    (evlis (evlis-cont-fn cont)
                                       (cdr (evlis-cont-unevaled cont))
                                       (cons arg (evlis-cont-evaled cont))
                                       (evlis-cont-env cont)
                                       (unicont-block-env cont)
                                       (unicont-go-env cont)
                                       (unicont-catch-env cont)
                                       (unicont-errcont cont)
                                       (unicont-cont cont)))
        ((evprogn-cont-p cont)  (evprogn (evprogn-cont-forms cont)
                                         (evprogn-cont-env cont)
                                         (unicont-block-env cont)
                                         (unicont-go-env cont)
                                         (unicont-catch-env cont)
                                         (unicont-errcont cont)
                                         (unicont-cont cont)))
        ((evletstar-cont-p cont) (evletstar (cdr (evletstar-cont-varpairs cont))
                                            (evletstar-cont-exp cont)
                                            (acons (caar (evletstar-cont-varpairs cont))
                                                                             arg
                                                                             (evletstar-cont-env cont))
                                            (unicont-block-env cont)
                                            (unicont-go-env cont)
                                            (unicont-catch-env cont)
                                            (unicont-errcont cont)
                                            (unicont-cont cont)))
        ((evtagbody-cont-p cont) (evtagbody (evtagbody-cont-body cont)
                                            (evtagbody-cont-env cont)
                                            (unicont-block-env cont)
                                            (unicont-go-env cont)
                                            (unicont-catch-env cont)
                                            (unicont-errcont cont)
                                            (unicont-cont cont)))
        ((evlet-cont-p cont)     (evlet (evlet-cont-vars cont)
                                        (cdr (evlet-cont-exps cont))
                                        (cons arg (evlet-cont-evald-exps cont))
                                        (evlet-cont-exp cont)
                                        (evlet-cont-env cont)
                                        (evlet-cont-block-env cont)
                                        (evlet-cont-go-env cont)
                                        (evlet-cont-catch-env cont)
                                        (evlet-cont-errcont cont)
                                        (evlet-cont-cont cont)))
        ((or-cont-p cont)       (if (not (null arg))
                                    (apply-continuation (or-cont-cont cont) arg)
                                    (evor (or-cont-exps cont)
                                          (or-cont-env cont)
                                          (or-cont-block-env cont)
                                          (or-cont-go-env cont)
                                          (or-cont-catch-env cont)
                                          (or-cont-errcont cont)
                                          (or-cont-cont cont))))
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
