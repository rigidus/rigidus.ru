(require 'anaphora)
(require 'optima)
(require 'fare-quasiquote-extras)
(require 'fare-quasiquote-optima)

(use-package :anaphora)
(use-package :optima)

(named-readtables:in-readtable :fare-quasiquote)

(match '(let ((x 2)) (print x))
  (`(let ,var-forms ,@forms) (list :var-forms var-forms :forms forms)))

(defun maptree (predicate-transformer tree)
  (multiple-value-bind (t-tree control)
      (aif (funcall predicate-transformer tree)
           it
           (values tree #'mapcar))
    (if (and (consp t-tree)
             control)
        (funcall control
                 #'(lambda (x)
                     (maptree predicate-transformer x))
                 t-tree)
        t-tree)))

(defmacro mtm (transformer tree)
  (let ((lambda-param (gensym)))
    `(maptree #'(lambda (,lambda-param)
                  (values (match ,lambda-param ,transformer)
                          #'mapcar))
              ,tree)))

(let ((acc)
      (block-tag-label))
  (defun global-let (param)
    (mtm (`(let ,var-forms ,@forms)
           `(progn ,@(mapcar #'(lambda (var-form)
                                 (push var-form acc)
                                 `(setf ,(car var-form) ,(cadr var-form)))
                             var-forms)
                   ,@(mapcar #'global-let forms)))
         param))
  (defun remove-block-return (param)
    (cond ((consp param)
           (acond ((match param (`(block nil ,@forms) forms))
                   (progn
                     ;; (print (list "=================" it))
                     `(tagbody ,@(progn
                                  (car (push (gensym "BTL-") block-tag-label))
                                  (mapcar #'remove-block-return it))
                         ,(progn (pop block-tag-label)))
                     ))
                  ((match param (`(return-from nil (progn nil)) t))
                   (progn
                     `(go ,(car block-tag-label))
                     ))
                  (t (mapcar #'remove-block-return param))))
          (t param)))
  (defun remove-progn (param)
    (mtm (`(progn ,@forms)
           `(,@(mapcar #'remove-progn forms)))
         param))
  (defun remove-tagbody (param)
    (mtm (`(tagbody ,@forms)
           `(,@(mapcar #'remove-tagbody forms)))
         param))
  (defun clear-acc ()
    (setf acc nil))
  (defun get-acc ()
    acc)
  (defun replace-yield (param post-yield)
    (mtm (`(yield ,@forms)
           `(tagbody
               (setf state t)
               (return-from the-test ,@forms)
             ,post-yield))
         param))
  (defun run (param)
    (clear-acc)
    (let ((rs (global-let param))
          (post-yield (gensym "POST-YIELD-")))
      `(let ,(append (get-acc) `((state)))
         (defun the-test
             (tagbody
                (unless (null state)
                  (go ,post-yield))
                ,@(let ((lines))
                       (labels ((rec (param)
                                  (mapcar #'(lambda (x)
                                              (cond ((and (consp x) (consp (car x))) (rec x))
                                                    (t (setf lines (append lines (list x))))))
                                          param)))
                         (rec
                          (remove-tagbody (remove-progn (replace-yield (remove-block-return rs) post-yield))))
                         (remove-if #'(lambda (x)
                                        (or (and (consp x)
                                                 (equal (car x) 'declare))
                                            (null x)))
                                    lines))
                       )))))))

(defparameter *test* (sb-cltl2:macroexpand-all
                      '(dotimes (x 3)
                        (dotimes (y 3) (yield (cons x y))))))


(defmacro with-yield (forms)
  `,(run (sb-cltl2:macroexpand-all forms)))

(print
 (sb-cltl2:macroexpand-all '(with-yield
                             (dotimes (x 3)
                               (dotimes (y 3) (yield (cons x y)))))))

(with-yield
    (dotimes (x 3)
      (dotimes (y 3) (yield (cons x y)))))

(test)


(LET ((Y 0) (X 0) (STATE))
  (defun the-test ()
    (TAGBODY
       (UNLESS (NULL STATE)
         (GO POST-YIELD-1220))
       (SETF X 0)
       (GO G1217)
     G1216
       (SETF Y 0)
       (GO G1219)
     G1218
       (SETF STATE T)
       (RETURN-FROM THE-TEST (CONS X Y))
     POST-YIELD-1220
       (SETQ Y (1+ Y))
     G1219
       (IF (>= Y 3)
           NIL
           (GO G1218))
       (GO BTL-1222)
     BTL-1222
       (SETQ X (1+ X))
     G1217
       (IF (>= X 3)
           NIL
           (GO G1216))
       (GO BTL-1221)
     BTL-1221)))

(the-test)





;; (let ((state nil)
;;       (x 0)
;;       (y 0))
;;   (defun test ()
;;     (tagbody
;;        (if (null state)
;;            (go label-test-x)
;;            (go label-next))
;;      label-iteration-x
;;        (go label-test-y)
;;      label-iteration-y
;;        (setq state t)
;;        (return-from test (print (cons x y)))
;;      label-next
;;        (setq y (1+ y))
;;      label-test-y
;;        (unless (>= y 3)
;;          (go label-iteration-y))
;;        (setq y 0)
;;      exit-y
;;        (setq x (1+ x))
;;      label-test-x
;;        (unless (>= x 3)
;;          (go label-iteration-x))))
;;   (test)
;;   )

;; (test)



;;; Here's an example that you gave earlier
(defun iter (n)
  (as-generator
   (yield 1) (dotimes (i n) (yield i))))

;;; And here's an implementation of it
(defclass generator ()
  ((empty? :initform nil :accessor empty?)
   (bound :initform -1 :accessor bound)
   (value :initform nil :accessor value)
   (closure :initarg :closure :accessor closure)))

(defgeneric step! (gen)
  (:method ((gen generator))
    (cond
      ((empty? gen) nil)
      (t (funcall (closure gen) gen) (value gen)))))

(defun iter (n)
  (let ((place 1)
        (counter 0))
    (make-instance 'generator :closure #'(lambda (gen)
                                           (block my-iter
                                             (tagbody
                                              0 (case place
                                                  (1 (go 1))
                                                  (2 (go 2)))
                                              1 (setf place 2)
                                                (setf (value gen) 1)
                                                (unless (< counter n)
                                                  (setf (empty? gen) t))
                                                (return-from my-iter nil)
                                              2 (when (< counter n)
                                                  (setf (value gen) counter))
                                                (incf counter)
                                                (unless (< counter n)
                                                  (setf (empty? gen) t))
                                                (return-from my-iter nil)))))))

(defparameter *gen* (iter 5))

(loop until (empty? *gen*) do (print (step! *gen*)))
=>
1
0
1
2
3
4
NIL




;;I start out with a continuation datatype:

(defclass continuation ()
  ((func-object :initarg :func-object :reader func-object)))

;;((A -> Answer) -> Answer) -> (continuation-of A) -- a funcallable tag
(defun continuation (fun)
  (make-instance 'continuation :func-object fun))

(defmethod unit ((type (eql 'continuation)) val)
  (continuation #'(lambda (current) (funcall current val))))

(defmethod bind ((monad-val continuation) next)
  (continuation #'(lambda (current)
                    (funcall (func-object monad-val)
                             (lambda (val)
                               (funcall (func-object (funcall next val))
                                        current))))))

(defmethod run ((monad-val continuation))
  (funcall (func-object monad-val) #'identity))


;;;((A -> continuation) -> continuation) -> continuation
(defun call/cc (entry-point)
  (continuation #'(lambda (current)
                    (flet ((escape (val)
                             ;;compare with unit
                             (continuation #'(lambda (final)
                                               (declare (ignore final))
                                               (funcall current val)))))
                      ;;compare with bind
                      (funcall (func-object (funcall entry-point #'escape))
                               current)))))


;; --> I can define yield as a regular, top-level function:
(defun yield (val)
  (continuation #'(lambda (current)
                    (cons current val))))

;; --> add a run method for cons (prolly means yields should be their own class)
(defmethod run ((monad-val cons))
  monad-val)


;;;add a helper macro

;;;just like its says: sequential evaluation threaded with binds
(defmacro monad-progn (&body forms)
  (labels ((make-bind-seq (forms)
             (cond ((null forms)
                    nil)
                   ((null (cdr forms))
                    (car forms))
                   (t (let ((_ (gensym)))
                        `(bind ,(car forms)
                               #'(lambda(,_)
                                   (declare (ignore ,_))
                                   ,(make-bind-seq (cdr forms)))))))))
    (make-bind-seq forms)))

;;; and define a generator facility

(defconstant gen-nil (unit 'continuation nil))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun generator-body (body)
    (let ((escape (gensym)))
      `(call/cc
        #'(lambda (,escape)
            (flet ((yield (x)
                     (call/cc
                      #'(lambda (escape1)
                          (funcall ,escape (cons escape1 x))))))
              (monad-progn
               ,@body
               gen-nil)))))))

(defmacro defgen (name args &body body)
  `(defun ,name ,args
     ,(generator-body body)))


;; --> redefine defgen:
(defmacro defgen (name args &body body)
  `(defun ,name ,args
     (monad-progn
       ,@body
       gen-nil)))

;; --> and the rest of the code should work unchanged
(defmacro monad-progn (&body forms)
  (cond ((null forms)
         nil)
        ((null (cdr forms))
         (car forms))
        (t (let ((_ (gensym)))
             `(bind ,(car forms)
                    #'(lambda(,_)
                        (declare (ignore ,_))
                        (monad-progn ,@(cdr forms))))))))

(defun next (generator &optional return-value)
  (let ((gen-pair (run generator)))
    (if (null gen-pair)
        (values nil nil)
        (destructuring-bind (gen . val) gen-pair
          (values (funcall gen return-value) val)))))


(defconstant gen-nil (unit 'continuation nil))


;;here's an example
;;; the generator needs to written in a monadic style
;;; defgen binds the function yield in the body
(defgen leaves (tree)
  (labels ((leaves (tree)
             (cond ((null tree)
                    gen-nil)
                   ((and (null (cadr tree)) (null (cddr tree)))
                    (yield (car tree)))
                   (t (monad-progn
                       (leaves (cadr tree))
                       (leaves (cddr tree)))))))
    (leaves tree)))

;;; the consumer uses next to iterate through the generator
;;; the interface shares a lot of similarities with lazy-lists
(defun same-fringe (t1 t2 &optional (pred #'eql))
  (labels ((luup (gen1 gen2)
             (multiple-value-bind (gen1 val1) (next gen1)
               (multiple-value-bind (gen2 val2) (next gen2)
                 (cond ((and (null gen1) (null gen2)) t)
                       ((or (null gen1) (null gen2)) nil)
                       ((funcall pred val1 val2)
                        (luup gen1 gen2))
                       (t nil))))))
    (or (eq t1 t2)
        (luup (leaves t1) (leaves t2)))))



(same-fringe '(3 (2 (1 NIL)) 5 (4 NIL) 6 NIL)
             '(3 (2 (1 NIL)) 5 (4 NIL) 6 NIL))
==> T

(same-fringe '(3 (2 (0 NIL)) 5 (4 NIL) 6 NIL)
             '(3 (2 (1 NIL)) 5 (4 NIL) 6 NIL))
==> NIL

(same-fringe '(3 (2 (1 NIL)) 5 (4 NIL) 6 NIL)
             '(0 (2 (1 NIL)) 5 (4 NIL) 6 NIL))

==> T
