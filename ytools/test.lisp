(defparameter default-out-stream-var* '*standard-output*)

(defun is-whitespace (ch)
  (or (char= ch #\space)
      (not (graphic-char-p ch))))

(defun coerce-to-string (x)
  (cond ((stringp x) x)
        ((characterp x) (string x))
        ((numberp x)
         (princ-to-string x))
        ((symbolp x)
         (symbol-name x))
        (t (format nil "~a" x))   ))

(defun car-eq (x y)
  (and (consp x)
       (eq (car x) y)))

(defun ignore-convert (body)
  (cond ((and (not (null body))
              (consp (car body))
              (eq (caar body) 'ignore))
         `((declare (cl:ignore ,@(cdar body))) ,@(cdr body)))
        (t body)   ))

(defun underscores-elim (args)
  (let ((realargs '())
        (new-ignores '())
        (keyargs nil))
    (dolist (a args)
      (labels ((got-underscore ()
                 (let ((new (gensym)))
                   (push new new-ignores)
                   (push (subst new '_ a)
                         realargs))))
	    (cond ((eq a '_)
               (got-underscore))
              ((consp a)
               (cond (keyargs
                      (cond ((or (eq (car a) '_)
                                 (and (consp (car a))
                                      (eq (cadr (car a))
                                          '_)))
                             (got-underscore))
                            (t
                             (push a realargs))))
                     ((eq (car a) '_)
                      (got-underscore))
                     (t
                      (push a realargs))))
              (t
               (cond ((eq a '&key)
                      (setq keyargs t)))
               (push a realargs)))))
    (values (reverse realargs)
            new-ignores)))

(defun ignore-smooth (args body)
  (multiple-value-bind (newargs to-be-ignored)
      (underscores-elim args)
    (let ((realbod (ignore-convert body)))
      (cond ((null to-be-ignored)
             (values args realbod))
            (t
             (values newargs `((declare (cl:ignore ,@to-be-ignored))
                               ,@realbod)))))))

(cl:defmacro \\ (args &rest body)
  (multiple-value-bind (args body)
      (ignore-smooth args body)
    `(function (lambda ,args ,@body))   ))

(defun symstuff (l)
  `(concatenate 'string
                ,@(mapcan
                   (\\ (x)  (cond ((stringp x)
                                   (cond ((and (find-if #'alpha-char-p x)
                                               (not (find-if #'is-whitespace
                                                             x)))
                                          (format *error-output*
                                                  "Warning: alphabetic string ~s in ~
                                      build-symbol;~
                                    ~%  more portable to use symbols.~
                                    ~%  to make this warning go away ~
                                      replace with :~a)"
                                                  x x)))
                                   (list `',x))
                                  ((symbolp x)
                                   (list `',(symbol-name x)))
                                  ((atom x)
                                   (list `(coerce-to-string ,x)))
                                  ((member (car x) '(:< <) :test #'eq)
                                   (mapcar (\\ (y) `(coerce-to-string ,y)   )
                                           (cdr x)))
                                  ((member (car x) '(:++ ++) :test #'eq)
                                   (list `(princ-to-string (incf ,(cadr x)))))
                                  (t (list `(coerce-to-string ,x)))   ))
                   l)   ))

;; Macro for building new symbols.
;; (BUILD-SYMBOL [(:package <p>)] -specs-) creates a symbol
;; whose print name is built out of specs.  An atomic or string spec is
;; concatenated in.  A spec of the form (:< e1 e2 ...) has each eI evaluated
;; and its characters concatenated in.  A spec of the form (:++ e) increments e
;; and concatenates in its characters.  Anything else is supposed to
;; evaluate to a list of characters, which are concatenated in.
;; Example: If A = FOO, B = (1 2 3), and C = (#\B \#A \#R), then
;; (build-symbol (:< A) (:++ (CAR B)) (CDR C) "< >") is FOO2AR</ >, and
;; B becomes (2 2 3).
(defmacro build-symbol (&rest l)
  (let ((p (find-if #'(lambda (x) (and (consp x) (eq (car x) ':package)))
                    l)))
    (cond (p
           (setq l (remove p l))))
    (let ((pkg (cond ((member (cadr p) '(nil) :test #'eq)
                      nil)
                     (t `(find-package ,(cadr p))))))
      (cond (p
             (cond (pkg
                    `(values (intern ,(symstuff l) ,pkg)))
                   (t
                    `(make-symbol ,(symstuff l)))))
            (t
             `(values (intern ,(symstuff l))))))))

(defparameter symno* 0) ;; General-purpose counter for built symbols

(defun gen-var (sym)
  (build-symbol (:package nil) (< sym) - (++ symno*)))

(define-setf-expander alref. (alist^ key^ &optional (default^ 'nil)
                              &key ((:test test^) '#'eq)
                                ((:acc acc^) '#'right)
                                ((:new-entry new-entry^) 'nil))
  (multiple-value-bind (altemps alvals alstores alist-set alist-acc)
      (get-setf-expansion alist^)
    (let ((key-var (gensym)) (alist-var (gensym)) (entry-var (gensym))
          (new-var (gensym))
          (store-var (car alstores)))
      (let ((acc-form (cond ((and (consp acc^)
                                  (member (car acc^) '(function quote funktion) :test #'eq))
                             `(,(cadr acc^) ,entry-var))
                            (t
                             (error "Can't set ~s of alref. entry in ~s"
                                    acc^ alist^)))))
	    (values `(,@altemps ,key-var ,alist-var
                            ,entry-var)
                `(,@alvals ,key^ ,alist-acc
                           (assoc= ,test^ ,key-var ,alist-var))
                `(,new-var)
                `(progn
                   (cond ((not ,entry-var)
                          (setq ,entry-var (cons ,key-var ,new-entry^))
                          (let ((,store-var (cons ,entry-var ,alist-var)))
                            ,alist-set)))
                   (setf ,acc-form ,new-var))
                `(cond (,entry-var ,acc-form)
                       (t ,default^)))))))

;;; Most common special case.
(defmacro alref (alist^ key^ &optional (default^ 'nil)
			     &key ((:test test^) '#'eq))
  `(alref. ,alist^ ,key^ ,default^ :test ,test^ :acc #'second :new-entry (list nil)))

;;; Each entry is of form (char global-handler -local-handlers-)
;;; The current readtable is looked up in the local-handlers (itself an
;;; alist).  If no entry, use global (the value of ytools-readtable*).
;;; If no global, '!' should be taken
;;; as an ordinary character.
(defparameter excl-handlers* '())

;;; The following two procedures return an access function and a set function
;;; for the place in
;;; an entry where the excl-reader procedure is stored.
;;; Note that if no special handler is associated with the readtable,
;;; we return the handler, if any, associated with ytools-readtable*.
;;; Specifying false as the value for rt is the same as specifying
;;; ytools-readtable*.
(defun readtable-excl-acc (rt)
  (cond ((or (not rt) (eq rt ytools-readtable*))
         (\\ (entry) (cadr entry)))
        (t
         (\\ (entry)
             (let ((rte (assoc rt (cddr entry) :test #'eq)))
               (cond (rte (cadr rte))
                     (t (cadr entry))))))))

(defun excl-reader (srm ch)
  (setq ch (peek-char nil srm nil nil))
  (cond ((not ch)
         ;; end of file
         (intern "!"))
        (t
         (labels ((nonmacro ()
                    (cond ((member ch '(#\space #\tab #\newline
                                        #\return #\linefeed
                                        #\page #\( #\))
                                   :test #'char=)
                           (intern "!"))
                          (t
                           ;; if any problems here, could try
                           ;; UNREAD-CHAR + (VALUES)
                           (values
                            (intern (concatenate 'string
                                                 "!"
                                                 (string (read srm t nil t)))))))))
           (let ((e (assoc ch excl-handlers* :test #'eq)))
             (cond (e
                    (let ((handler
                           (funcall (readtable-excl-acc *readtable*) e)))
                      (cond (handler
                             (read-char srm)
                             (funcall handler srm ch))
                            (t
                             (nonmacro)))))
                   (t (nonmacro))))))))

(set-macro-character #\! #'excl-reader)

;;; Alist of (hook-name new-expander) pairs
;; (defvar macro-hooks* !())

(defun check-for-macro-hook (name whole env expander)
  (let ((h (alref macro-hooks* name)))
    (cond (h
           (funcall h whole env expander))
          (t
           (funcall expander whole env)))))


(defmacro def-hooked-macro (name args &body body)
;;; Premature complexification --
;;;;   (multiple-value-bind (hookname args body)
;;;;                        (cond ((is-Symbol args)
;;;;                               (values args (car body) (cdr body)))
;;;;                              (t
;;;;                               (values name args body)))
  ;; We have to fish out the &whole and &environment parameters
  ;; because (a) we need to refer to them and (b) the param list
  ;; gets recycled into a 'destructuring-bind', which doesn't allow
  ;; an &environment param --
  (multiple-value-bind (env-var d-args)
      (let ((evtl (member '&env args)))
        (cond (evtl
               (values (cadr evtl)
                       (nconc (ldiff args evtl)
                              (cddr evtl))))
              (t
               (values (gen-var 'env)
                       args))))
    (let ((whole-var
           (cond ((car-eq d-args '&whole)
                  (values (cadr d-args) (cddr d-args)))
                 (t
                  (values (gen-var 'whole) d-args))))
          (rest-var (gen-var 'rest)))
      `(defmacro ,name (&whole ,whole-var &rest ,rest-var
                        &environment ,env-var)
         (declare (ignore ,rest-var))
         (check-for-macro-hook
          ',name ,whole-var ,env-var
          (\\ (,whole-var ,env-var)
              (declare (ignorable ,env-var))
              (destructuring-bind ,d-args (rest ,whole-var)
                ,@body)))))))

(def-hooked-macro datafun-table (name ind &key (size 100))
  `(eval-when  (:compile-toplevel :load-toplevel :execute)
     (defvar ,name (make-hash-table :size ',size :test #'eq))
     (datafun attach-datafun ,ind
              (defun :^ (ind sym fname)
                (ignore ind)
                (setf (table-entry ,name sym) (symbol-function fname))))))

(datafun-table out-ops* out-operator)


(defun get-out-operator (exp)
  (and (consp exp)
	   (symbolp (car exp))
	   (gethash (car exp) out-ops*)))

(defun out-expand (exp stream)
  (cond ((stringp exp)
	     `(princ ,exp (out-prepare ,stream)))
	    ((numberp exp)
	     (if (> exp 0)
             `(print-spaces ,exp (out-prepare ,stream))
             `(out-srmlines ,(- exp) ,stream)))
	    ((member exp '(t :%) :test #'eq)
	     `(out-srmlines 1 ,stream))
	    (t
	     (let ((out-op (get-out-operator exp)))
           (cond (out-op
                  (funcall out-op exp stream))
                 (t
                  `(prin1 ,exp (out-prepare ,stream))))))))

(defun expand-out-body (exps stream)
  (mapcar #'(lambda (e)
              (out-expand e stream))
	      exps))


(defmacro out (&rest exps)
  (let ((stream (gensym))
	;;;;(cleanup-forms* '())
        (stream-form default-out-stream-var*)
        (string-output nil))
    (cond ((and (not (null exps))
                (consp (car exps))
                (member (car (car exps)) '(:to to) :test #'eq))
           ;; (:to s) almost always occurs as first thing, so we
           ;; catch this here
           (cond ((not (eq (cadar exps) 't))
                  (setq stream-form (cadar exps))))
           (setq exps (cdr exps))))
    (cond ((member stream-form '(:string nil) :test #'eq)
           (setq string-output t)
           (setq stream-form (gensym))))
    (let ((code `(let ((,stream (stream-outify ,stream-form)))
                   (out-indent ,stream 0
                               (let ((out-vals* '()))
                                 (cl:declare (special out-vals*))
                                 ,@(expand-out-body exps stream)
                                 ,@(include-if (not string-output)
                                               '(list->values out-vals*)))))))
      (cond (string-output `(with-output-to-string (,stream-form)
                              ,code))
            (t code)))))

(out (:to srm) (foo v1 v2) :%)
