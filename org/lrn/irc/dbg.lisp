(defpackage :gebug
  (:use :common-lisp)
  (:shadow
   #:defun
   #:defmethod
   #:debug))

(in-package :gebug)

;; Say (DEBUG <fun-1> ... <fun-n>) to enable debugging of a function or method.
;; Say (UNDEBUG <fun-1> ... <fun-n>) to disable again.
;; Say (UNDEBUG) to disable all debugging.

(defmacro defun (name args &body body)
  `(macrolet ((%current-function () '',name)
              (%current-file () '',*compile-file-truename*))
     (cl:defun ,name ,args
       ,@body)))

(defun method-name (name arguments)
  ;; we are lazy ...
  (list 'method name
        (loop for argument in arguments
           when (member argument lambda-list-keywords) do (return)
           collect (if (consp argument)
                       (cadr argument)
                       'T))))

(defmacro defmethod (name args &body body)
  `(macrolet ((%current-function () '',(method-name name args))
              (%current-file () '',*compile-file-truename*))
     (cl:defmethod ,name ,args
       ,@body)))

(defvar *debug-hooks* nil)

(defstruct (debug-hook (:constructor %make-debug-hook))
  name
  file)

(defun make-debug-hook (name file)
  ;; Used from within load time value
  (or (find name *debug-hooks*
            :key #'(lambda (x) (debug-hook-name (cdr x)))
            :test #'equal)
      (car (push (cons nil (%make-debug-hook :name name :file file))
                 *debug-hooks*))))


(defun debug-show-1 (file function &rest places-and-values)
  (apply #'format *trace-output*
         "~&;; ~A:~%~@<;:: ~@;~2I~S: ~:@_~@<~@{~S = ~S~^, ~_~}~:>~:> ~%"
         file
         function
         places-and-values))

(defmacro debugf (&rest variables)
  `(when
       ;; We want this test as unobtrusive as possible, so declare CAR
       ;; inline and optimize for speed.
       (locally
           (declare (inline car)
                    (optimize (speed 3) (safety 0)))
         (car
          (the cons
               (macrolet ((frob (&environment env)
                            (print 'hi)
                            `(load-time-value (make-debug-hook
                                               ,(macroexpand '(%current-function) env)
                                               ,(macroexpand '(%current-file) env)))))
                 (frob)))))
     (debug-show-1
      (%current-file)
      (%current-function)
      ,@(mapcan (lambda (x) (list `',x x)) variables))))

(defun fuzzy-match (matcher matchee)
  (cond ((eq matcher '*) t)
        ((atom matcher) (eq matcher matchee))
        ((consp matcher)
         (loop for p in matcher
            for n in matchee do
              (when (not (fuzzy-match p n))
                (return nil))
            finally (return t)))))

(defun hook-matches-p (hook pattern)
  (setf hook (cdr hook))
  (cond ((or (stringp pattern)
             (pathnamep pattern))
         (pathname-match-p (debug-hook-file hook) pattern))
        ((symbolp pattern)
         (or (eq pattern (debug-hook-name hook))
             (and (consp (debug-hook-name hook))
                  (eq (car (debug-hook-name hook)) 'method)
                  (eq (cadr (debug-hook-name hook)) pattern))))
        ((fuzzy-match pattern (debug-hook-name hook)))))

(defun find-matching-hooks (hook-spec)
  (remove-if-not #'(lambda (h) (hook-matches-p h hook-spec))
                 *debug-hooks*))

(defun debug-1 (specs &optional (on-p t))
  (dolist (h (if (and (not on-p) (null specs))
                 *debug-hooks*
                 (reduce #'union (mapcar #'find-matching-hooks specs) :initial-value nil)))
    (unless (eq (car h) on-p)
      (setf (car h) on-p)
      (format T "~&;; ~A debugging of ~S.~%"
              (if on-p "enabling" "disabling")
              (debug-hook-name (cdr h))))))

(defmacro debug (&rest functions)
  `(debug-1 ',functions))

(defmacro undebug (&rest functions)
  `(debug-1 ',functions nil))

;;;;;

(defmethod bar ((x integer) (y integer))
  (debugf x y)
  (+ x y))

(defmethod bar ((x integer) (y string))
  (debugf x y)
  (+ x (parse-integer y)))

(defun foo (x)
  (debugf x (+ x 1))
  (debugf
   x (+ x 1) (get-universal-time)
   x (+ x 1) (get-universal-time)
   x (+ x 1) (get-universal-time)
   x (+ x 1) (get-universal-time)
   x (+ x 1) (get-universal-time)
   x (+ x 1) (get-universal-time)
   x (+ x 1) (get-universal-time)
   )
  (+ x 1))

(defun blah (x)
  (debugf x)
  (+ x 1))
