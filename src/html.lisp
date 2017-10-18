;; [[file:doc.org::*DSL для построения HTML][html]]
(in-package :rigidus)

(defun stringify (&rest things)
  "Like concatenate but prints all of its arguments."
  (format nil "~{~A~}" things))
(in-package :rigidus)

(defun rn ()
  (string #\Newline))

(defun idx (idx)
  (make-string (* 2 idx) :initial-element #\Space))
(in-package :rigidus)

(defvar *ps-html-empty-tag-aware-p* t)

(defvar *html-empty-tags* '(:area :atop :audioscope :base :basefont :br :choose :col :frame
                            :hr :img :input :isindex :keygen :left :limittext :link :meta
                            :nextid :of :over :param :range :right :spacer :spot :tab :wbr))

(defun empty-tag-p (tag)
  (and *ps-html-empty-tag-aware-p*
       (member tag *html-empty-tags*)))
(in-package :rigidus)

(defun concat-constant-strings (str-list)
  (flet ((expand (expr)
           ;; (setf expr (ps-macroexpand expr))
           ;; (cond ((and (consp expr) (eq (car expr) 'quote) (symbolp (second expr)))
           ;;        (symbol-to-js-string (second expr)))
           ;;       ((keywordp expr) (string-downcase expr))
           ;;       ((characterp expr) (string expr))
           ;;       (t expr))
           expr
           ))
    (reverse (reduce (lambda (optimized-list next-expr)
                       (let ((next-obj (expand next-expr)))
                         (if (and (or (numberp next-obj) (stringp next-obj))
                                  (stringp (car optimized-list)))
                             (cons (format nil "~a~a" (car optimized-list) next-obj) (cdr optimized-list))
                             (cons next-obj optimized-list))))
                     (cons () str-list)))))
(in-package :rigidus)

(defparameter *ps-html-mode* :sgml "One of :sgml or :xml")

(in-package #:rigidus)

(defun process-html-forms-lhtml (idx forms)
  (let ((r) (idx idx))
    (labels ((process-attrs (attrs)
               (do (attr-test attr-name attr-val)
                   ((not attrs))
                 (setf attr-name (pop attrs)
                       attr-test (when (not (keywordp attr-name))
                                   (let ((test attr-name))
                                     (setf attr-name (pop attrs))
                                     test))
                       attr-val (pop attrs))
                 (if attr-test
                     (push `(if ,attr-test
                                (stringify ,(format nil " ~A=\"" attr-name) ,attr-val "\"")
                                "")
                           r)
                     (progn
                       (push (format nil " ~A=\"" attr-name) r)
                       (push attr-val r)
                       (push "\"" r)))))
             (process-form% (tag attrs content)
               (push `(idx ,idx) r)
               (push (format nil "<~A" tag) r)
               (process-attrs attrs)
               (if (or content (not (empty-tag-p tag)))
                   (progn (push ">" r)
                          (push '(rn) r)
                          (incf idx)
                          (map nil #'process-form content)
                          (decf idx)
                          (push `(idx ,idx) r)
                          (push (format nil "</~A>" tag) r)
                          (push '(rn) r))
                   (progn (when (eql *ps-html-mode* :xml)
                            (push "/" r))
                          (push ">" r)
                          (push '(rn) r))))
             (process-form (form)
               (cond ((keywordp form)
                      (process-form (list form)))
                     ((atom form)
                      (progn
                        (push `(idx ,idx) r)
                        (push form r)
                        (push '(rn) r)))
                     ((and (consp form) (keywordp (car form)))
                      (process-form% (car form) () (cdr form)))
                     ((and (consp form) (consp (first form)) (keywordp (caar form)))
                      (process-form% (caar form) (cdar form) (cdr form)))
                     (t (progn
                          (push `(idx ,idx) r)
                          (push form r)
                          (push '(rn) r)
                          )))))
      (map nil #'process-form forms)
      (concat-constant-strings (reverse r)))))

(in-package #:rigidus)

(defmacro html (idx &rest html-forms)
  `(stringify ,@(with-standard-io-syntax (process-html-forms-lhtml idx html-forms))))
;; html ends here
