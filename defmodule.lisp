;; [[file:doc.org::*Определения модуля][defmodule]]
(restas:define-module #:rigidus
  (:use #:closer-mop #:cl #:iter #:alexandria #:anaphora #:postmodern)
  (:shadowing-import-from :closer-mop
                          :defclass
                          :defmethod
                          :standard-class
                          :ensure-generic-function
                          :defgeneric
                          :standard-generic-function
                          :class-name))

(in-package #:rigidus)

(in-package :rigidus)

(defmacro bprint (var)
  `(subseq (with-output-to-string (*standard-output*)
             (pprint ,var)) 1))

(defmacro err (var)
  `(error (format nil "ERR:[~A]" (bprint ,var))))

(defmacro def/route (name param &body body)
  `(progn
     (restas:define-route ,name ,param
       ,@body)
     (restas:define-route
         ,(intern (concatenate 'string (symbol-name name) "/"))
         ,(cons (concatenate 'string (car param) "/") (cdr param))
       ,@body)))

(define-condition pattern-not-found-error (error)
  ((text :initarg :text :reader text)))

(defun extract (cortege html)
  (loop :for (begin end regexp) :in cortege :collect
     (multiple-value-bind (start fin)
         (ppcre:scan regexp html)
       (when (null start)
         (error 'pattern-not-found-error :text regexp))
       (subseq html (+ start begin) (- fin end)))))

(in-package #:rigidus)

(defparameter *base-dir* (sb-posix:getcwd))

(setf (logical-pathname-translations "org")
      `(("source;*.*"
         ,(concatenate 'string *base-dir* "/org/*.org"))
        ("publish;*.*"
         ,(concatenate 'string *base-dir* "/public_html/*.html"))
        ("source;articles;*.*"
         ,(concatenate 'string *base-dir* "/org/articles/**/*.org"))
        ("source;articles;*;*.*"
         ,(concatenate 'string *base-dir* "/org/articles/*/*.org"))
        ("publish;articles;*.*"
         ,(concatenate 'string *base-dir* "/public_html/articles/**/*.org"))
        ("publish;articles;*;*.*"
         ,(concatenate 'string *base-dir* "/public_html/articles/*/*.org"))
        ))

;; (translate-logical-pathname "org:source;articles;about.txt")
;; (translate-logical-pathname "org:source;articles;emacs;about.txt")

;; (translate-logical-pathname "org:publish;articles;about.txt")
;; (translate-logical-pathname "org:publish;articles;emacs;about.txt")
(in-package #:rigidus)

(defparameter *menu*
  (let* ((2th-level-org-files  (directory (translate-logical-pathname "org:source;*.org")))
         (2th-level-html  (directory-namestring (translate-logical-pathname "org:publish;")))
         (data (mapcar #'(lambda (pathname)
                           (list (pathname-name pathname)
                                 (extract '((4 5 "<h1>.*</h1>")
                                            (7 7 "<order>.*</order"))
                                          (alexandria:read-file-into-string
                                           (merge-pathnames pathname 2th-level-html)))))
                       2th-level-org-files))
         (sorted (sort data #'(lambda (a b)
                                (< (parse-integer (cadadr a))
                                   (parse-integer (cadadr b)))))))
    (mapcar #'(lambda (x)
                (cons (car x) (caadr x)))
            sorted)))



(setf asdf:*central-registry*
      (remove-duplicates (append asdf:*central-registry*
                                 (list (make-pathname :directory (list :relative (sb-posix:getcwd)))))
                         :test #'equal))

(defparameter *basedir* (make-pathname :directory (list :relative (sb-posix:getcwd))))

(defun path (relative)
  (merge-pathnames relative *basedir*))


#| POSTGRESQL
вставить в /etc/postgresql/<version>/main/pg_hba.conf
local all all trust
чтобы он доверял локальным пользователям
потом переключаемся в пользователя postgres и создаем базу
createuser -DRS <dbuser>
createdb -l ru_RU.UTF-8 -T template0 -O <dbuser> <dbname>
psql
alter user <dbuser> with password '<dbpassword>';
|#

;; (defparameter *db-name* "rigidusdb")
;; (defparameter *db-user* "rigidus")
;; (defparameter *db-pass* "rigidus1234")
;; (defparameter *db-serv* "localhost")
;; (defparameter *db-spec* (list *db-name* *db-user* *db-pass* *db-serv*))
;; (connect-toplevel *db-name* *db-user* *db-pass* *db-serv*)
;; (disconnect-toplevel)
;; (defparameter *db-connection* (connect *db-name* *db-user* *db-pass* *db-serv*))


;; (defmacro incrementor (name fld)
;;   `(let ((,(intern (format nil "INC-~A-~A" (symbol-name name) (symbol-name fld))) 0))
;;      (list
;;       (defun ,(intern (format nil "INCF-~A-~A" (symbol-name name) (symbol-name fld)())) ()
;;         (incf ,(intern (format nil "INC-~A-~A" (symbol-name name) (symbol-name fld)))))
;;       (defun ,(intern (format nil "INIT-~A-~A" (symbol-name name) (symbol-name fld) ())) (init-value)
;;         (setf ,(intern (format nil "INC-~A-~A" (symbol-name name) (symbol-name fld))) init-value)))))


;; (progn
;;   (incrementor comment id)
;;   (defclass comment () ;; definition of COMMENT
;;     ((id     :col-type integer    :initarg :id     :initform (incf-comment-id) :accessor id)
;;      (key    :col-type string     :initarg :key    :initform ""  :accessor key)
;;      (parent :col-type integer    :initarg :parent :initform ""  :accessor parent)
;;      (msg    :col-type string     :initarg :msg    :initform ""  :accessor msg)
;;      (childs                      :initarg :childs :initform nil :accessor childs))
;;     (:metaclass dao-class)
;;     (:keys id))
;;   ;; (unless (table-exists-p "comment") ;; create table COMMENT if not exists
;;     (with-connection (list *db-name* *db-user* *db-pass* *db-serv*)
;;       (query (sql (:drop-table :if-exists 'comment)))
;;       (execute (dao-table-definition 'comment))))
;; ;; )

;; (progn
;;   (let ((a (make-dao 'comment :key "TEST" :parent 0 :msg "first comment")))
;;     (make-dao 'comment :key "TEST" :parent (id a) :msg "second comment"))
;;   (let ((a (make-dao 'comment :key "TEST" :parent 0 :msg "third comment")))
;;     (make-dao 'comment :key "TEST" :parent (id a) :msg "parent comment 1")
;;     (let ((b (make-dao 'comment :key "TEST" :parent (id a) :msg "parent comment 2")))
;;       (make-dao 'comment :key "TEST" :parent (id b) :msg "sub parent comment 2"))))
;; defmodule ends here
