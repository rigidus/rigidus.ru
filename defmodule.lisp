(require 'closer-mop)
(require 'postmodern)
(require 'restas)
(require 'closure-template)
(require 'restas-directory-publisher)
(require 'anaphora)
(require 'cl-base64)


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

(let ((path '(:RELATIVE "repo/rigidus.ru")))
  (setf asdf:*central-registry*
        (remove-duplicates (append asdf:*central-registry*
                                   (list (merge-pathnames
                                          (make-pathname :directory path)
                                          (user-homedir-pathname))))
                           :test #'equal)))

(defparameter *basedir*
  (asdf:component-pathname (asdf:find-system '#:rigidus)))

(defun path (relative)
  (merge-pathnames relative *basedir*))

(closure-template:compile-template :common-lisp-backend (path "templates.htm"))

(defclass orgdata ()
  ((content    :accessor orgdata-content)
   (sections   :accessor orgdata-sections)
   (directives :accessor orgdata-directives)))

(defparameter *articles* (make-hash-table :test #'equal))
(defparameter *cached-articles-page* nil)

(defparameter *aliens* (make-hash-table :test #'equal))
(defparameter *cached-alien-page* nil)

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

(defparameter *db-name* "rigidusdb")
(defparameter *db-user* "rigidus")
(defparameter *db-pass* "rigidus1234")
(defparameter *db-serv* "localhost")
(defparameter *db-spec* (list *db-name* *db-user* *db-pass* *db-serv*))
(connect-toplevel *db-name* *db-user* *db-pass* *db-serv*)
;; (disconnect-toplevel)
(defparameter *db-connection* (connect *db-name* *db-user* *db-pass* *db-serv*))


(defmacro incrementor (name fld)
  `(let ((,(intern (format nil "INC-~A-~A" (symbol-name name) (symbol-name fld))) 0))
     (list
      (defun ,(intern (format nil "INCF-~A-~A" (symbol-name name) (symbol-name fld)())) ()
        (incf ,(intern (format nil "INC-~A-~A" (symbol-name name) (symbol-name fld)))))
      (defun ,(intern (format nil "INIT-~A-~A" (symbol-name name) (symbol-name fld) ())) (init-value)
        (setf ,(intern (format nil "INC-~A-~A" (symbol-name name) (symbol-name fld))) init-value)))))


(progn
  (incrementor comment id)
  (defclass comment () ;; definition of COMMENT
    ((id  :col-type integer :initarg :id  :initform (incf-product-id) :accessor id)
     (key :col-type string  :initarg :key :initform "" :accessor key)
     (msg :col-type string  :initarg :msg :initform "" :accessor msg))
    (:metaclass dao-class)
    (:keys id))
  (unless (table-exists-p "comment") ;; create table COMMENT if not exists
    (with-connection (list *db-name* *db-user* *db-pass* *db-serv*)
      (query (sql (:drop-table :if-exists 'comment)))
      (execute (dao-table-definition 'comment)))))
