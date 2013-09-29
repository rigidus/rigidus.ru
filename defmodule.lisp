(defpackage #:rigidus
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

(defclass orgdata ()
  ((content    :accessor orgdata-content)
   (sections   :accessor orgdata-sections)
   (directives :accessor orgdata-directives)))

(defmethod render ((data list))
  (destructuring-bind (headtitle navpoints content) data
    (tpl:root (list :headtitle headtitle
                    :content (tpl:base (list :navpoints navpoints
                                             :content content
                                             :stat (tpl:stat)))))))

(defmethod render ((file pathname))
  (if (string= (pathname-type file) "org")
      (render (parse-org file))
      (call-next-method)))

(defmethod render ((data orgdata))
  (let* ((content     (concatenate 'string (orgdata-content data)))
         (sections    (orgdata-sections data))
         (directives  (orgdata-directives data))
         (title       (getf directives :title))
         (menu-memo   (menu)))
    (render
     (list title
           menu-memo
           (tpl:default
               (list :title title
                     :navpoints menu-memo
                     :sections (iter (for i from 1)
                                     (for section in sections)
                                     (collect (list :anchor (format nil "anchor-~a" i)
                                                    :level (format nil "level-~a" (car section))
                                                    :title (cadr section))))
                     :links ""
                     :content content))))))

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

(defmacro bprint (var)
  `(subseq (with-output-to-string (*standard-output*)
             (pprint ,var)) 1))

(defmacro err (var)
  `(error (format nil "ERR:[~A]" (bprint ,var))))


(setf asdf:*central-registry*
      (remove-duplicates (append asdf:*central-registry*
                                 (list (make-pathname :directory (list :relative (sb-posix:getcwd)))))
                         :test #'equal))

(defparameter *basedir* (make-pathname :directory (list :relative (sb-posix:getcwd))))

(defun path (relative)
  (merge-pathnames relative *basedir*))

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
