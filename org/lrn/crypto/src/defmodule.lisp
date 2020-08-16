;; [[file:~/src/rigidus.ru/org/lrn/crypto/mycoin.org::defmodule][defmodule]]
;;;; Copyright © 2014-2018 Glukhov Mikhail. All rights reserved.
;;;; Licensed under the GNU AGPLv3
(restas:define-module #:mycoin
  (:use #:closer-mop #:cl #:iter #:alexandria #:anaphora #:postmodern)
  (:shadowing-import-from :closer-mop
                          :defclass
                          :defmethod
                          :standard-class
                          :ensure-generic-function
                          :defgeneric
                          :standard-generic-function
                          :class-name))

(in-package #:mycoin)

;; special syntax for pattern-matching - ON
(named-readtables:in-readtable :fare-quasiquote)

;; Подключение к базе данных PostgreSQL
(defvar *db-name* "mycoin")
(defvar *db-user* "crypto")
(defvar *db-pass* "9Jb17sqGQtZb927hRp37Hbspba7p34L")
(defvar *db-serv* "localhost")

(defvar *db-spec* (list *db-name* *db-user* *db-pass* *db-serv*))

;; Здесь подключаются утилиты
(in-package :mycoin)

(define-condition pattern-not-found-error (error)
  ((text :initarg :text :reader text)))

(defun get-directory-contents (path)
  "Функция возвращает содержимое каталога"
  (when (not (equal "/" (coerce (last (coerce path 'list)) 'string)))
    (setf path (format nil "~A/" path)))
  (directory (format nil "~A*.*" path)))

;; Превращает инициализированные поля объекта в plist
(defun get-obj-data (obj)
  (let ((class (find-class (type-of obj)))
        (result))
    (loop :for slot :in (closer-mop:class-direct-slots class) :collect
       (let ((slot-name (closer-mop:slot-definition-name slot)))
         (when (slot-boundp obj slot-name)
           (setf result
                 (append result (list (intern (symbol-name slot-name) :keyword)
                                      (funcall slot-name obj)))))))
    result))

;; Assembly WHERE clause
(defun make-clause-list (glob-rel rel args)
  (append (list glob-rel)
          (loop
             :for i
             :in args
             :when (and (symbolp i)
                        (getf args i)
                        (not (symbolp (getf args i))))
             :collect (list rel i (getf args i)))))

;; Макросы для корректного вывода ошибок
(defmacro bprint (var)
  `(subseq (with-output-to-string (*standard-output*)  (pprint ,var)) 1))

(defmacro err (var)
  `(error (format nil "ERR:[~A]" (bprint ,var))))

;; Отладочный вывод
(defparameter *dbg-enable* t)
(defparameter *dbg-indent* 1)

(defun dbgout (out)
  (when *dbg-enable*
    (format t (format nil "~~%~~~AT~~A" *dbg-indent*) out)))

(defmacro dbg (frmt &rest params)
  `(dbgout (format nil ,frmt ,@params)))

;; (macroexpand-1 '(dbg "~A~A~{~A~^,~}" "zzz" "34234" '(1 2 3 4)))

(defun anything-to-keyword (item)
  (intern (string-upcase (format nil "~a" item)) :keyword))

(defun alist-to-plist (alist)
  (if (not (equal (type-of alist) 'cons))
      alist
      ;;else
      (loop
         :for (key . value)
         :in alist
         :nconc (list (anything-to-keyword key) value))))

;; Чтобы выводить коллекции напишем макрос
(defmacro with-collection ((item collection) &body body)
  `(loop :for ,item :in ,collection :collect
      ,@body))

;; Чтобы выводить элемент коллекции напишем макрос
(defmacro with-element ((item elt) &body body)
  `(let ((,item ,elt))
     (list
      ,@body)))

(defun replace-all (string part replacement &key (test #'char=))
  "Returns a new string in which all the occurences of the part
         is replaced with replacement."
  (with-output-to-string (out)
    (loop with part-length = (length part)
       for old-pos = 0 then (+ pos part-length)
       for pos = (search part string
                         :start2 old-pos
                         :test test)
       do (write-string string out
                        :start old-pos
                        :end (or pos (length string)))
       when pos do (write-string replacement out)
       while pos)))

(defun explore-dir (path)
  (let ((raw (directory path))
        (dirs)
        (files))
    (mapcar #'(lambda (x)
                (if (cl-fad:directory-pathname-p x)
                    (push x dirs)
                    (push x files)))
            raw)
    (values dirs files raw)))

;; clear-db
(defun drop (tbl-lst)
  (let ((tables tbl-lst))
    (flet ((rmtbl (tblname)
             (when (with-connection *db-spec*
                     (query (:select 'table_name :from 'information_schema.tables :where
                                     (:and (:= 'table_schema "public")
                                           (:= 'table_name tblname)))))
               (with-connection *db-spec*
                 (query (:drop-table (intern (string-upcase tblname))))))))
      (loop :for tblname :in tables :collect
         (rmtbl tblname)))))

;; contains
(defun contains (string pattern)
  (if (search pattern string)
      t))

;; empty
(defun empty (string)
  (if (or (null string)
          (equal "" string))
      t))

;; Механизм трансляции путей
(in-package :mycoin)

(defparameter *base-dir*
  (merge-pathnames
   (make-pathname :directory '(:relative "repo/rigidus.ru/org/lrn/crypto"))
   (user-homedir-pathname)))

(defparameter *base-path* (directory-namestring *base-dir*))

;; (setf (logical-pathname-translations "org")
;;       `(("source;*.*"
;;          ,(concatenate 'string *base-path* "org/*.org"))
;;         ("publish;*.*"
;;          ,(concatenate 'string *base-path* "www/*.html"))))

;; (translate-logical-pathname "org:source;articles;about.txt")
;; ;; #P"/home/rigidus/repo/rigidus.ru/org/articles/about.org"
;; (translate-logical-pathname "org:source;articles;emacs;about.txt")
;; ;; #P"/home/rigidus/repo/rigidus.ru/org/articles/emacs/about.org"
;; (translate-logical-pathname "org:publish;articles;about.txt")
;; ;; #P"/home/rigidus/repo/rigidus.ru/www/articles/about.org"
;; (translate-logical-pathname "org:publish;articles;emacs;about.txt")
;; ;; #P"/home/rigidus/repo/rigidus.ru/www/articles/emacs/about.org"

;; Работа с html tree
(in-package :mycoin)

(in-package :mycoin)

(defun html-to-tree (html)
  ;; (html5-parser:node-to-xmls
  (html5-parser:parse-html5-fragment html :dom :xmls))
(in-package :mycoin)

(defun tree-to-html (tree &optional (step 0))
  (macrolet ((indent ()
               `(make-string (* 3 step) :initial-element #\Space)))
    (labels ((paired (subtree)
               (format nil "~A<~A~A>~%~A~4:*~A</~A>~%"
                       (indent)
                       (car subtree)
                       (format nil "~:[~; ~1:*~{~A~^ ~}~]"
                               (mapcar #'(lambda (attr)
                                           (let ((key (car attr))
                                                 (val (cadr attr)))
                                             (format nil "~A=\"~A\"" key val)))
                                       (cadr subtree)))
                       (format nil "~{~A~}"
                               (progn
                                 (incf step)
                                 (let ((ret (mapcar #'(lambda (x)
                                                        (subtree-to-html x step))
                                                    (cddr subtree))))
                                   (decf step)
                                   ret)))))
             (singled (subtree)
               (format nil "~A<~A~A />~%"
                       (indent)
                       (car subtree)
                       (format nil "~:[~; ~1:*~{~A~^ ~}~]"
                               (mapcar #'(lambda (attr)
                                           (let ((key (car attr))
                                                 (val (cadr attr)))
                                             (format nil "~A=\"~A\"" key val)))
                                       (cadr subtree)))))
             (subtree-to-html (subtree &optional (step 0))
               (cond ((stringp subtree) (format nil "~A~A~%" (indent) subtree))
                     ((numberp subtree) (format nil "~A~A~%" (indent) subtree))
                     ((listp   subtree)
                      (let ((tag (car subtree)))
                        (cond ((or (equal tag "img")
                                   (equal tag "link")
                                   (equal tag "meta"))  (singled subtree))
                              (t (paired subtree)))))
                     (t (format nil "[:err:~A]" subtree)))))
      (reduce #'(lambda (a b) (concatenate 'string a b))
              (mapcar #'(lambda (x) (subtree-to-html x step))
                      tree)))))

;; Механизм преобразования страниц
(in-package :mycoin)

(defun enobler (pathname &optional dbg)
  (let* ((file-contents (alexandria:read-file-into-string pathname))
         (onestring (cl-ppcre:regex-replace-all "(\\n|\\s*$)" file-contents (if dbg "" " ")))
         ;; (tree (html-to-tree onestring))
         ;; (inject-css '("link" (("href" "/css/style.css") ("rel" "stylesheet") ("type" "text/css"))))
         ;; (replace-css #'(lambda (in)
         ;;                  (optima:match in
         ;;                    (`("style" (("type" "text/css")) ,_) inject-css))))
         ;; (remove-css (maptree-transform replace-css tree))
         ;; (inject-js '("script" (("src" "scripts.js"))))
         ;; (replace-js  #'(lambda (in)
         ;;                  (optima:match in
         ;;                    (`("script" (("type" "text/javascript")) ,_) inject-js))))
         ;; (remove-js (maptree-transform replace-js remove-css))
         ;; (result tree)
    ;; (if dbg
    ;;     result
    ;;     (format nil "~A~A~%~A~%~A"
    ;;             ;; "<!DOCTYPE html>\n"
    ;;             ""
    ;;             ;; (tree-to-html result)
    ;;             file-contents
    ;;             (tpl:stat)
    ;;             "  <div id=\"linker\"><a href=\"/\">Home</a></div>"
              )
    onestring
    ))

;; Читаем и применяем конфиг
(load
 (make-pathname :directory (sb-posix:getcwd)
                :name "node"
                :type "cfg"))
;; defmodule ends here
