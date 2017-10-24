;; [[file:doc.org::*Определение модуля][defmodule]]
;;;; Copyright © 2014-2017 Glukhov Mikhail. All rights reserved.
;;;; Licensed under the GNU AGPLv3
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

;; special syntax for pattern-matching - ON
(named-readtables:in-readtable :fare-quasiquote)

;; Здесь подключаются утилиты
(in-package :rigidus)

(defmacro bprint (var)
  `(subseq (with-output-to-string (*standard-output*)
             (pprint ,var)) 1))

(defmacro err (var)
  `(error (format nil "ERR:[~A]" (bprint ,var))))

(define-condition pattern-not-found-error (error)
  ((text :initarg :text :reader text)))

(defun extract (cortege html)
  (loop :for (begin end regexp) :in cortege :collect
     (multiple-value-bind (start fin)
         (ppcre:scan regexp html)
       (when (null start)
         (error 'pattern-not-found-error :text regexp))
       (subseq html (+ start begin) (- fin end)))))

(defun get-directory-contents (path)
  "Функция возвращает содержимое каталога"
  (when (not (equal "/" (coerce (last (coerce path 'list)) 'string)))
    (setf path (format nil "~A/" path)))
  (directory (format nil "~A*.*" path)))

(defun maptree-transform (predicate-transformer tree)
  (multiple-value-bind (t-tree control)
      (aif (funcall predicate-transformer tree)
           it
           (values tree #'mapcar))
    (if (and (consp t-tree)
             control)
        (funcall control
                 #'(lambda (x)
                     (maptree-transform predicate-transformer x))
                 t-tree)
        t-tree)))

;; mtm - синтаксический сахар для maptree-transform
(defmacro mtm (transformer tree)
  (let ((lambda-param (gensym)))
    `(maptree-transform #'(lambda (,lambda-param)
                            (values (optima:match ,lambda-param ,transformer)
                                    #'mapcar))
                        ,tree)))


;; Механизм трансляции путей
(in-package :rigidus)

(defparameter *base-dir*
  (merge-pathnames
   (make-pathname :directory '(:relative "repo/rigidus.ru"))
   (user-homedir-pathname)))

(defparameter *base-path* (directory-namestring *base-dir*))

(setf (logical-pathname-translations "org")
      `(("source;*.*"
         ,(concatenate 'string *base-path* "org/*.org"))
        ("publish;*.*"
         ,(concatenate 'string *base-path* "www/*.html"))))

;; (translate-logical-pathname "org:source;articles;about.txt")
;; ;; #P"/home/rigidus/repo/rigidus.ru/org/articles/about.org"
;; (translate-logical-pathname "org:source;articles;emacs;about.txt")
;; ;; #P"/home/rigidus/repo/rigidus.ru/org/articles/emacs/about.org"
;; (translate-logical-pathname "org:publish;articles;about.txt")
;; ;; #P"/home/rigidus/repo/rigidus.ru/www/articles/about.org"
;; (translate-logical-pathname "org:publish;articles;emacs;about.txt")
;; ;; #P"/home/rigidus/repo/rigidus.ru/www/articles/emacs/about.org"

(closure-template:compile-template
 :common-lisp-backend (merge-pathnames
                       (make-pathname :name "templates" :type "htm")
                       (merge-pathnames
                        (make-pathname :directory '(:relative "src"))
                        *base-dir*)))

;; Механизм преобразования страниц
(in-package :rigidus)

(defun html-to-tree (html)
  ;; (html5-parser:node-to-xmls
  (html5-parser:parse-html5-fragment html :dom :xmls))

(defun tree-to-html (tree &optional (step 0))
  (macrolet ((indent () `(make-string (* 3 step) :initial-element #\Space)))
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

(defun ennobler (pathname &optional dbg)
  (let* ((file-contents (alexandria:read-file-into-string pathname))
         (onestring (cl-ppcre:regex-replace-all "(\\n|\\s*$)" file-contents (if dbg "" " ")))
         (tree (html-to-tree onestring))
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
         (result tree))
    (if dbg
        result
        (format nil "<!DOCTYPE html>~%~A~%~A"
                (tree-to-html result)
                (tpl:stat)))))
;; defmodule ends here
