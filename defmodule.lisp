(require 'restas)
(require 'closure-template)
(require 'restas-directory-publisher)
(require 'cl-base64)

(restas:define-module #:rigidus
    (:use #:cl #:iter #:alexandria))

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
  ((content :accessor orgdata-content)
   (sections :accessor orgdata-sections)
   (directives :accessor orgdata-directives)))

(defparameter *articles* (make-hash-table :test #'equal))
(defparameter *cached-articles-page* nil)

(defparameter *aliens* (make-hash-table :test #'equal))
(defparameter *cached-alien-page* nil)
