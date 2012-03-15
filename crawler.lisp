(setf asdf:*central-registry*
      (remove-duplicates (append asdf:*central-registry*
                                 (list (merge-pathnames
                                        (make-pathname :directory '(:relative "repo/puri-unicode"))
                                        (user-homedir-pathname))))
                         :test #'equal))
(require 'puri)

(require 'cl-mysql)
(require 'cl-fad)
(require 'cl-store)
(require 'restas)
(require 'restas-directory-publisher)
(require 'cl-json)
(require 'cl-ppcre)
(require 'cl-smtp)
(require 'cl-mime)
(require 'arnesi)
(require 'closer-mop)
(require 'drakma)


(restas:define-module #:WIZARD
    (:use #:cl #:iter #:asdf #:cl-mysql #:alexandria #:anaphora #:ppcre)
  (:import-from #:cl-fad #:pathname-as-directory))

(in-package #:wizard)

(asdf:oos 'asdf:load-op :drakma)
(asdf:oos 'asdf:load-op :cl-html-parse)

(defun extract-links (param &optional (acc nil))
  (loop :for elt :in param :do
     (cond ((equal 'cons (type-of elt))
            (setf acc (extract-links elt acc)))
           (t (let (pos)
                (when (and (equal :a elt)
                           (setf pos (position :href param))
                           (nth (setf pos (+ 1 pos)) param))
                  (push (nth pos param) acc))))))
  acc)

(defun uri-to-str (uri)
  (string-downcase
   (format nil "~@[~a~]://~@[~a~]~@[~a~]" (puri:uri-scheme uri) (puri:uri-host uri) (puri:uri-path uri))))

(defclass page ()
  ((reflist          :initarg :reflist                  :initform nil       :accessor reflist)
   (body-or-stream   :initarg :body-or-stream           :initform nil       :accessor body-or-stream)
   (status-code      :initarg :status-code              :initform nil       :accessor status-code)
   (headers          :initarg :headers                  :initform nil       :accessor headers)
   (uri              :initarg :uri                      :initform nil       :accessor uri)
   (stream-out       :initarg :stream-out               :initform nil       :accessor stream-out)
   (must-close       :initarg :must-close               :initform nil       :accessor must-close)
   (reason-phrase    :initarg :reason-phrase            :initform nil       :accessor reason-phrase)))

(defun process-page (uri-str)
  (format t "~%=[~A]=[~A]" uri-str (hash-table-count *pgs*))
  (multiple-value-bind (body-or-stream status-code headers uri stream-out must-close reason-phrase)
      (drakma:http-request uri-str :user-agent "Mozilla/5.0 (X11; U; Linux i686; ru; rv:1.9.2.13) Gecko/20101206 Ubuntu/10.04 (lucid) Firefox/3.6.13")
    (setf (gethash uri-str *pgs*)
          (make-instance 'page
                         :body-or-stream  body-or-stream
                         :status-code     status-code
                         :headers         headers
                         :uri             uri
                         :stream-out      stream-out
                         :must-close      must-close
                         :reason-phrase   reason-phrase))
    (if (equal 200 status-code)
        (if (not (equal 0 (search "text/html" (cdr (assoc :content-type headers)))))
            (format t "~%WARN! headers: ~A" headers)
            ;; else
            (progn
              (format t "~%200 OK : ~A" uri-str)
              (loop
                 :for  lnk
                 :in   (remove-duplicates (extract-links (html-parse:parse-html body-or-stream)))
                 :with svd = (let ((svd))
                               (maphash #'(lambda (k v)
                                            (when (status-code v)
                                              (push k svd)))
                                        *pgs*)
                               svd)
                 :with uri-lnk
                 :when (progn
                         (setf uri-lnk (puri:merge-uris lnk (puri:parse-uri uri-str)))
                         (unless (puri:uri-host uri-lnk)
                           (setf uri-lnk (puri:merge-uris uri-lnk (puri:parse-uri (format nil "http://~A" *target-host*)))))
                         (unless (puri:uri-scheme uri-lnk)
                           (setf (puri:uri-scheme uri-lnk) :http))
                         (and
                          (equal 0 (search (format nil "http://~A" *target-host*)
                                           (uri-to-str uri-lnk)))
                          (not (find (uri-to-str uri-lnk) svd :test #'equal))))
                 :do
                 (format t "~%| ~A" (uri-to-str uri-lnk))
                 (setf (gethash (uri-to-str uri-lnk) *pgs*) (make-instance 'page))
                 )))
        ;; else (not 200 OK)
        (format t "~% ~A ~A : ~A" status-code reason-phrase uri-str)
        )))

(defun crawler (target-host)
  (let ((*target-host* target-host)
        (*pgs* (make-hash-table :test #'equal)))
    (declare (special *target-host* *pgs*))
    (setf (gethash (format nil "http://~A" *target-host*) *pgs*)
          (make-instance 'page))
    (tagbody
     re
       (let ((pgs *pgs*)
             (cur))
         (block fnd
           (maphash #'(lambda (k v)
                        (unless (status-code v)
                          (setf cur k)
                          (return-from fnd)))
                    pgs))
         (when cur
           (process-page cur)
           (go re))))
    *pgs*))

(defparameter *p* (crawler "rigidus.ru"))
(extract-links (html-parse:parse-html (body-or-stream (gethash (car (hash-table-keys *p*)) *p*))))



;; (html-parse:parse-html (drakma:http-request "http://www.320-8080.ru" :user-agent "Mozilla/5.0 (X11; U; Linux i686; ru; rv:1.9.2.13) Gecko/20101206 Ubuntu/10.04 (lucid) Firefox/3.6.13"))


;; (maphash #'(lambda (k v)
;;              (format nil "~%~A:~A | " k (status-code v) (body-or-stream v)))
;;          (crawler "www.320-8080.ru"))

;; (defparameter *p* (crawler "www.320-8080.ru"))
;; (hash-table-keys
