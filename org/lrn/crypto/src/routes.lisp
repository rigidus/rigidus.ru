;; [[file:~/src/rigidus.ru/org/lrn/crypto/mycoin.org::routes][routes]]
;;;; Copyright © 2014-2018 Glukhov Mikhail. All rights reserved.
;;;; Licensed under the GNU AGPLv3
(in-package #:mycoin)

(in-package #:mycoin)

;; (restas:mount-module -css- (#:restas.directory-publisher)
;;   (:url "/css/")
;;   (restas.directory-publisher:*directory*
;;    (merge-pathnames (make-pathname :directory '(:relative "css"))
;;                     *base-dir*)))

;; (restas:mount-module -img- (#:restas.directory-publisher)
;;   (:url "/img/")
;;   (restas.directory-publisher:*directory*
;;    (merge-pathnames (make-pathname :directory '(:relative "img"))
;;                     *base-dir*)))

;; (restas:mount-module -js- (#:restas.directory-publisher)
;;   (:url "/js/")
;;   (restas.directory-publisher:*directory*
;;    (merge-pathnames (make-pathname :directory '(:relative "js"))
;;                     *base-dir*)))

;; (restas:mount-module -resources- (#:restas.directory-publisher)
;;   (:url "/resources")
;;   (restas.directory-publisher:*directory*
;;    (merge-pathnames (make-pathname :directory '(:relative "resources"))
;;                     *base-dir*)))
(in-package #:mycoin)

(defparameter *log-404* nil)

(defun page-404 (&optional (title "404 Not Found") (content "Страница не найдена"))
  "404 Not Found")

(restas:define-route not-found-route ("*any")
  (push any *log-404*)
  (restas:abort-route-handler
   (page-404)
   :return-code hunchentoot:+http-not-found+
   :content-type "text/html"))
(in-package #:mycoin)

(restas:define-route robots ("/robots.txt")
  (format nil "User-agent: *~%Disallow: "))
(in-package :mycoin)

;; (restas:mount-module -base- (#:restas.directory-publisher)
;;   (:url "/")
;;   (:render-method (make-instance 'orgmode-handler))
;;   (restas.directory-publisher:*directory*
;;    (merge-pathnames (make-pathname :directory '(:relative "www"))
;;                     *base-dir*)))

(restas:mount-module -doc- (#:restas.directory-publisher)
  (:url "/doc")
  (:render-method (make-instance 'orgmode-handler))
  (restas.directory-publisher:*directory*
   (merge-pathnames (make-pathname :directory '(:relative "www/doc"))
                    *base-dir*)))
(in-package :mycoin)

(restas:define-route index ("/")
  ;; (enobler (translate-logical-pathname "org:publish;index"))
  "mainpage"
  )

;; (restas:define-route index.html ("/index.html")
;;   (enobler (translate-logical-pathname "org:publish;index")))

;; (defmacro def/route (name param &body body)
;;   `(progn
;;      (restas:define-route ,name ,param
;;        ,@body)
;;      (restas:define-route
;;          ,(intern (concatenate 'string (symbol-name name) "/"))
;;          ,(cons (concatenate 'string (car param) "/") (cdr param))
;;        ,@body)
;;      (restas:define-route
;;          ,(intern (concatenate 'string (symbol-name name) ".html"))
;;          ,(cons (concatenate 'string (car param) ".html") (cdr param))
;;        ,@body)))

;; (def/route research ("research")
;;   (enobler (translate-logical-pathname "org:publish;research")))

;; (def/route slides ("slides")
;;   (enobler (translate-logical-pathname "org:publish;slides")))

;; (def/route projects ("projects")
;;   (enobler (translate-logical-pathname "org:publish;projects")))

(in-package #:mycoin)

(restas:define-route who-are-you ("/who-are-you")
  (format nil "MyCoin Node #~A" *node-port*))
;; routes ends here
