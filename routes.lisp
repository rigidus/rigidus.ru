;; [[file:doc.org::*Routing][routes]]
(in-package #:rigidus)

(in-package :rigidus)

(restas:define-route main ("/")
  (base-page *head-title*
             "Rigidus homepage"
             *menu*))
(in-package #:rigidus)

(defparameter *log-404* nil)

(defun page-404 (&optional (title "404 Not Found") (content "Страница не найдена"))
  (let* ((title "404 Not Found"))
    (base-page *head-title*
               title
               *menu*)))

(restas:define-route not-found-route ("*any")
  (push any *log-404*)
  (restas:abort-route-handler
   (page-404)
   :return-code hunchentoot:+http-not-found+
   :content-type "text/html"))
(in-package #:rigidus)

(restas:define-route robots ("/robots.txt")
  (format nil "User-agent: *~%Disallow: "))
;; routes ends here
