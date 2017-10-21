;; [[file:doc.org::*Маршрутизация][routes]]
(in-package #:rigidus)

(in-package :rigidus)

(restas:define-route index ("/")
  (ennobler (translate-logical-pathname "org:publish;index")))

(restas:define-route index.html ("/index.html")
  (ennobler (translate-logical-pathname "org:publish;index")))

(defmacro def/route (name param &body body)
  `(progn
     (restas:define-route ,name ,param
       ,@body)
     (restas:define-route
         ,(intern (concatenate 'string (symbol-name name) "/"))
         ,(cons (concatenate 'string (car param) "/") (cdr param))
       ,@body)
     (restas:define-route
         ,(intern (concatenate 'string (symbol-name name) ".html"))
         ,(cons (concatenate 'string (car param) ".html") (cdr param))
       ,@body)))

(def/route about ("about")
  (ennobler (translate-logical-pathname "org:publish;about")))

(restas:mount-module -doc- (#:restas.directory-publisher)
  (:url "/doc")
  (restas.directory-publisher:*directory*
   (merge-pathnames (make-pathname :directory '(:relative "repo/rigidus.ru/www/doc"))
                    (user-homedir-pathname))))

(restas:mount-module -prj- (#:restas.directory-publisher)
  (:url "/prj")
  (restas.directory-publisher:*directory*
   (merge-pathnames (make-pathname :directory '(:relative "repo/rigidus.ru/www/prj"))
                    (user-homedir-pathname))))

(restas:mount-module -lrn/asm- (#:restas.directory-publisher)
  (:url "/lrn/asm")
  (restas.directory-publisher:*directory*
   (merge-pathnames (make-pathname :directory '(:relative "repo/rigidus.ru/www/lrn/asm"))
                    (user-homedir-pathname))))
(in-package #:rigidus)

(defparameter *log-404* nil)

(defun page-404 (&optional (title "404 Not Found") (content "Страница не найдена"))
  "404 Not Found")

(restas:define-route not-found-route ("*any")
  (push any *log-404*)
  (restas:abort-route-handler
   (page-404)
   :return-code hunchentoot:+http-not-found+
   :content-type "text/html"))
(in-package #:rigidus)

(restas:define-route robots ("/robots.txt")
  (format nil "User-agent: *~%Disallow: "))

(restas:mount-module -css- (#:restas.directory-publisher)
  (:url "/css/")
  (restas.directory-publisher:*directory*
   (merge-pathnames (make-pathname :directory '(:relative "css"))
                    *base-dir*)))

(restas:mount-module -img- (#:restas.directory-publisher)
  (:url "/img/")
  (restas.directory-publisher:*directory*
   (merge-pathnames (make-pathname :directory '(:relative "img"))
                    *base-dir*)))

(restas:mount-module -js- (#:restas.directory-publisher)
  (:url "/js/")
  (restas.directory-publisher:*directory*
   (merge-pathnames (make-pathname :directory '(:relative "js"))
                    *base-dir*)))
;; routes ends here
