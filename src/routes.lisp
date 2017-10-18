;; [[file:doc.org::*Публикация и Routing][routes]]
(in-package #:rigidus)

(in-package #:rigidus)

(defmacro def/route/2th_level ()
  `(progn
     ,@(mapcar
        #'(lambda (key)
            (let ((key (car key)))
              `(def/route ,(intern (string-upcase key)) (,key)
                 (base-page *head-title* "Rigidus homepage" *menu*
                            (alexandria:read-file-into-string
                             ,(concatenate 'string
                                           (directory-namestring
                                            (translate-logical-pathname "org:publish;"))
                                           key
                                           ".html"))))))
        *menu*)))

(def/route/2th_level)

;; (print
;;  (macroexpand-1
;;   '(def/route/2th_level)))

;; =>
;; (PROGN
;;   (DEF/ROUTE ABOUT
;;       ("about")
;;     (BASE-PAGE *HEAD-TITLE* "Rigidus homepage" *MENU*
;;                (READ-FILE-INTO-STRING
;;                 "/home/rigidus/repo/rigidus.ru/public_html/about.html")))
;;   (DEF/ROUTE ARTICLES
;;       ("articles")
;;     (BASE-PAGE *HEAD-TITLE* "Rigidus homepage" *MENU*
;;                (READ-FILE-INTO-STRING
;;                 "/home/rigidus/repo/rigidus.ru/public_html/articles.html")))
;;   (DEF/ROUTE BLOGS
;;       ("blogs")
;;     (BASE-PAGE *HEAD-TITLE* "Rigidus homepage" *MENU*
;;                (READ-FILE-INTO-STRING
;;                 "/home/rigidus/repo/rigidus.ru/public_html/blogs.html")))
;;   (DEF/ROUTE ALIENS
;;       ("aliens")
;;     (BASE-PAGE *HEAD-TITLE* "Rigidus homepage" *MENU*
;;                (READ-FILE-INTO-STRING
;;                 "/home/rigidus/repo/rigidus.ru/public_html/aliens.html")))
;;   (DEF/ROUTE RESOURCES
;;       ("resources")
;;     (BASE-PAGE *HEAD-TITLE* "Rigidus homepage" *MENU*
;;                (READ-FILE-INTO-STRING
;;                 "/home/rigidus/repo/rigidus.ru/public_html/resources.html")))
;;   (DEF/ROUTE CONTACTS
;;       ("contacts")
;;     (BASE-PAGE *HEAD-TITLE* "Rigidus homepage" *MENU*
;;                (READ-FILE-INTO-STRING
;;                 "/home/rigidus/repo/rigidus.ru/public_html/contacts.html"))))



(in-package :rigidus)

(restas:define-route main ("/")
  (alexandria:read-file-into-string (format nil "~A/index.html" *www-path*)))
(in-package #:rigidus)

(defparameter *log-404* nil)

(defun page-404 (&optional (title "404 Not Found") (content "Страница не найдена"))
  (let* ((title "404 Not Found"))
    (base-page *head-title*
               title
               *menu*
               "Page not found")))

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
