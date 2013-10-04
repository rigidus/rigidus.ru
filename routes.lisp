(in-package #:rigidus)

(defclass rigidus-render () ())

;; 404

(defun page-404 (&optional (title "404 Not Found") (content "Страница не найдена"))
  (let* ((title "404 Not Found")
         (menu-memo (menu)))
    (render
     (list title
           menu-memo
           (tpl:default
               (list :title title
                     :navpoints menu-memo
                     :content "Страница не найдена"))))))

(restas:define-route not-found-route ("*any")
  (restas:abort-route-handler
   (page-404)
   :return-code hunchentoot:+http-not-found+
   :content-type "text/html"))


;; main

(restas:define-route main ("/")
  (let* ((lines (iter (for line in-file "afor.txt" using #'read-line)
                     (collect line)))
         (line (nth (random (length lines))
                    lines))
         (data (list "Программирование - как искусство"
                   (menu)
                   (tpl:main (list :title line
                                   :links )))))
    (destructuring-bind (headtitle navpoints content) data
      (tpl:root (list :headtitle headtitle
                      :content (tpl:base (list :navpoints navpoints
                                               :content content
                                               :stat (tpl:stat))))))))

;; plan file pages

(defmacro def/route (name param &body body)
  `(progn
     (restas:define-route ,name ,param
       ,@body)
     (restas:define-route
         ,(intern (concatenate 'string (symbol-name name) "/"))
         ,(cons (concatenate 'string (car param) "/") (cdr param))
       ,@body)))


(def/route about ("about")
  (render #P"content/about.org"))

(def/route resources ("resources")
  (render #P"content/resources.org"))

(def/route faq ("faq")
  (render #P"content/faq.org"))

(def/route contacts ("contacts")
  (render #P"content/contacts.org"))


;; showing articles

(defun show-article-from-hash (strkey hash)
  (multiple-value-bind (article isset)
      (gethash strkey hash)
    (unless isset
      (restas:abort-route-handler
       (page-404)
       :return-code hunchentoot:+http-not-found+
       :content-type "text/html"))
    article))


(def/route articles ("articles")
  (render *cached-articles-page*))

(def/route aliens ("aliens")
  (render *cached-alien-page*))

(def/route article ("articles/:strkey")
  (render (show-article-from-hash strkey *articles*)))

(def/route alien ("alien/:strkey")
  (render (show-article-from-hash strkey *aliens*)))


(restas:define-route onlisp ("onlisp/doku.php")
  (let* ((content (tpl:onlisp))
         (title "Перевод книги Пола Грэма \"On Lisp\"")
         (menu-memo (menu)))
    (render
     (list title
           menu-memo
           (tpl:default
               (list :title title
                     :navpoints menu-memo
                     :sections ""
                     :links ""
                     :content content))))))

(require 'bordeaux-threads)

(defparameter *serial-status* nil)
(defparameter *serial-lock*   (bordeaux-threads:make-lock "serial-lock"))

(defun serial-getter ()
  (tagbody
   re
     (bordeaux-threads:acquire-lock *serial-lock* t)
     (with-open-file (stream "/dev/ttyACM0"
                             :direction :io
                             :if-exists :overwrite
                             :external-format :ascii)
       (setf *serial-status* (format nil "~C" (read-char stream))))
     (bordeaux-threads:release-lock *serial-lock*)
     (go re)))


(defparameter *serial-thread* (bordeaux-threads:make-thread #'serial-getter :name "serial-getter"))

(restas:define-route test ("test")
  (with-open-file (stream "/dev/ttyACM0"
                          :direction :io
                          :if-exists :overwrite
                          :external-format :ascii)
    (format stream "9"))
  (sleep 1)
  (let ((tmp (parse-integer *serial-status*))
        (rs  nil))
    (if (equal 1 (logand tmp 1))
      (setf rs (append rs (list :red "checked")))
      (setf rs (append rs (list :darkred "checked"))))
    (if (equal 2 (logand tmp 2))
        (setf rs (append rs (list :lightgreen "checked")))
        (setf rs (append rs (list :green "checked"))))
    (let* ((content (tpl:controltbl rs))
           (title "Control Service")
           (menu-memo (menu)))
      (render (list title
                    menu-memo
                    (tpl:default
                        (list :title title
                              :navpoints menu-memo
                              :content content)))))))

(restas:define-route test-post ("test" :method :post)
  (let ((rs 0))
    (when (string= (hunchentoot:post-parameter "red") "on")
      (setf rs (logior rs 1)))
    (when (string= (hunchentoot:post-parameter "green") "on")
      (setf rs (logior rs 2)))
    (with-open-file (stream "/dev/ttyACM0"
                            :direction :io
                            :if-exists :overwrite
                            :external-format :ascii)
      (format stream "~A" rs))
    (hunchentoot:redirect "/test")))

;; submodules

(restas:mount-module -css- (#:restas.directory-publisher)
  (:url "/css/")
  (restas.directory-publisher:*directory* (merge-pathnames (make-pathname :directory '(:relative "repo/rigidus.ru/css")) (user-homedir-pathname))))

(restas:mount-module -js- (#:restas.directory-publisher)
  (:url "/js/")
  (restas.directory-publisher:*directory* (merge-pathnames (make-pathname :directory '(:relative "repo/rigidus.ru/js"))  (user-homedir-pathname))))

(restas:mount-module -img- (#:restas.directory-publisher)
  (:url "/img/")
  (restas.directory-publisher:*directory* (merge-pathnames (make-pathname :directory '(:relative "repo/rigidus.ru/img")) (user-homedir-pathname))))

(restas:mount-module -resources- (#:restas.directory-publisher)
  (:url "/resources/")
  (restas.directory-publisher:*directory* (merge-pathnames (make-pathname :directory '(:relative "repo/rigidus.ru/resources")) (user-homedir-pathname)))
  (restas.directory-publisher:*autoindex* t))
