;; [[file:doc.org::*Маршруты][routes]]
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

(defmacro § ((title) &rest sequences)
    (let ((sequences (loop :for item :in sequences :collect
                        (cond ((and (stringp item) (equal 0 (position #\★  item)))
                               (concatenate 'string "<span style=\"color: green\">★ </span>" (subseq item 1)))
                              ((and (stringp item) (equal 0 (position #\✦  item)))
                               (concatenate 'string "<span style=\"color: red\">✦ </span><span style=\"color: #feffb8\"><strong>" (subseq item 1) "</strong></span>"))
                              (t item)))))
      `(tpl:section (list :title ,title :elts  (list ,@sequences)))))

(restas:define-route main ("/")
  (let* ((lines (iter (for line in-file "afor.txt" using #'read-line) (collect line)))
         (line (nth (random (length lines)) lines))
         (data (list "Программирование - как искусство"
                     (menu)
                     (tpl:main (list :title line :links "")))))
    (destructuring-bind (headtitle navpoints content)
        data
      (tpl:root (list :headtitle headtitle
                      :stat (tpl:stat)
                      :navpoints navpoints
                      :title line
                      :columns (tpl:main))))))

(let ((title "Клеточные автоматы"))
  (restas:define-route blog ("/post/:post-id")
    (let* ((data (list title
                       (menu)
                       (tpl:main (list :title title :links "")))))
      (destructuring-bind (headtitle navpoints content)
          data
        (tpl:root (list :headtitle headtitle
                        :stat (tpl:stat)
                        :navpoints navpoints
                        :title title
                        :columns (tpl:org (list :content "content"))
                        ))))))

(def/route blog ("blog")
  (let* ((content (tpl:onlisp))
         (title "")
         (menu-memo (menu)))
    (render
     (list title
           menu-memo
           (tpl:default
               (list :title title
                     :navpoints menu-memo
                     :sections (list (list :level 1 :anchor "2332" :title "wefewfew"))
                     :links ""
                     :content (tpl:blog (list :title "wefewf"))))))))

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

(def/route radio ("radio")
  (render #P"content/radio.org"))


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

(let ((h-articles (make-hash-table :test #'equal)))
  (def/route article ("articles/:strkey")
    (multiple-value-bind (article isset)
        (gethash strkey h-articles)
      (if isset
          (render article)
          (let* ((filename (format nil "content/articles/~A.org" strkey))
                 (truename (probe-file filename)))
            (if (null truename)
                (page-404)
                (let ((data (parse-org truename)))
                  (setf (orgdata-content data)
                        (process-directive-make-list-by-category data h-articles "subst"))
                  (destructuring-bind (headtitle navpoints)
                      (list "title" (menu))
                    (tpl:root (list :headtitle (getf (orgdata-directives data) :title)
                                    :stat (tpl:stat)
                                    :navpoints navpoints
                                    :title (getf (orgdata-directives data) :title)
                                    :columns (tpl:org (list :content (orgdata-content data)))))))))))))

;; (render (show-article-from-hash "ecb" *articles*))

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

;; (defparameter *serial-status* nil)
;; (defparameter *serial-lock*   (bordeaux-threads:make-lock "serial-lock"))

;; (defun serial-getter ()
;;   (tagbody
;;    re
;;      (bordeaux-threads:acquire-lock *serial-lock* t)
;;      (with-open-file (stream "/dev/ttyACM0"
;;                              :direction :io
;;                              :if-exists :overwrite
;;                              :external-format :ascii)
;;        (setf *serial-status* (format nil "~C" (read-char stream))))
;;      (bordeaux-threads:release-lock *serial-lock*)
;;      (go re)))


;; (defparameter *serial-thread* (bordeaux-threads:make-thread #'serial-getter :name "serial-getter"))

;; ;; stty -F /dev/ttyACM0 cs8 9600 ignbrk -brkint -icrnl -imaxbel -opost -onlcr -isig -icanon -iexten -echo -echoe -echok -echoctl -echoke noflsh -ixon -crtscts raw

;; (restas:define-route test ("test")
;;   (with-open-file (stream "/dev/ttyACM0"
;;                           :direction :io
;;                           :if-exists :overwrite
;;                           :external-format :ascii)
;;     (format stream "9"))
;;   (sleep 1)
;;   (let ((tmp (parse-integer *serial-status*))
;;         (rs  nil))
;;     (if (equal 2 (logand tmp 2))
;;         (setf rs (append rs (list :red "checked")))
;;         (setf rs (append rs (list :darkred "checked"))))
;;     (if (equal 1 (logand tmp 1))
;;         (setf rs (append rs (list :lightgreen "checked")))
;;         (setf rs (append rs (list :green "checked"))))
;;     (let* ((content (tpl:controltbl rs))
;;            (title "Control Service")
;;            (menu-memo (menu)))
;;       (render (list title
;;                     menu-memo
;;                     (tpl:default
;;                         (list :title title
;;                               :navpoints menu-memo
;;                               :content content)))))))

;; (restas:define-route test-post ("test" :method :post)
;;   (let ((rs 0))
;;     (when (string= (hunchentoot:post-parameter "red") "on")
;;       (setf rs (logior rs 2)))
;;     (when (string= (hunchentoot:post-parameter "green") "on")
;;       (setf rs (logior rs 1)))
;;     (with-open-file (stream "/dev/ttyACM0"
;;                             :direction :io
;;                             :if-exists :overwrite
;;                             :external-format :ascii)
;;       (format stream "~A" rs))
;;     (hunchentoot:redirect "/test")))

;; submodules

(restas:mount-module -css- (#:restas.directory-publisher)
  (:url "/css/")
  (restas.directory-publisher:*directory* (merge-pathnames (make-pathname :directory '(:relative "repo/rigidus.ru/css")) (user-homedir-pathname))))

(restas:mount-module -font- (#:restas.directory-publisher)
  (:url "/font/")
  (restas.directory-publisher:*directory* (merge-pathnames (make-pathname :directory '(:relative "repo/rigidus.ru/font")) (user-homedir-pathname))))

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
;; routes ends here
