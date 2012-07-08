(in-package #:rigidus)


;; 404

(defun page-404 (&optional (title "404 Not Found") (content "Страница не найдена"))
  (let* ((title "404 Not Found")
         (menu-memo (menu)))
    (restas:render-object
     (make-instance 'rigidus-render)
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
  (let* ((lines (iter (for line in-file (path "afor.txt") using #'read-line)
                     (collect line)))
         (line (nth (random (length lines))
                    lines)))
    (list "Программирование - как искусство"
          (menu)
          (tpl:main (list :title line
                          :links (get-sape-links "/"))))))


;; plan file pages

(restas:define-route about ("about")
  (path "content/about.org"))
(restas:define-route about/ ("about/")
  (path "content/about.org"))

(restas:define-route resources ("resources")
  (path "content/resources.org"))
(restas:define-route resources/ ("resources/")
  (path "content/resources.org"))

(restas:define-route faq ("faq")
  (path "content/faq.org"))
(restas:define-route faq/ ("faq/")
  (path "content/faq.org"))

(restas:define-route contacts ("contacts")
  (path "content/contacts.org"))
(restas:define-route contacts/ ("contacts/")
  (path "content/contacts.org"))


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


(restas:define-route articles ("articles")
  *cached-articles-page*)
(restas:define-route articles/ ("articles/")
  *cached-articles-page*)

(restas:define-route article ("articles/:strkey")
  (show-article-from-hash strkey *articles*))
(restas:define-route article/ ("articles/:strkey/")
  (show-article-from-hash strkey *articles*))


(restas:define-route aliens ("aliens")
  *cached-alien-page*)
(restas:define-route aliens/ ("aliens/")
  *cached-alien-page*)

(restas:define-route alien ("alien/:strkey")
  (show-article-from-hash strkey *aliens*))
(restas:define-route alien/ ("alien/:strkey/")
  (show-article-from-hash strkey *aliens*))

(restas:define-route onlisp ("onlisp/doku.php")
  (let* ((content
          "В настоящее время групп энтузиастов начала работу над переводом замечательной книги Пола Грэма \"On Lisp\"<br /></br>
Вы можете помочь в улучшении этого материала путем вычитки переведенных глав или присоединившись к переводу текста.<br /></br>
После завершения перевода он будет опубликован здесь. Переведенные и непереведенные главы размещены на гитхабе: <br /></br>
<a href=\"https://github.com/rigidus/onlisp\">https://github.com/rigidus/onlisp</a><br /></br>
Присоединяйтесь к команде переводчиков!")
         (title "Перевод книги Пола Грэма \"On Lisp\"")
         (menu-memo (menu)))
    (restas:render-object
     *default-render-method*
     (list title
           menu-memo
           (tpl:default
               (list :title title
                     :navpoints menu-memo
                     :sections ""
                     :links (get-sape-links (hunchentoot:REQUEST-URI*))
                     :content (get-sape-context (hunchentoot:REQUEST-URI*) content)))))))


;; submodules

(restas:mount-submodule -css- (#:restas.directory-publisher)
  (restas.directory-publisher:*baseurl* '("css"))
  (restas.directory-publisher:*directory* (path "css/")))

(restas:mount-submodule -js- (#:restas.directory-publisher)
  (restas.directory-publisher:*baseurl* '("js"))
  (restas.directory-publisher:*directory* (path "js/")))

(restas:mount-submodule -img- (#:restas.directory-publisher)
  (restas.directory-publisher:*baseurl* '("img"))
  (restas.directory-publisher:*directory* (path "img/")))

(restas:mount-submodule -resources- (#:restas.directory-publisher)
  (restas.directory-publisher:*baseurl* '("resources"))
  (restas.directory-publisher:*directory* (path "resources/")))
