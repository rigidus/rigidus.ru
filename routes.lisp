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

(defmacro def/route (name param &body body)
  `(progn
     (restas:define-route ,name ,param
       ,@body)
     (restas:define-route
         ,(intern (concatenate 'string (symbol-name name) "/"))
         ,(cons (concatenate 'string (car param) "/") (cdr param))
       ,@body)))


(def/route about ("about" :method :post)
  (path "content/about.org"))

(def/route resources ("resources")
  (path "content/resources.org"))

(def/route faq ("faq")
  (path "content/faq.org"))

(def/route contacts ("contacts")
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


(def/route articles ("articles")
  *cached-articles-page*)


(def/route aliens ("aliens")
  *cached-alien-page*)


(def/route article ("articles/:strkey")
  (show-article-from-hash strkey *articles*))

(def/route article-post-action ("articles/:strkey" :method :post)
  ;; (format nil "zzz: ~A" (hunchentoot:post-parameters*)))
  ;; acts
  (aif (hunchentoot:parameter "act")
       (cond ((string= it "comment-del")
              ;; ***TODO***: reqursive delete
              (format nil "~A"
                      (execute
                       (sql (:delete-from 'comment :where (:= 'id (parse-integer (hunchentoot:post-parameter "id"))))))))
             ((string= it "comment-add")
              ;; ***TODO***: escape strings, rebuild comments
              (progn (query
                      (sql (:insert-into 'comment :set
                                         'parent (parse-integer (hunchentoot:post-parameter "parent"))
                                         'msg (hunchentoot:post-parameter "msg")
                                         'key (string-upcase strkey)
                                         'id (incf-comment-id))))
                     "1"))
             ((string= it "comment-expand")
              ;; (format nil "zzz: ~A" (hunchentoot:post-parameters*))
              ;; (format nil "iii: ~A"
              ;;         (parse-integer (hunchentoot:post-parameter "id")))
              (json:encode-json-to-string
               (reverse
                (cdr
                 (reqursive-comments-view
                  (get-comments (parse-integer (hunchentoot:post-parameter "id"))))))))
             ((string= it "comment-edit") "-edit")
             (t (error 'act-param-not-valid)))
       ;; else
       (error 'act-not-found-post-actions)))
  ;; (show-article-from-hash strkey *articles*))


(def/route alien ("alien/:strkey")
  (show-article-from-hash strkey *aliens*))


(restas:define-route onlisp ("onlisp/doku.php")
  (let* ((content (tpl:onlisp))
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
