(in-package #:rigidus)

(defun menu ()
  (list (list :link "/" :title "Главная")
        (list :link "/about" :title "Об авторе")
        ;; Тут надо резюме
        ;; (list :link "/blog/" :title "Блог")
        (list :link "/articles/" :title "Статьи")
        (list :link "/aliens/" :title "Материалы")
        (list :link "/resources/" :title "Ресурсы")
        (list :link "/faq/" :title "FAQ")
        ;; (list :link "/job/" :title "О, работа!")
        (list :link "/contacts" :title "Контакты")))


(defun get-directory-contents (path)
  "Функция возвращает содержимое каталога"
  (when (not (equal "/" (coerce (last (coerce path 'list)) 'string)))
    (setf path (format nil "~A/" path)))
  (directory (format nil "~A*.*" path)))

(defun cache-section (global-var-hash relative-filepath)
  "Функция кеширует в хеш-таблице содержимое каталога"
  (loop :for file :in  (get-directory-contents relative-filepath) :do
     (setf (gethash (pathname-name file) global-var-hash)
           (parse-org file))))

;; cache-page
(defun cache-page (relative-filepath global-var-hash subst)
  (let ((data (parse-org relative-filepath)))
    (setf (orgdata-content data)
          (ppcre:regex-replace-all
           "@make-list-by-category(.*)@"
           (orgdata-content data)
           (list #'(lambda (match reg)
                     (declare (ignore match))
                     (let* ((instr (string-trim '(#\Space #\Tab #\Newline) reg)))
                       (multiple-value-bind (star color category)
                           (values-list (split-sequence:split-sequence #\Space instr))
                         (format nil
                                 "<ul>~{~a~}</ul>"
                                 (iter (for x in (sort (find-articles-by-category category global-var-hash subst)
                                                       #'string<
                                                       :key #'(lambda (x) (getf x :sort))))
                                       (collect (tpl:li (append x (list :star star :color color))))))))))
           :simple-calls t))
    data))

;; (defun load-org ()
;;   ;; *articles* *aliens* *asdf*
;;   (cache-section *articles* "content/articles/")
;;   (cache-section *aliens*   "content/aliens/")
;;   (cache-section *blogs*   "content/blogs/")
;;   ;; cached pages
;;   (setf *cached-articles-page* (cache-page #P"content/articles.org" *articles* "/articles/"))
;;   (setf *cached-alien-page*    (cache-page #P"content/alien.org"    *aliens*   "/alien/"))
;;   (setf *cached-blogs-page*    (cache-page #P"content/blogs.org"    *blogs*   "/blogs/")))


;; (load-org)

;; (orgdata-directives (gethash "asdf-foreword" *aliens*))
;; (orgdata-directives (gethash "asdf-architecture" *aliens*))

;; start
(restas:start '#:rigidus :port 9993)
(restas:debug-mode-on)
;; (restas:debug-mode-off)
(setf hunchentoot:*catch-errors-p* t)


;; (maphash #'(lambda (k v)
;;             ;; (print (orgdata-content v)))
;;              (print (orgdata-directives v)))
;;         *blogs*)
