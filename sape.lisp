;; [[file:doc.org::*Sape][Sape:1]]
(in-package #:rigidus)


(defun base64-cookies ()
  (let* ((cookies   (hunchentoot:cookies-out*))
         (serialize (mapcar #'(lambda (x)
                                (let ((name (car x))
                                      (value (hunchentoot:cookie-value (cdr x))))
                                  (format nil "s:~d:\"~a\";s:~d:\"~a\";"
                                          (length name)
                                          name
                                          (length value)
                                          value)))
                            cookies))
         (seri-str  (format nil "a:~d:{~a}"
                            (length cookies)
                            (if (null cookies)
                                ""
                                (format nil "~{~a~}" serialize)))))
    (base64:string-to-base64-string seri-str)))

(defun recode (content from to)
  (sb-ext:octets-to-string (sb-ext:string-to-octets content :external-format from) :external-format to))

(defun get-sape-links (uri)
  (let ((rs "")
        (extproc (sb-ext:run-program "/usr/bin/php" `("-q" ,(format nil "~a" (path "links.php")))
                                     :environment (append (sb-ext:posix-environ)
                                                          (list (format nil "REQUEST_URI=~a" uri))
                                                          (list (format nil "COOKIE=~a" (base64-cookies))))
                                     :wait t
                                     :input nil
                                     :output :stream)))
    (unwind-protect
         (with-open-stream (out (sb-ext:process-output extproc))
           (do ((c (read-char out) (read-char out nil 'the-end)))
               ((not (characterp c)))
             (setf rs (concatenate 'string rs (string c))))))
    (when extproc
      (sb-ext:process-close extproc)
      (sb-ext:process-exit-code extproc))
    ;; latin-1 = :ISO8859-1 = :cp1252 (http://ru.wikipedia.org/wiki/ISO_8859-1)
    (format nil "~a" (recode (base64:base64-string-to-string rs) :ISO8859-1 :cp1251))
    ))

(defun get-sape-context (uri content)
  (let* ((rs "")
         (input-stream (make-string-input-stream content)) ;; no recode - utf-8
         (extproc (sb-ext:run-program "/usr/bin/php" `("-q" ,(format nil "~a" (path "context.php")))
                                      :environment (append (sb-ext:posix-environ)
                                                           (list (format nil "REQUEST_URI=~a" uri))
                                                           (list (format nil "COOKIE=~a" (base64-cookies))))
                                      :wait t
                                      :input input-stream
                                      :output :stream)))
    (unwind-protect
         (with-open-stream (out (sb-ext:process-output extproc))
           (do ((c (read-char out) (read-char out nil 'the-end)))
               ((not (characterp c)))
             (setf rs (concatenate 'string rs (string c))))))
    (when extproc
      (sb-ext:process-close extproc)
      (sb-ext:process-exit-code extproc))
    ;; latin-1 = :ISO8859-1 = :cp1252 (http://ru.wikipedia.org/wiki/ISO_8859-1)
    (format nil "~a" (recode (base64:base64-string-to-string rs) :ISO8859-1 :utf-8))))
;; Sape:1 ends here
;; [[file:doc.org::*Инициализация][sape]]
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

(in-package #:rigidus)

(defun get-directory-contents (path)
  "Функция возвращает содержимое каталога"
  (when (not (equal "/" (coerce (last (coerce path 'list)) 'string)))
    (setf path (format nil "~A/" path)))
  (directory (format nil "~A*.*" path)))

(in-package #:rigidus)

(defun cache-section (global-var-hash relative-filepath)
  "Функция кеширует в хеш-таблице содержимое каталога"
  (loop :for file :in  (get-directory-contents relative-filepath) :do
     (setf (gethash (pathname-name file) global-var-hash)
           (parse-org file))))

(in-package #:rigidus)

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

(defun load-org ()
  ;; *articles* *aliens* *asdf*
  (cache-section *articles* "content/articles/")
  (cache-section *aliens*   "content/aliens/")
  (cache-section *blogs*   "content/blogs/")
  ;; cached pages
  (setf *cached-articles-page* (cache-page #P"content/articles.org" *articles* "/articles/"))
  (setf *cached-alien-page*    (cache-page #P"content/alien.org"    *aliens*   "/alien/"))
  (setf *cached-blogs-page*    (cache-page #P"content/blogs.org"    *blogs*   "/blogs/")))

(load-org)

(orgdata-directives (gethash "asdf-foreword" *aliens*))
(orgdata-directives (gethash "asdf-architecture" *aliens*))

;; start
(restas:start '#:rigidus :port 9993)
(restas:debug-mode-on)
;; (restas:debug-mode-off)
(setf hunchentoot:*catch-errors-p* t)


;; (maphash #'(lambda (k v)
;;             ;; (print (orgdata-content v)))
;;              (print (orgdata-directives v)))
;;         *blogs*)
;; sape ends here
;; [[file:doc.org::*Sape][sape]]
(in-package #:rigidus)
;; sape ends here
