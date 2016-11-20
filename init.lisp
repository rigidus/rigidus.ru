;; [[file:doc.org::*Инициализация][init]]
(in-package #:rigidus)

(defun menu ()
  (list (list :link "/" :title "Главная")
        ;; Тут надо резюме
        (list :link "/about/" :title "О проекте")
        (list :link "/articles/" :title "Статьи")
        (list :link "/aliens/" :title "Материалы")
        (list :link "/resources/" :title "Ресурсы")
        (list :link "/contacts" :title "Контакты")))

(in-package #:rigidus)

(defun get-directory-contents (path)
  "Функция возвращает содержимое каталога"
  (when (not (equal "/" (coerce (last (coerce path 'list)) 'string)))
    (setf path (format nil "~A/" path)))
  (directory (format nil "~A*.*" path)))

;; start
(restas:start '#:rigidus :port 9993)
(restas:debug-mode-on)
;; (restas:debug-mode-off)
(setf hunchentoot:*catch-errors-p* t)
;; init ends here
