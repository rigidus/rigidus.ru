;; [[file:doc.org::*Подготовка к старту][prepare]]
;;;; Copyright © 2014-2017 Glukhov Mikhail. All rights reserved.
;;;; Licensed under the GNU AGPLv3

;; (closure-template:compile-template :common-lisp-backend #P"templates.htm")

;; (in-package #:rigidus)

;; (defparameter *repo-folder* "repo")
;; (defparameter *prj-folder* "rigidus")

;; ;; Базовый путь, от которого будем все считать
;; (defparameter *base-path*
;;   (format nil "~A~A"
;;           (namestring (user-homedir-pathname))
;;           (format nil "~A/~A/src/"
;;                   *repo-folder*
;;                   *prj-folder*)))

;; ;; Путь к данным
;; (defparameter *data-path*
;;   (format nil "~A~A"
;;           (namestring (user-homedir-pathname))
;;           (format nil "~A/~A/data/"
;;                   *repo-folder*
;;                   *prj-folder*)))

;; ;; Путь к стилям
;; (defparameter *css-path*
;;   (format nil "~A~A"
;;           (namestring (user-homedir-pathname))
;;           (format nil "~A/~A/css/"
;;                   *repo-folder*
;;                   *prj-folder*)))

;; ;; Путь к картинкам
;; (defparameter *img-path*
;;   (format nil "~A~A"
;;           (namestring (user-homedir-pathname))
;;           (format nil "~A/~A/img/"
;;                   *repo-folder*
;;                   *prj-folder*)))
;; (defparameter *pic-path*
;;   (format nil "~A~A"
;;           (namestring (user-homedir-pathname))
;;           (format nil "~A/~A/pic/"
;;                   *repo-folder*
;;                   *prj-folder*)))
;; (defparameter *ava-path*
;;   (format nil "~A~A"
;;           (namestring (user-homedir-pathname))
;;           (format nil "~A/~A/ava/"
;;                   *repo-folder*
;;                   *prj-folder*)))

;; ;; Путь к шрифтам
;; (defparameter *font-path*
;;   (format nil "~A~A"
;;           (namestring (user-homedir-pathname))
;;           (format nil "~A/~A/fonts/"
;;                   *repo-folder*
;;                   *prj-folder*)))

;; ;; Путь к скриптам
;; (defparameter *js-path*
;;   (format nil "~A~A"
;;           (namestring (user-homedir-pathname))
;;           (format nil '"~A/~A/js/"
;;                   *repo-folder*
;;                   *prj-folder*)))


;; ;; Компилируем шаблоны
;; (closure-template:compile-template
;;  :common-lisp-backend (pathname (concatenate 'string *base-path* "templates.htm")))

;; ;; submodules

;; ;; (restas:mount-module -css- (#:restas.directory-publisher)
;; ;;   (:url "/css/")
;; ;;   (restas.directory-publisher:*directory* *css-path*))

;; ;; (restas:mount-module -img- (#:restas.directory-publisher)
;; ;;   (:url "/img/")
;; ;;   (restas.directory-publisher:*directory* *img-path*))

;; ;; (restas:mount-module -pic- (#:restas.directory-publisher)
;; ;;   (:url "/pic/")
;; ;;   (restas.directory-publisher:*directory* *pic-path*))

;; ;; (restas:mount-module -ava- (#:restas.directory-publisher)
;; ;;   (:url "/ava/")
;; ;;   (restas.directory-publisher:*directory* *ava-path*))

;; ;; (restas:mount-module -font- (#:restas.directory-publisher)
;; ;;   (:url "/font/")
;; ;;   (restas.directory-publisher:*directory* *font-path*))

;; ;; (restas:mount-module -js- (#:restas.directory-publisher)
;; ;;   (:url "/js/")
;; ;;   (restas.directory-publisher:*directory* *js-path*))

;; ;; (restas:mount-module -resources- (#:restas.directory-publisher)
;; ;;   (:url "/resources/")
;; ;;   (restas.directory-publisher:*directory* "/resources/")
;; ;;   (restas.directory-publisher:*autoindex* t))
;; prepare ends here
