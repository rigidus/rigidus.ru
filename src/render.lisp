;; [[file:doc.org::*Рендеринг][render]]
;;;; Copyright © 2014-2017 Glukhov Mikhail. All rights reserved.
;;;; Licensed under the GNU AGPLv3
(in-package #:rigidus)

(defclass orgmode-handler () ())

(defmethod restas:render-object ((renderer orgmode-handler) (file pathname))
  ;; NOTE: Оставлено как пример вызова CGI
  ;; (cond
  ;;   ((and (string= (pathname-type file) "cgi"))
  ;;    (hunchentoot-cgi::handle-cgi-script file))
  ;;   (t
  ;;    (call-next-method)))
  (enobler file))
;; render ends here
