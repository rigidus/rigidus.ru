(in-package #:rigidus)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; default-render
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass rigidus-render () ())
(defclass rigidus-render-comment () ())

(defparameter *default-render-method* (make-instance 'rigidus-render))


(defmethod restas:render-object ((designer rigidus-render) (data list))
  (destructuring-bind (headtitle navpoints content) data
    (tpl:root (list :headtitle headtitle
                    :content (tpl:base (list :navpoints navpoints
                                             :content content
                                             :stat (tpl:stat)))))))

(defmethod restas:render-object ((designer rigidus-render) (file pathname))
  (if (string= (pathname-type file) "org")
      (restas:render-object designer (parse-org file))
      (call-next-method)))


(defmethod restas:render-object ((designer rigidus-render) (data orgdata))
  (let* ((content     (concatenate 'string
                                   (orgdata-content data)
                                   ;; (get-comments data)
                                   ))
         (sections    (orgdata-sections data))
         (directives  (orgdata-directives data))
         (title       (getf directives :title))
         (menu-memo   (menu)))
    (restas:render-object
     designer
     (list title
           menu-memo
           (tpl:default
               (list :title title
                     :navpoints menu-memo
                     :sections (iter (for i from 1)
                                     (for section in sections)
                                     (collect (list :anchor (format nil "anchor-~a" i)
                                                    :level (format nil "level-~a" (car section))
                                                    :title (cadr section))))
                     :links (get-sape-links (hunchentoot:REQUEST-URI*))
                     :content (get-sape-context (hunchentoot:REQUEST-URI*) content)))))))
