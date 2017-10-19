;; [[file:doc.org::*Расширенные макросы для HTML-DSL][ext_html]]
(in-package #:rigidus)

(in-package #:rigidus)

(defmacro repeat ((comment position varname maplist) &body body)
  (let ((format-string
         (concatenate 'string "<!-- [" comment "] -->~%~{~A~}~"
                      (format nil "~D" (* 2 position))
                      "T<!-- [/" comment "]-->")))
    `(format nil ,format-string
             (mapcar #'(lambda (,varname)
                         (html ,position
                               ,@body))
                     ,maplist))))

;; (macroexpand-1 '(repeat ("the-comment" 4 menu-elt *menu*)
;;                  ((:li :class "menu-el")
;;                   ((:a :href (car menu-elt)) (cdr menu-elt)))))

;; => (FORMAT NIL "<!-- [the-comment] -->~%~{~A~}~8T<!-- [/the-comment]-->"
;;            (MAPCAR
;;             #'(LAMBDA (MENU-ELT)
;;                 (HTML 4
;;                       ((:LI :CLASS "menu-el")
;;                        ((:A :HREF (CAR MENU-ELT)) (CDR MENU-ELT)))))
;;             *MENU*)), T
(in-package #:rigidus)

(defun base-page (head-title title menu content)
  (html 0
        "<!doctype html>"
        ((:html)
         ((:head)
          ((:title) head-title))
         ((:body)
          ((:h1) title)
          ((:div :id "main-menu" :class "navigation-menu")
           ((:ul :class "menu-list")
            (repeat ("main-menu" 4 menu-elt menu)
              ((:li :class "menu-el")
               ((:a :href (concatenate 'string "/" (car menu-elt))) (cdr menu-elt))))))
          ((:div :id "page-content")
           content)))))

;; (print
;;  (base-page "Программирование - как искусство"
;;             "Rigidus homepage"
;;             '(("about"     . "О проекте")
;;               ("contacts"  . "Контакты"))
;;             "Under construction"))

;; "<!doctype html>
;; <HTML>
;;   <HEAD>
;;     <TITLE>
;;       Программирование - как искусство
;;     </TITLE>
;;   </HEAD>
;;   <BODY>
;;     <H1>
;;       Rigidus homepage
;;     </H1>
;;     <DIV ID=\"main-menu\" CLASS=\"navigation-menu\">
;;       <UL CLASS=\"menu-list\">
;;         <!-- [main-menu] -->
;;         <LI CLASS=\"menu-el\">
;;           <A HREF=\"about\">
;;             О проекте
;;           </A>
;;         </LI>
;;         <LI CLASS=\"menu-el\">
;;           <A HREF=\"contacts\">
;;             Контакты
;;           </A>
;;         </LI>
;;         <!-- [/main-menu]-->
;;       </UL>
;;     </DIV>
;;     <DIV ID=\"page-content\">
;;       Under construction
;;     </DIV>
;;   </BODY>
;; </HTML>
;; "
;; ext_html ends here
