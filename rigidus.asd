;; [[file:doc.org::*Файл определения системы][defsystem]]
;;;; Copyright © 2014-2017 Glukhov Mikhail. All rights reserved.
;;;; Licensed under the GNU AGPLv3
(asdf:defsystem #:rigidus
  :version      "0.0.3"
  :author       "rigidus <i.am.rigidus@gmail.com>"
  :licence      "AGPLv3"
  :description  "site http://rigidus.ru"
  :depends-on   (#:anaphora
                 #:closer-mop
                 #:cl-ppcre
                 #:cl-base64
                 #:cl-json
                 #:cl-html5-parser
                 #:cl-who
                 #:cl-fad
                 #:optima
                 #:closure-template
                 #:drakma
                 #:restas
                 #:restas-directory-publisher
                 #:split-sequence
                 #:postmodern
                 #:restas
                 #:optima
                 #:fare-quasiquote-extras
                 #:fare-quasiquote-optima)
  :serial       t
  :components   ((:module "src"
                          :serial t
                          :pathname "src"
                          :components ((:static-file "templates.htm")
                                       (:file "prepare")
                                       (:file "defmodule")
                                       (:file "html")
                                       (:file "ext-html")
                                       (:file "orgmode")
                                       (:file "render")
                                       (:file "routes")
                                       (:file "init")
                                       (:static-file "daemon.conf")
                                       (:static-file "daemon.lisp")
                                       (:static-file "daemon.sh")))))
;; defsystem ends here
