;; [[file:~/src/rigidus.ru/org/lrn/crypto/mycoin.org::defsystem][defsystem]]
;;;; Copyright © 2014-2018 Glukhov Mikhail. All rights reserved.
;;;; Licensed under the GNU AGPLv3
(asdf:defsystem #:mycoin
  :version      "0.0.1"
  :author       "rigidus <i.am.rigidus@gmail.com>"
  :licence      "AGPLv3"
  :description  "mycoincurrency for experimental purposes"
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
                 #:fare-quasiquote-optima
                 #:ironclad)
  :serial       t
  :components   ((:module "src"
                          :serial t
                          :pathname "src"
                          :components ((:static-file "templates.htm")
                                       (:file "prepare")
                                       (:file "defmodule")
                                       (:file "entity")
                                       (:file "entityes")
                                       (:file "render")
                                       (:file "routes")
                                       (:file "init")
                                       ))))
;; defsystem ends here
