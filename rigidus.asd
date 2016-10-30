;; [[file:doc.org::*Каркас проекта][defsystem]]
;;;; Copyright © 2014-2016 Glukhov Mikhail. All rights reserved.
;;;; Licensed under the GNU AGPLv3
(asdf:defsystem #:rigidus
  :version      "0.0.2"
  :author       "rigidus <i.am.rigidus@gmail.com>"
  :licence      "GPLv3"
  :description  "site http://rigidus.ru"
  :depends-on   (#:anaphora
                 #:closer-mop
                 #:cl-ppcre
                 #:restas-directory-publisher
                 #:cl-base64
                 #:postmodern
                 #:restas
                 #:closure-template
                 #:drakma
                 #:split-sequence
                 #:cl-json)
  :serial       t
  :components   ((:static-file "templates.htm")
                 (:file "prepare")
                 (:file "defmodule")
                 (:file "orgmode")
                 (:file "sape")
                 (:file "routes")
                 (:file "init")
                 (:static-file "daemon.conf")
                 (:static-file "daemon.lisp")
                 (:static-file "daemon.sh")))
;; defsystem ends here
