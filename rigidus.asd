(asdf:defsystem #:rigidus
  :version      "0.0.2"
  :author       "rigidus <i.am.rigidus@gmail.com>"
  :licence      "GPLv3"
  :description  "site http://rigidus.ru"
  :depends-on   (#:cl-ppcre
                 #:restas-directory-publisher
                 #:closure-template)
  :serial       t
  :components   ((:static-file "templates.htm")
                 (:file "defmodule")
                 (:file "orgmode")
                 (:file "sape")
                 (:file "render")
                 (:file "routes")
                 (:file "init")
                 (:static-file "daemon.conf")
                 (:static-file "daemon.lisp")
                 (:static-file "daemon.sh")))
