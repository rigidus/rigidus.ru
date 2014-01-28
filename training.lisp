(in-package #:rigidus)

(closure-template:compile-template :common-lisp-backend #P"train.htm")

(def/route training ("training")
  (alexandria:read-file-into-string #P"train.htm"))
