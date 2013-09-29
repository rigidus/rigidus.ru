(ql:quickload "anaphora")
(ql:quickload "closer-mop")
(ql:quickload "postmodern")
(ql:quickload "restas")
(ql:quickload "closure-template")
(ql:quickload "restas-directory-publisher")
(ql:quickload "cl-base64")

(closure-template:compile-template :common-lisp-backend #P"templates.htm")
