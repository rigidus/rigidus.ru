;; [[file:~/src/rigidus.ru/org/lrn/crypto/mycoin.org::prepare][prepare]]
;;;; Copyright © 2014-2018 Glukhov Mikhail. All rights reserved.
;;;; Licensed under the GNU AGPLv3

(closure-template:compile-template
 :common-lisp-backend (merge-pathnames
                       (make-pathname :name "templates" :type "htm")
                       (merge-pathnames
                        (make-pathname :directory '(:relative "src"))
                        (merge-pathnames
                         (make-pathname :directory '(:relative "src/rigidus.ru/org/lrn/crypto"))
                         (user-homedir-pathname)))))
;; prepare ends here
