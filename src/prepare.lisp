;; [[file:doc.org::*Подготовка к запуску][prepare]]
;;;; Copyright © 2014-2017 Glukhov Mikhail. All rights reserved.
;;;; Licensed under the GNU AGPLv3

(closure-template:compile-template
 :common-lisp-backend (merge-pathnames
                       (make-pathname :name "templates" :type "htm")
                       (merge-pathnames
                        (make-pathname :directory '(:relative "src"))
                        (merge-pathnames
                         (make-pathname :directory '(:relative "repo/rigidus.ru"))
                         (user-homedir-pathname)))))
;; prepare ends here
