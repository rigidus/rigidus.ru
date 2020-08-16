;; [[file:~/src/rigidus.ru/org/lrn/crypto/mycoin.org::init][init]]
;;;; Copyright © 2014-2018 Glukhov Mikhail. All rights reserved.
;;;; Licensed under the GNU AGPLv3
(in-package #:mycoin)

;; start
(restas:start '#:mycoin :port *node-port*)
(restas:debug-mode-on)
;; (restas:debug-mode-off)
(setf hunchentoot:*catch-errors-p* t)
;; init ends here
