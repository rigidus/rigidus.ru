;; [[file:doc.org::*Инициализация][init]]
;;;; Copyright © 2014-2017 Glukhov Mikhail. All rights reserved.
;;;; Licensed under the GNU AGPLv3
(in-package #:rigidus)

;; start
(restas:start '#:rigidus :port 9993)
(restas:debug-mode-on)
;; (restas:debug-mode-off)
(setf hunchentoot:*catch-errors-p* t)
;; init ends here
