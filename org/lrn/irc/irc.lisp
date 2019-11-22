;; https://common-lisp.net/project/cl-irc/

(require :cl-irc)

(in-package :irc)

(defparameter *connection*
  (connect :nickname "testolo3" :server "irc.freenode.org"))

(read-message-loop *connection*)

(join *connection* "#alfazobeta")

(defun my-hook (param)
  (format t "~%[~A]~%" param))

(defun my-ping-hook (param)
  (format t "~%[-=:ping ~A :=-]~%" param))

(add-hook *connection* 'irc-message #'my-hook)

(add-hook *connection* 'irc-ping-message #'my-ping-hook)

(get-hooks *connection* 'irc-message)
(get-hooks *connection* 'irc-ping-message)

(cl-irc:privmsg *connection* "#alfazobeta" "https://dropmefiles.com/DD1kr")
