;; [[file:~/src/rigidus.ru/org/lrn/crypto/mycoin.org::generators][generators]]
;; Copyright © 2014-2018 Glukhov Mikhail. All rights reserved.
;; Licensed under the GNU AGPLv3

(setq org-confirm-babel-evaluate nil)

(defun gen-fields (rows)
  (let ((result))
    (push "\n" result)
    (push (format "  (%s\n" (butlast (car rows))) result)
    (mapcar #'(lambda (x)
                (push (format "   %s\n" (butlast x)) result))
            (butlast (cdr rows)))
    (push (format "   %s)" (butlast (car (last rows)))) result)
    (mapconcat 'identity (reverse result) "")))

(defun gen-states (rows)
  (let ((result)
        (hash (make-hash-table :test #'equal))
        (states))
    (dolist (elt rows nil)
      (puthash (cadr elt) nil hash)
      (puthash (cadr (cdr elt))  nil hash))
    (maphash (lambda (k v)
               (push k states))
             hash)
    (push "\n" result)
    (push "  (" result)
    (dolist (elt (butlast states))
      (push (format ":%s " elt) result))
    (push (format ":%s)" (car (last states))) result)
    (mapconcat 'identity (reverse result) "")))

(defun gen-actions (rows)
  (let ((result))
    (push "\n" result)
    (let ((x (car rows)))
      (push (format "  ((:%s :%s :%s)" (cadr x) (cadr (cdr x)) (car x)) result))
    (if (equal 1 (length rows))
        (push ")" result)
      (progn
        (push "\n" result)
        (mapcar #'(lambda (x)
                    (push (format "   (:%s :%s :%s)\n" (cadr x) (cadr (cdr x)) (car x)) result))
                (cdr (butlast rows)))
        (let ((x (car (last rows))))
          (push (format "   (:%s :%s :%s))" (cadr x) (cadr (cdr x)) (car x)) result))))
    (mapconcat 'identity (reverse result) "")))
;; generators ends here
