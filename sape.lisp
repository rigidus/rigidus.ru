(in-package #:rigidus)


(defun base64-cookies ()
  (let* ((cookies   (hunchentoot:cookies-out*))
         (serialize (mapcar #'(lambda (x)
                                (let ((name (car x))
                                      (value (hunchentoot:cookie-value (cdr x))))
                                  (format nil "s:~d:\"~a\";s:~d:\"~a\";"
                                          (length name)
                                          name
                                          (length value)
                                          value)))
                            cookies))
         (seri-str  (format nil "a:~d:{~a}"
                            (length cookies)
                            (if (null cookies)
                                ""
                                (format nil "~{~a~}" serialize)))))
    (base64:string-to-base64-string seri-str)))

(defun recode (content from to)
  (sb-ext:octets-to-string (sb-ext:string-to-octets content :external-format from) :external-format to))

(defun get-sape-links (uri)
  (let ((rs "")
        (extproc (sb-ext:run-program "/usr/bin/php" `("-q" ,(format nil "~a" (path "links.php")))
                                     :environment (append (sb-ext:posix-environ)
                                                          (list (format nil "REQUEST_URI=~a" uri))
                                                          (list (format nil "COOKIE=~a" (base64-cookies))))
                                     :wait t
                                     :input nil
                                     :output :stream)))
    (unwind-protect
         (with-open-stream (out (sb-ext:process-output extproc))
           (do ((c (read-char out) (read-char out nil 'the-end)))
               ((not (characterp c)))
             (setf rs (concatenate 'string rs (string c))))))
    (when extproc
      (sb-ext:process-close extproc)
      (sb-ext:process-exit-code extproc))
    ;; latin-1 = :ISO8859-1 = :cp1252 (http://ru.wikipedia.org/wiki/ISO_8859-1)
    (format nil "~a" (recode (base64:base64-string-to-string rs) :ISO8859-1 :cp1251))
    ))

(defun get-sape-context (uri content)
  (let* ((rs "")
         (input-stream (make-string-input-stream content)) ;; no recode - utf-8
         (extproc (sb-ext:run-program "/usr/bin/php" `("-q" ,(format nil "~a" (path "context.php")))
                                      :environment (append (sb-ext:posix-environ)
                                                           (list (format nil "REQUEST_URI=~a" uri))
                                                           (list (format nil "COOKIE=~a" (base64-cookies))))
                                      :wait t
                                      :input input-stream
                                      :output :stream)))
    (unwind-protect
         (with-open-stream (out (sb-ext:process-output extproc))
           (do ((c (read-char out) (read-char out nil 'the-end)))
               ((not (characterp c)))
             (setf rs (concatenate 'string rs (string c))))))
    (when extproc
      (sb-ext:process-close extproc)
      (sb-ext:process-exit-code extproc))
    ;; latin-1 = :ISO8859-1 = :cp1252 (http://ru.wikipedia.org/wiki/ISO_8859-1)
    (format nil "~a" (recode (base64:base64-string-to-string rs) :ISO8859-1 :utf-8))))

