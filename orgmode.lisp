(in-package #:rigidus)

(defmacro find-command (str body &optional (replace '(setf line "ℕ")))
  `(when (equal 0 (search ,str line))
     (let ((tail (handler-case (subseq line (+ 1 (length ,str)))
                   (SB-KERNEL:BOUNDING-INDICES-BAD-ERROR () ,str))))
       ,body
       ,replace)))

(defmacro find-directive (directive)
  `(find-command ,directive (setf (getf directives (intern (string-upcase (subseq ,directive 1)) :keyword))
                                  (string-trim '(#\  #\tab #\Newline) tail))))


(defgeneric parse-org (src)
  (:documentation "Transform org markup into HTML"))

(defmethod parse-org ((file pathname))
  (parse-org (alexandria:read-file-into-string file)))

(defmethod parse-org ((org-content string))
  ;; Разбиваем входный текст по строкам
  (let ((strings (split-sequence:split-sequence #\NewLine org-content))
        (sections)    ;; Информация о заголовках секций
        (mode nil)    ;; Режимы в котором мы находимся
        (directives)  ;; Директивы, например @category
        (br 0)        ;; Счетчик переводов строки для вывода обычного текста
        (save)        ;; Внутренняя переменная для сохранения и последующего вывода в файл
        (result (make-instance 'orgdata)))
    ;; Возвратим html
    (setf (orgdata-content result)
          (format nil "~{~A~%~}"
                  (remove-if #'(lambda (line)
                                 (search "ℕ" line))
                             (loop :for line :in strings
                                :collect
                                (progn
                                  ;; Удаляем директиву -*-
                                  (when (search "-*-" line)
                                    (setf line "ℕ"))
                                  ;; Директивы
                                  (find-directive "@title")
                                  (find-directive "@category")
                                  (find-directive "@sort")
                                  ;; *
                                  (when (ppcre:scan "\\A\\*+\\s+" line)
                                    (setf line
                                          (let ((cnt 1)) ;; Подcчитаем количество звездочек
                                            (loop :for item :across line :do
                                               (if (char= #\* item)
                                                   (incf cnt)
                                                   (return)))
                                            (let ((headline (subseq line cnt)))
                                              (push (list cnt headline) sections)
                                              (format nil "<h~a><a name=\"anchor-~a\">~a</a></h~a>"
                                                      cnt (length sections) headline cnt)))))
                                  ;; @/code
                                  (find-command "@/code" (setf (getf mode :code) nil) (setf line "</pre>"))
                                  (find-command "</source" (setf (getf mode :code) nil) (setf line "</pre>"))
                                  ;; ;; @store
                                  ;; (find-command "@store"
                                  ;;               (with-open-file (fstream (path tail) :direction :output :if-exists :supersede)
                                  ;;                 (format fstream "~{~a~%~}"
                                  ;;                         (loop :for i :in (reverse save)
                                  ;;                            :unless (search "ℕ" i)
                                  ;;                            :collect i))))
                                  ;; ;; @append
                                  ;; (find-command "@append"
                                  ;;               (with-open-file (fstream (path tail) :direction :output :if-exists :append)
                                  ;;                 (format fstream "~{~a~%~}"
                                  ;;                         (loop :for i :in (reverse save)
                                  ;;                            :unless (search "ℕ" i)
                                  ;;                            :collect i))))
                                  ;; mode:code
                                  (when (getf mode :code)
                                    (push line save)
                                    (setf line (format nil "~a" (ppcre:regex-replace-all "<" line "&lt;"))))
                                  ;; Проверка на малый разделитель
                                  (when (ppcre:scan "\\A\\-{3,}" line)
                                    (setf line "<div class=\"divider\">.</div>"))
                                  ;; Проверка на большой разделитель
                                  (when (ppcre:scan "\\A\\={3,}(.*)" line)
                                    (setf line
                                          (cl-ppcre:regex-replace "\\A\\={3,}(.*)" line
                                                                  (list #'(lambda (match reg)
                                                                            (declare (ignore match))
                                                                            (format nil "~a<div ~a ~a>~a~a"
                                                                                    "<div class=\"divider\">.</div>"
                                                                                    "class=\"guideNum\""
                                                                                    "id=\"config-options\""
                                                                                    reg
                                                                                    "<a href=\"#top\">top</a></div>")))
                                                                  :simple-calls t)))
                                  ;; default
                                  (setf line
                                        (if (not (string= "" line))
                                            line
                                            (progn
                                              (incf br)
                                              (if (> br 1)
                                                  (progn
                                                    (setf br 0)
                                                    "<p>")
                                                  "</p>"))))
                                  ;; @code
                                  (if (or (equal 0 (search "@code" line))
                                          (equal 0 (search "<source" line)))
                                      (prog1
                                          "<pre>"
                                        (setf save nil)
                                        (setf (getf mode :code) t))
                                      ;; else
                                      line))))))
    ;; заголовки секций (в обратном порядке)
    (setf (orgdata-sections result)
          (reverse sections))
    ;; директивы
    (setf (orgdata-directives result)
          directives)
    result))


(defun find-articles-by-category (category global-var-hash subst)
  "Возвращает все статьи, у которых @category соотвествует параметру"
  (sort (iter (for filename in (hash-table-keys global-var-hash))
              (let ((directives (orgdata-directives (gethash filename global-var-hash))))
                (when (string= category (getf directives :category))
                  (collect (list :title (getf directives :title)
                                 :link  (concatenate 'string subst filename)
                                 :sort  (getf directives :sort))))))
        #'string<
        :key #'(lambda (x) (getf x :title))))
