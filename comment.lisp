(in-package #:rigidus)

(defgeneric get-comments (src)
  (:documentation "Generic function for MVC-comments"))


(defmethod get-comments ((data orgdata))
  "Controller"
  (let ((key (loop for k being the hash-keys in *articles* using (hash-value v) :do
                  (when (equal data v)
                    (return k)))))
    (get-comments
     (get-comments
      (intern (string-upcase key) :keyword)))))


(defmethod get-comments ((key symbol))
  "Model"
  ;; (error key)
  (select-dao 'comment (:and (:= 'parent 0) (:= 'key (symbol-name key)))))


(defmethod get-comments ((comments list))
  "View"
  (tpl:comments
   (list :comments
         (loop :for comment :in comments :collect
            (list :msg (msg comment)
                  :level 0
                  :id (id comment))))))


(defmethod get-comments ((root-id integer))
  "recursive object tree"
  (let ((rs (car (select-dao 'comment (:= 'id root-id)))))
    (setf (childs rs)
          (loop :for chd :in (select-dao 'comment (:= 'parent root-id) 'id) :collect
             (get-comments (id chd))))
    rs))


(defun reqursive-comments-view (root &key (level 0))
  (let ((rs (list `((id    . ,(id root))
                    (msg   . ,(msg root))
                    (level . ,level)))))
    (incf level 1)
    (loop :for chd :in (childs root) :collect
       (setf rs (append rs (reqursive-comments-view chd :level level))))
    rs))

