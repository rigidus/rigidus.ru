(in-package #:rigidus)

(defgeneric get-comments (src)
  (:documentation "Generic function for MVC-comments"))


(defmethod get-comments ((data orgdata))
  "Comment controller"
  (let ((key (loop for k being the hash-keys in *articles* using (hash-value v) :do
                  (when (equal data v)
                    (return k)))))
    ;; acts
    (awhen (hunchentoot:parameter "act")
      (cond ((string= it "new") "-new")
            ((string= it "get") "-get")
            ((string= it "set") "-set")))
    (get-comments
     (get-comments
      (intern (string-upcase key) :keyword)))))


(defmethod get-comments ((key symbol))
  "Model"
  (list "a1" "b2" "c3"))

(defmethod get-comments ((comments list))
  "View"
  (format nil "~{~A~}"
          (loop :for comment :in comments :collect
             (format nil "~A | <br/>" comment))))


;; == Модель (англ. Model).
;; Модель предоставляет знания:
;; данные и методы работы с этими данными, реагирует на запросы, изменяя своё состояние.
;; Не содержит информации, как эти знания можно визуализировать.
;; -> (get-comments (article article))
;; -> (get-comments (comment comment))


;; == Представление, вид (англ. View).
;; Отвечает за отображение информации (визуализацию).
;; Выводим с помощью шаблонизатора результат или сообщение об ошибке, или сообщение об отсутствии данных
;; -> (render-object (designer rigidus-comment) (data list))

;; == Контроллер (англ. Controller).
;; Обеспечивает связь между пользователем и системой:
;; контролирует ввод данных пользователем и использует модель и представление для реализации необходимой реакции.
;; -> (del (comment comment))
;; -> (set (comment comment))
;; -> (new (comment comment))

;; Обработать действия пользователя, если они есть - измененить модель
;; Получить и проинициализировать View заполнив шаблон
;; Получить Model, проверить данные
;; Передать данные в View, для шаблонизации и выдачи

;; Модель ничего «не знает» о Представленииб а Контроллер не содержит в себе какой-либо бизнес-логики.
