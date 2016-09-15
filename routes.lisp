(in-package #:rigidus)

(defclass rigidus-render () ())

;; 404

(defun page-404 (&optional (title "404 Not Found") (content "Страница не найдена"))
  (let* ((title "404 Not Found")
         (menu-memo (menu)))
    (render
     (list title
           menu-memo
           (tpl:default
               (list :title title
                     :navpoints menu-memo
                     :content "Страница не найдена"))))))

(restas:define-route not-found-route ("*any")
  (restas:abort-route-handler
   (page-404)
   :return-code hunchentoot:+http-not-found+
   :content-type "text/html"))


;; main

(defmacro § ((title) &rest sequences)
    (let ((sequences (loop :for item :in sequences :collect
                        (cond ((and (stringp item) (equal 0 (position #\★  item)))
                               (concatenate 'string "<span style=\"color: green\">★ </span>" (subseq item 1)))
                              ((and (stringp item) (equal 0 (position #\✦  item)))
                               (concatenate 'string "<span style=\"color: red\">✦ </span><span style=\"color: #feffb8\"><strong>" (subseq item 1) "</strong></span>"))
                              (t item)))))
      `(tpl:section (list :title ,title :elts  (list ,@sequences)))))


(defparameter *leftcol*
  (concatenate
   'string
   (§ ("Что здесь происходит?")
      "Слайды с лекции \"Литературное программирование\", которую я читал 22 января 2015 доступны здесь: <a href=\"http://slides.com/rigidusrigidus/deck#/\">http://slides.com/rigidusrigidus/deck/</a>"
      "✦ С Новым 2015 Годом!"
      "✦ С Новым 2014 Годом!"
      "★ В Египте, проходя реабилитацию, не удержался и запрограммировал автоматизацию работы гидов принимающей компании. А то жалко их - все делают в экселе.."
      "★ Попал в дтп и полгода работал удаленно, практически в режиме \"не вставая с постели\". Еще никогда я не был настолько производителен..."
      "★ Маленькое хобби \"про ардуино\" совершенно неожиданно для меня превратилось в практически вполне серьезное программирование микроконтроллеров и создание \"умных домов\". Оказывается довольно увлекательное и полезное дело."
      "✦ С Новым 2013 Годом!"
      "★ Не лиспом единым.. На новом проекте возникла интересная возможность познакомиться с еще одним функциональным языком программирования - Erlang."
      "Сравнивая с Common Lisp, можно сказать, что он не настолько изменяет мышление программиста, пришедшего из традиционных языков. Благодаря этому освоение проходит быстрее и проще, но и удивительного существенно меньше."
      "Однако в целом, Erlang весьма неплохо сбалансирован и как язык, и с точки зрения инфраструктуры."
      "★ В данный момент я исследую возможности контекстно-ориентированного программирования с использованием Common Lisp. Метод проходит опробацию на текущем проекте, над которым я работаю. Пока результаты весьма оптимистичные!"
      "✦ С Новым 2012 Годом!"
      "★ На новом проекте мне удалось отказаться от проприетарного сервиса Dropbox в пользу версионирования, основанного на <a href=\"http://ru.wikipedia.org/wiki/Git\" rel=\"nofollow\">GIT</a>."
      "Доказано: <a href=\"http://ru.wikipedia.org/wiki/Git\" rel=\"nofollow\">GIT</a> способны легко понять не только программисты, но и аналитики, верстальщики и контент-менеджеры."
      "★ Новый проект на Common Lisp - система организации тендеров, связывающая застройщиков и поставщиков строительных ресурсов. Новая архитектура, в основу которой положен специализированный DSL и генерация кода по нему."
      "★ Я покинул проект компании \"Цифры\" (<a href=\"http://320-8080.ru\" rel=\"nofollow\">320-8080.ru</a>) из-за отсутствия финансовых перспектив. Теперь проект развивает мой ученик, оставшийся работать в компании. Исходные коды интернет-магазина <a href=\"https://github.com/rigidus/cl-eshop\" rel=\"nofollow\">опубликованы</a> под лиценизией GPL v3 на github.com"
      "★ Common Lisp - один из самых удобных языков (в том числе и) для веб-программирования. Этот сайт тому подтверждение."
      "★ <a href=\"http://habrahabr.ru/blogs/webdev/111365/\" rel=\"nofollow\">Первый пост</a> (про лисп, конечно же :) на хабре"
      "★ Org-mode. Вся жизнь внутри Емакса: список дел, работа, покупки, и средство для подготовки документов в удобном для автомтической обработки формате."
      "✦ С Новым 2011 Годом!"
      "★ Выложил <a href=\"https://github.com/rigidus/cl-eshop\" rel=\"nofollow\">исходный код рабочего проекта</a> на github. OpenSource грядет!"
      "★ <a href=\"http://dropbox.com\" rel=\"nofollow\">Dropbox</a> in production! Теперь контент-менеджеры удобно работают с данными одновременно, а версионирование страхует их от ошибок.")))

(defparameter *rightcol*
  (concatenate
   'string
   (§ ("Кто здесь?")
      "Пока это всего лишь я, Rigidus, собираю здесь все свои проекты, статьи и руководства."
      "Я интересуюсь продвинутыми технологиями программирования, администрирования и разработки проектов в IT-сфере."
      "На этом сайте я стараюсь собрать лучшие руководства, описания методологий и справочники, которые могут быть использованы в моей (и не только моей) работе.")
   (§ ("Что дальше будет?")
      "В ближайшее время я добавлю несколько актуальных своих работ. Будут дописываться всяческие модули, сервисы и документация.")
   (§ ("Последние обновления")
      "★ Выложил пару статей посвященных объектно-ориентированному подходу и его альтернативам:"
      "<a href=\"/articles/oop-polyethylene\">ООП. Оборачиваем ошибки полиэтиленом</a><br /><a href=\"/articles/oo-dispatch\">Нетрадиционно ориентированная диспетчеризация</a>"
      "в которых постарался максимально подробно рассмотреть возможности, о которых не известно широкой аудитории поклонников \"паттернов проектирования\""
      "★ Начинаю выкладывать в \"Материалы\" подробный разбор системы ASDF (в 20 частях), который сделал linkfly."
      "<img src=\"/img/john-mccarthy.jpg\" />")))

(restas:define-route main ("/")
  (let* ((lines (iter (for line in-file "afor.txt" using #'read-line)
                      (collect line)))
         (line (nth (random (length lines))
                    lines))
         (data (list
                "Программирование - как искусство"
                (menu)
                (tpl:main
                 (list :title line
                       :links "")))))
    (destructuring-bind (headtitle navpoints content)
        data
      (tpl:root (list :headtitle headtitle
                      :content (tpl:base-main (list :navpoints navpoints
                                               :title line
                                               :content content
                                               :stat (tpl:stat))))))))


(def/route blog ("blog")
  (let* ((content (tpl:onlisp))
         (title "")
         (menu-memo (menu)))
    (render
     (list title
           menu-memo
           (tpl:default
               (list :title title
                     :navpoints menu-memo
                     :sections (list (list :level 1 :anchor "2332" :title "wefewfew"))
                     :links ""
                     :content (tpl:blog (list :title "wefewf"))))))))

;; plan file pages

(defmacro def/route (name param &body body)
  `(progn
     (restas:define-route ,name ,param
       ,@body)
     (restas:define-route
         ,(intern (concatenate 'string (symbol-name name) "/"))
         ,(cons (concatenate 'string (car param) "/") (cdr param))
       ,@body)))


(def/route about ("about")
  (render #P"content/about.org"))

(def/route resources ("resources")
  (render #P"content/resources.org"))

(def/route faq ("faq")
  (render #P"content/faq.org"))

(def/route contacts ("contacts")
  (render #P"content/contacts.org"))

(def/route radio ("radio")
  (render #P"content/radio.org"))


;; showing articles

(defun show-article-from-hash (strkey hash)
  (multiple-value-bind (article isset)
      (gethash strkey hash)
    (unless isset
      (restas:abort-route-handler
       (page-404)
       :return-code hunchentoot:+http-not-found+
       :content-type "text/html"))
    article))


(def/route articles ("articles")
  (render *cached-articles-page*))

(def/route aliens ("aliens")
  (render *cached-alien-page*))

(def/route article ("articles/:strkey")
  (render (show-article-from-hash strkey *articles*)))


;; (render (show-article-from-hash "ecb" *articles*))


(def/route alien ("alien/:strkey")
  (render (show-article-from-hash strkey *aliens*)))

(restas:define-route onlisp ("onlisp/doku.php")
  (let* ((content (tpl:onlisp))
         (title "Перевод книги Пола Грэма \"On Lisp\"")
         (menu-memo (menu)))
    (render
     (list title
           menu-memo
           (tpl:default
               (list :title title
                     :navpoints menu-memo
                     :sections ""
                     :links ""
                     :content content))))))

(require 'bordeaux-threads)

;; (defparameter *serial-status* nil)
;; (defparameter *serial-lock*   (bordeaux-threads:make-lock "serial-lock"))

;; (defun serial-getter ()
;;   (tagbody
;;    re
;;      (bordeaux-threads:acquire-lock *serial-lock* t)
;;      (with-open-file (stream "/dev/ttyACM0"
;;                              :direction :io
;;                              :if-exists :overwrite
;;                              :external-format :ascii)
;;        (setf *serial-status* (format nil "~C" (read-char stream))))
;;      (bordeaux-threads:release-lock *serial-lock*)
;;      (go re)))


;; (defparameter *serial-thread* (bordeaux-threads:make-thread #'serial-getter :name "serial-getter"))

;; ;; stty -F /dev/ttyACM0 cs8 9600 ignbrk -brkint -icrnl -imaxbel -opost -onlcr -isig -icanon -iexten -echo -echoe -echok -echoctl -echoke noflsh -ixon -crtscts raw

;; (restas:define-route test ("test")
;;   (with-open-file (stream "/dev/ttyACM0"
;;                           :direction :io
;;                           :if-exists :overwrite
;;                           :external-format :ascii)
;;     (format stream "9"))
;;   (sleep 1)
;;   (let ((tmp (parse-integer *serial-status*))
;;         (rs  nil))
;;     (if (equal 2 (logand tmp 2))
;;         (setf rs (append rs (list :red "checked")))
;;         (setf rs (append rs (list :darkred "checked"))))
;;     (if (equal 1 (logand tmp 1))
;;         (setf rs (append rs (list :lightgreen "checked")))
;;         (setf rs (append rs (list :green "checked"))))
;;     (let* ((content (tpl:controltbl rs))
;;            (title "Control Service")
;;            (menu-memo (menu)))
;;       (render (list title
;;                     menu-memo
;;                     (tpl:default
;;                         (list :title title
;;                               :navpoints menu-memo
;;                               :content content)))))))

;; (restas:define-route test-post ("test" :method :post)
;;   (let ((rs 0))
;;     (when (string= (hunchentoot:post-parameter "red") "on")
;;       (setf rs (logior rs 2)))
;;     (when (string= (hunchentoot:post-parameter "green") "on")
;;       (setf rs (logior rs 1)))
;;     (with-open-file (stream "/dev/ttyACM0"
;;                             :direction :io
;;                             :if-exists :overwrite
;;                             :external-format :ascii)
;;       (format stream "~A" rs))
;;     (hunchentoot:redirect "/test")))

;; submodules

(restas:mount-module -css- (#:restas.directory-publisher)
  (:url "/css/")
  (restas.directory-publisher:*directory* (merge-pathnames (make-pathname :directory '(:relative "repo/rigidus.ru/css")) (user-homedir-pathname))))

(restas:mount-module -font- (#:restas.directory-publisher)
  (:url "/font/")
  (restas.directory-publisher:*directory* (merge-pathnames (make-pathname :directory '(:relative "repo/rigidus.ru/font")) (user-homedir-pathname))))

(restas:mount-module -js- (#:restas.directory-publisher)
  (:url "/js/")
  (restas.directory-publisher:*directory* (merge-pathnames (make-pathname :directory '(:relative "repo/rigidus.ru/js"))  (user-homedir-pathname))))

(restas:mount-module -img- (#:restas.directory-publisher)
  (:url "/img/")
  (restas.directory-publisher:*directory* (merge-pathnames (make-pathname :directory '(:relative "repo/rigidus.ru/img")) (user-homedir-pathname))))

(restas:mount-module -resources- (#:restas.directory-publisher)
  (:url "/resources/")
  (restas.directory-publisher:*directory* (merge-pathnames (make-pathname :directory '(:relative "repo/rigidus.ru/resources")) (user-homedir-pathname)))
  (restas.directory-publisher:*autoindex* t))
