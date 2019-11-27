(ql:quickload "bordeaux-threads")
(ql:quickload "clx")
(ql:quickload "zpng")
(ql:quickload "png-read")
(ql:quickload "drakma")
(ql:quickload "cl-ppcre")
(ql:quickload "cl-base64")
(ql:quickload "prbs")

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

(defun save-png (pathname-str png)
  (zpng:write-png png pathname-str))

(defun get-png-sequence (png)
  (flex:with-output-to-sequence (stream)
    (zpng:write-png-stream png stream)))

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

(defun save (frmt-filename-str dims image)
  (let* ((height (car dims))
         (width  (* 8 (cadr dims))) ;; dbg: for unpack image
         (unpacked-image (unpack-image image))
         (unk-filename (format nil frmt-filename-str (gensym)))
         (png (get-png-obj width height unpacked-image :grayscale)))
    (save-png unk-filename png)))

;; TODO: get-png-sequence and encrypt

(let ((prev)
      (cnt 9999))
  (defun shot-func ()
    (format t "~%::shot-func")
    (let* ((snap (pack-image (x-snapshot)))
           (dims (array-dimensions snap)))
      (if (> cnt 4)
          (progn
            (save "FILE~A" dims snap)
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
            (save "FILE~ADIFF" dims xored)
            (setf prev snap)
            (incf cnt))))
    ;; re-schedule times
    (schedule-timer *shot-timer* 1 :absolute-p nil)))

(defparameter *shot-timer*
  (make-timer #'(lambda ()
                  (shot-func))
              :name "shot" :thread t))

;; (schedule-timer *shot-timer* 0.5)
