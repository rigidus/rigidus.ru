(ql:quickload "clx")
(ql:quickload "zpng")
(ql:quickload "png-read")

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

(defparameter *default-x* 70)
(defparameter *default-y* 0)
(defparameter *default-width* 600)
(defparameter *default-heght* 300)

(multiple-value-bind (default-width default-height) (x-size)
  (defun x-snapshot (&key (x *default-x*) (y *default-y*)
                       (width  *default-width*) (height *default-heght*)
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
              width height)))
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
            (zpng:data-array image))))))

;; ;; TEST: save screenshot
;; (x-snapshot :path "~/Pictures/snap1.png")


;; Ошибка, возникающая когда мы пытаемся прочитать png
;; в котором неизвестно сколько байт на точку
(define-condition unk-png-color-type (error)
  ((color :initarg :color :reader color))
  (:report
   (lambda (condition stream)
     (format stream "Error in LOAD-PNG: unknown color type: ~A"
             (color condition)))))

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
             (:indexed-color 1)
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
    ;; (format t "~% new-arr ~A "(array-dimensions result))
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

;; ;; TEST: equality screenshot and load-file-data
;; (assert (equalp (progn
;;                   (x-snapshot :path "~/Pictures/snap2.png")
;;                   (load-png "~/Pictures/snap2.png"))
;;                 (x-snapshot)))


(defun save-png (width height pathname-str image
                 &optional (color-type :truecolor-alpha))
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
    (zpng:write-png png pathname-str)))


;; ;; TEST: saving loaded data
;; (let* ((from "~/Pictures/snap2.png")
;;        (to   "~/Pictures/snap3.png")
;;        (image-data (load-png from)))
;;   (destructuring-bind (height width depth)
;;       (array-dimensions image-data)
;;     (save-png width height to image-data)))

;; ;; TEST: saving screenshot data
;; (let* ((to   "~/Pictures/snap4.png")
;;        (image-data (x-snapshot)))
;;   (destructuring-bind (height width depth)
;;       (array-dimensions image-data)
;;     (save-png width height to image-data)))


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

;; ;; TEST: load file and translate it to grayscale and save
;; (let* ((from "~/Pictures/snap4.png")
;;        (to   "~/Pictures/snap5.png")
;;        (image-data (binarization (load-png from))))
;;   (destructuring-bind (height width) ;; NB: no depth!
;;       (array-dimensions image-data)
;;     (save-png width height to image-data :grayscale))) ;; NB: grayscale!


;; ;; TEST: binarize and save screenshot
;; (let* ((to   "~/Pictures/snap6.png")
;;        (image-data (binarization (x-snapshot) 127))) ;; NEW: threshold!
;;   (destructuring-bind (height width) ;; NB: no depth!
;;       (array-dimensions image-data)
;;     (save-png width height to image-data :grayscale))) ;; NB: grayscale!

;; ;; TEST: try to load grayscale image and save it
;; (let* ((from "~/Pictures/snap6.png")
;;        (to   "~/Pictures/snap7.png")
;;        (image-data (load-png from)))
;;   (destructuring-bind (height width)
;;       (array-dimensions image-data)
;;     (save-png width height to image-data :grayscale)))

;; ;; TEST: try to load grayscale image, binarize and save it
;; (let* ((from "~/Pictures/snap7.png")
;;        (to   "~/Pictures/snap8.png")
;;        (image-data (binarization (load-png from) 127)))
;;   (destructuring-bind (height width) ;; NB: no depth!
;;       (array-dimensions image-data)
;;     (save-png width height to image-data :grayscale)))
(defconstant +foreground+ 0)
(defconstant +mark+ 127)
(defconstant +box+ 1)

(defun gramma-lookup (image)
  (let ((boxes))
    (do ((qy 0 (incf qy)))
        ((= qy (array-dimension image 0)))
    (do ((qx 0 (incf qx)))
        ((= qx (array-dimension image 1)))
        ;; when we found foreground point
        (when (equal +foreground+ (aref image qy qx))
          ;; (format t "~%SCAN: ~A.~A = ~A" qy qx (aref image qy qx))
          (let ((mark-points (list (cons qy qx)))
                (bucket))
            (tagbody
             gramma
               (let ((curr (pop mark-points)))
                 ;; save current point in bucket
                 (push curr bucket)
                 ;; ;;;; dbg-out current point
                 ;; (format t "~%:CURR:~A" curr)
                 (destructuring-bind (curr-x . curr-y)
                     curr
                   ;; mark current point
                   (setf (aref image curr-x curr-y) +mark+)
                   ;; lookup foreground-colored neighbors
                   (let* ((neighbors (list (cons (- curr-x 1) (- curr-y 1))
                                           (cons curr-x       (- curr-y 1))
                                           (cons (+ curr-x 1) (- curr-y 1))
                                           (cons (- curr-x 1) curr-y)
                                           (cons (+ curr-x 1) curr-y)
                                           (cons (- curr-x 1) (+ curr-y 1))
                                           (cons curr-x       (+ curr-y 1))
                                           (cons (+ curr-x 1) (+ curr-y 1))))
                          (new-points (loop
                                         :for (dx . dy) :in neighbors
                                         :when (equal +foreground+ (aref image dx dy))
                                         :collect (progn
                                                    ;; mark neighbors
                                                    (setf (aref image dx dy) +mark+)
                                                    (cons dx dy)))))
                     ;; add new-points (current poped yet)
                     (setf mark-points (append mark-points new-points))
                     ;; ;;;; dbg-out new points
                     ;; (format t "~%:PNTS:~A" new-points)
                     ;; ;;;; save png file
                     ;; (destructuring-bind (dw dh)
                     ;;     (array-dimensions image)
                     ;;   (save-png-gray
                     ;;    dw dh
                     ;;    (format nil "cell~4,'0d.png" pic)
                     ;;    (vectorize-image-gray image))
                     ;;   (incf pic))
                     ;; ---------------------
                     (unless (null mark-points)
                       (go gramma))))))
            ;; build bounding box
            (let ((left-up     (cons (reduce #'min (mapcar #'car bucket))
                                     (reduce #'min (mapcar #'cdr bucket))))
                  (right-down  (cons (reduce #'max (mapcar #'car bucket))
                                     (reduce #'max (mapcar #'cdr bucket)))))
              ;; (format t "~%:BOX: ~A" (list left-up right-down))
              (push (list left-up right-down) boxes))))))
    boxes))


;; TEST: lookup symbols
(let* ((from "text.png")
       (to   "cell1.png")
       (image-data (binarization (load-png from) 127))
       (boxes (gramma-lookup image-data)))
  (loop :for (left-up right-down) :in boxes :do
     ;; draw box
     (loop :for dx :from (car left-up) :to (car right-down)
        :with top = (cdr left-up) and bottom = (cdr right-down) :do
        (setf (aref image-data dx top) +box+)
        (setf (aref image-data dx bottom) +box+))
     (loop :for dy :from (cdr left-up) :to (cdr right-down)
        :with left = (car right-down) :and right = (car left-up) :do
        (setf (aref image-data left dy) +box+)
        (setf (aref image-data right dy) +box+)))
  (destructuring-bind (height width)
      (array-dimensions image-data)
    (save-png width height to image-data :grayscale)))
(defun append-image (image-up image-down y-point)
  "Принимает 2 массива изображений и высоту,
   где второе изображение будет наложено на первое.
   Изображения должны быть одинаковой ширины
   и иметь одинаковое количество байт на пиксель.
   Возвращает склеенный массив"
  (destructuring-bind (height-up width-up &optional colors-up)
      (array-dimensions image-up)
    (destructuring-bind (height-down width-down &optional colors-down)
        (array-dimensions image-down)
      (assert (equal width-up width-down))
      (assert (equal colors-up colors-down))
      (let* ((new-height (+ height-down y-point))
             (new-dims (if (null colors-down)
                           (list new-height width-down)
                           (list new-height width-down colors-down)))
             (image-new (make-array new-dims :element-type '(unsigned-byte 8))))
        ;; макрос для прохода по блоку точек
        (macrolet ((cycle ((py px height width &optional &body newline)
                           &body body)
                     `(do ((qy ,py (incf qy)))
                          ((= qy ,height))
                        (do ((qx ,px (incf qx)))
                            ((= qx ,width))
                          ,@body)
                        ,@newline)))
          ;; копируем первую картинку в новый массив
          ;; от ее начала до точки склейки, или до ее конца,
          ;; смотря что случиться раньше.
          ;; Если конец картинки случится раньше, то между
          ;; изображениями будет пустой блок
          (if (null colors-up)
              (cycle (0 0 (min height-up y-point) width-up)
                     (setf (aref image-new qy qx)
                           (aref image-up qy qx)))
              ;; else
              (cycle (0 0 (min height-up y-point) width-up)
                     (do ((qz 0 (incf qz)))
                         ((= qz colors-up))
                       (setf (aref image-new qy qx qz)
                             (aref image-up qy qx qz)))))
          ;; копируем вторую картинку в новый массив
          ;; от ее начала до конца
          (if (null colors-down)
              (let ((new-y y-point))
                (cycle (0 0 height-down width-down (incf new-y))
                       (setf (aref image-new new-y qx)
                             (aref image-up qy qx))))
              ;; else
              (let ((new-y y-point))
                (cycle (0 0 height-down width-down (incf new-y))
                       (do ((rz 0 (incf rz)))
                           ((= rz colors-down))
                         (setf (aref image-new new-y qx rz)
                               (aref image-up qy qx rz)))))))
        image-new))))

;; (block test-append-image-fullcolor
;;   (let* ((arr1 (x-snapshot :x 0 :y 0 :width 755 :height 300))
;;          (arr2 (x-snapshot :x 0 :y 0 :width 755 :height 300))
;;          (array (append-image arr1 arr2 400)))
;;     (destructuring-bind (height width  &rest rest)
;;         (array-dimensions array)
;;       (save-png width height "~/Pictures/result.png" array))))


;; (block test-append-image-grayscale
;;   (let* ((arr1 (binarization (x-snapshot :x 0 :y 0 :width 755 :height 300)))
;;          (arr2 (binarization (x-snapshot :x 0 :y 0 :width 755 :height 300)))
;;          (array (append-image arr1 arr2 200)))
;;     (destructuring-bind (height width  &rest rest)
;;         (array-dimensions array)
;;       (save-png width height "~/Pictures/result.png" array :grayscale))))
(defun append-xor (image-up image-down y-point)
  "Принимает 2 массива изображений и высоту,
   где второе изображение будет наложено на первое
   с помощью XOR.
   Изображения должны быть одинаковой ширины
   и иметь одинаковое количество байт на пиксель.
   Возвращает склеенный массив"
  (destructuring-bind (height-up width-up &optional colors-up)
      (array-dimensions image-up)
    (destructuring-bind (height-down width-down &optional colors-down)
        (array-dimensions image-down)
      (assert (equal width-up width-down))
      (assert (equal colors-up colors-down))
      (let* ((new-height (+ height-down y-point))
             (new-dims (if (null colors-down)
                           (list new-height width-down)
                           (list new-height width-down colors-down)))
             (image-new (make-array new-dims :element-type '(unsigned-byte 8))))
        ;; макрос для прохода по блоку точек
        (macrolet ((cycle ((py px height width &optional &body newline)
                           &body body)
                     `(do ((qy ,py (incf qy)))
                          ((= qy ,height))
                        (do ((qx ,px (incf qx)))
                            ((= qx ,width))
                          ,@body)
                        ,@newline)))
          ;; копируем первую картинку в новый массив
          ;; от ее начала до до ее конца (NB: тут отличие от append-image)
          (if (null colors-up)
              (cycle (0 0 height-up width-up)
                     (setf (aref image-new qy qx)
                           (aref image-up qy qx)))
              ;; else
              (cycle (0 0 height-up width-up)
                     (do ((qz 0 (incf qz)))
                         ((= qz colors-up))
                       (setf (aref image-new qy qx qz)
                             (aref image-up qy qx qz)))))
          ;; xor-им вторую картинку в новый массив
          ;; от ее начала до конца
          (if (null colors-down)
              (let ((new-y y-point))
                (cycle (0 0 height-down width-down (incf new-y))
                       (setf (aref image-new new-y qx)
                             (logxor (aref image-new new-y qx)
                                     (aref image-up qy qx)))))
              ;; else
              (let ((new-y y-point))
                (cycle (0 0 height-down width-down (incf new-y))
                       (do ((rz 0 (incf rz)))
                           ((= rz colors-down))
                         (setf (aref image-new new-y qx rz)
                               (logxor (aref image-new new-y qx rz)
                                       (aref image-up qy qx rz)))))
                ;; поправим излишне поксоренный альфа-канал (если он есть)
                ;; но только там где изображения перекрываются (!)
                (when (equal 4 colors-down)
                  (let ((new-y y-point))
                    (cycle (0 0 (- height-up y-point) width-up (incf new-y))
                           (do ((rz 0 (+ colors-down rz)))
                               ((= rz colors-down))
                             ;; (setf (aref image-new new-y qx (+ 3 rz))
                             ;;       #xFF)
                             ;; проверка правильности заксоривания -
                             ;; можно убрать после отладки
                             ;; (setf (aref image-new new-y qx (+ 2 rz)) #xFF)
                             )))))))
        image-new))))

;; (block test-append-xor-fullcolor
;;   (let* ((arr1 (x-snapshot :x 0 :y 0 :width 755 :height 300))
;;          (arr2 (x-snapshot :x 0 :y 0 :width 755 :height 300))
;;          (array (append-xor arr1 arr2 200)))
;;     (destructuring-bind (height width  &rest rest)
;;         (array-dimensions array)
;;       (save-png width height "~/Pictures/result.png" array))))

;; (block test-append-xor-grayscale
;;   (let* ((arr1 (binarization (x-snapshot :x 0 :y 0 :width 755 :height 300)))
;;          (arr2 (binarization (x-snapshot :x 0 :y 0 :width 755 :height 300)))
;;          (array (append-xor arr1 arr2 200)))
;;     (destructuring-bind (height width  &rest rest)
;;         (array-dimensions array)
;;       (save-png width height "~/Pictures/result.png" array :grayscale))))
(defun analysis (xored-image y-point)
  (destructuring-bind (height width &optional colors)
      (array-dimensions xored-image)
    (let ((intesect-height (- height y-point)) ;; высота пересечения
          (black 0))
      (macrolet ((cycle ((py px height width)
                         &body body)
                   `(do ((qy ,py (incf qy)))
                        ((= qy ,height))
                      (do ((qx ,px (incf qx)))
                          ((= qx ,width))
                        ,@body))))
        (if colors
            (cycle (y-point 0 height width)
                   (when (and (eql (aref xored-image qy qx 0) 0)
                              (eql (aref xored-image qy qx 1) 0)
                              (eql (aref xored-image qy qx 2) 0))
                     (incf black)))
            ;; else
            (cycle (y-point 0 height width)
                   (when (eql (aref xored-image qy qx) 0)
                     (incf black))))
      (let* ((pix-amount (* intesect-height width))
             (result (float (/ black pix-amount))))
        result)))))

(defun get-merge-results (image-up image-down)
  (do ((vy 0 (incf vy)))
      ((= vy (+ (array-dimension image-up 0)
                (array-dimension image-down 0))))
    (format t "~%: =vy: ~A = ~A"
            vy
            (analysis
             (append-xor image-up image-down vy)
             vy))))

;; (block test-merge-results-fullcolor
;;   (time
;;    (let* ((arr1 (x-snapshot :x 0 :y 0 :width 192 :height 108))
;;           (arr2 (x-snapshot :x 0 :y 0 :width 192 :height 108)))
;;      (get-merge-results arr1 arr2))))

;; (block test-merge-results-grayscale
;;   (time
;;    (let* ((arr1 (binarization (x-snapshot :x 0 :y 0 :width 755 :height 300)))
;;           (arr2 (binarization (x-snapshot :x 0 :y 0 :width 755 :height 300))))
;;      (get-merge-results arr1 arr2))))