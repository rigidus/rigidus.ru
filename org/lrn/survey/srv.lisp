(ql:quickload "bordeaux-threads")
(ql:quickload "clx")
(ql:quickload "zpng")
(ql:quickload "png-read")

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

(defun call-subscribers (subscribers)
  (loop :for subscriber :in subscribers
     :do (funcall subscriber)))

(defparameter *shot-queue* nil)

;; list of subscribers for `shot-func'
(defparameter *img-packer*
  (lambda ()
    ;; img_packer_contents
    (let ((img  (car (last *shot-queue*)))
          (file (format nil "~A" (gensym "FILE"))))
      (setf *shot-queue*  (nbutlast *shot-queue*))
      (save-png *default-width* *default-height* file
                (binarization img 127) :grayscale)
      (format t "~%::img-packer-stub ~A~%" file)
      (force-output))
    ;; end - no subscribers
    ))

(defparameter *shot-subscribers* (list *img-packer*))

(defparameter *shot-func*
  (lambda ()
    ;; shot_func_contents:
    (push (x-snapshot) *shot-queue*)
    (format t "~%::shot-func-stub ~A" (length *shot-queue*))
    (force-output)
    ;; Call for all subscibers
    (call-subscribers *shot-subscribers*)
    (schedule-timer *shot-timer* 1 :absolute-p nil)))

(defparameter *shot-timer*
  (make-timer #'(lambda ()
                  (funcall *shot-func*))
              :name "shot" :thread t))

;; (schedule-timer *shot-timer* 0.5)
