;;; To see how this file was used, visit: https://medium.com/galileo-onwards/why-is-a-raised-to-zero-one-64966392efbe
;;;
;;; See bottom of file for run examples.
;;;
;;; The code was written to generate images for the blog, not for
;;; performance or maintenance.

;; To load, download skippy https://www.xach.com/lisp/skippy/

;; To load the library in your CL implementation:
;; (let ((asdf:*central-registry* '(#p"/path/to/library/skippy/")))
;;    (asdf:load-system 'skippy))

(defpackage #:loggy
  (:use #:cl #:skippy))

(in-package #:loggy)



(use-package '#:skippy)

(defun example1 ()
  (let* ((height 100)
         (width 100)
         (data-stream (make-data-stream :height height
                                        :width width
                                        :color-table t))
         (image (make-image :height height :width width))
         (red (ensure-color (rgb-color #xFF #x00 #x00)
                            (color-table data-stream)))
         (white (ensure-color (rgb-color #xFF #xFF #xFF)
                              (color-table data-stream))))
    (add-image image data-stream)
    (fill (image-data image) white)
    (dotimes (i (truncate height 2))
      (let* ((start (* i width 2))
             (end (+ start width)))
        (fill (image-data image) red :start start :end end)))
    (output-data-stream data-stream #p"example1.gif")))

(example1)


(defun example2 ()
  (let* ((height 9)
         (width 99)
         (color-table (make-color-table))
         (data-stream (make-data-stream :height height
                                        :width width
                                        :color-table color-table))
         (gray (ensure-color #xCCCCCC color-table))
         (white (ensure-color #xFFFFFF color-table))
         (black (ensure-color #x000000 color-table))
         (bg (make-image :data-stream data-stream
                         :width width :height height
                         :image-data (make-image-data height width
                                                      :initial-element gray)))
         (sprite-data (make-image-data 3 3)))
    (flet ((hatch-data (data color1 color2)
             (dotimes (i (length data))
               (setf (aref data i) (if (zerop (mod i 2)) color1 color2)))))
      (hatch-data sprite-data white black)
      (hatch-data (image-data bg) white gray)
      (dotimes (i 96)
        (let ((image (make-image :height 3
                                 :width 3
                                 :image-data sprite-data
                                 :delay-time 10
                                 :disposal-method :restore-previous
                                 :transparency-index white
                                 :top-position 3
                                 :left-position i)))
          (add-image image data-stream)))
      (setf (loopingp data-stream) t)
      (output-data-stream data-stream #p"example2.gif"))))

(example2)


(defun example3 ()
  (let* ((height 100)
         (width 100)
         (color-count 256)
         (color-table (make-color-table))
         (data-stream (make-data-stream :color-table color-table
                                        :loopingp t
                                        :height height
                                        :width width)))
    (dotimes (i color-count)
      (add-color (rgb-color (random 256) (random 256) (random 256))
                 color-table))
    (dotimes (i color-count)
      (let* ((top (random height))
             (left (random width))
             (h (1+ (random (- height top))))
             (w (1+ (random (- width left))))
             (image (make-image :height h
                                :width w
                                :data-stream data-stream
                                :top-position top
                                :left-position left
                                :image-data (make-image-data w h
                                                             :initial-element (random color-count))
                                :delay-time 5)))
        (add-image image data-stream)))
    (output-data-stream data-stream #p"example3.gif")))

(example3)


(defmethod draw-line ((image image) (color integer)
                      (x0 integer) (y0 integer)
                      (x1 integer) (y1 integer))
  (let ((dx (abs (- x1 x0)))
        (dy (abs (- y1 y0)))
        (sx (if (< x0 x1) 1 -1))
        (sy (if (< y0 y1) 1 -1)))
    (let ((err (/ (if (> dx dy) dx (- dy)) 2))
          (e2 0))
      (loop
         (setf (pixel-ref image x0 y0) color)
         (when (and (= x0 x1) (= y0 y1))
           (return))
         (setq e2 err)
         (when (> e2 (- dx)) (decf err dy) (incf x0 sx))
         (when (< e2 dy) (incf err dx) (incf y0 sy))))))

(defun get-strip-xs (width n)
  "Get points on horizontal axis at which n splits may be evenly drawn."
  (loop with cell-width = (/ width n)
     as i from cell-width by cell-width
     as j from 1 below n
     collecting (round i)))

(defun get-strip-xs-sub1 (width n)
  "Get points on horizontal axis at which n splits may be evenly drawn."
  (loop
     with cell-width = (* width n)
     and count = (/ 1 n)
     as i from cell-width by cell-width
     and j from 1 to count
     while (< (round i) width)
     collecting (round i)))

(defun make-loggy (&key width height extrap)
  (let* ((colors '(#xFFFFFF #x000000 #xFF0000 #x00FFFF #xAAAAAA #x00FF88))
         (data-stream (make-data-stream
                       :width width :height (+ height (if extrap 20 0))
                       :color-table (make-color-table :initial-contents colors)
                       :loopingp 't))
         (image0 (make-image :width width :height (height data-stream)))
         (wid (1- width))
         (hei (1- height))
         (actual-hei (1- (height data-stream)))
         (black 1))
    (draw-line image0 black 0   0   wid 0)
    (draw-line image0 black 0   hei wid hei)
    (draw-line image0 black 0   0   0   hei)
    (draw-line image0 black wid 0   wid hei)
    (when extrap
      (draw-line image0 black 0   hei 0   actual-hei)
      (draw-line image0 black wid hei wid actual-hei)
      (draw-line image0 black 0 actual-hei wid actual-hei))
    (add-image image0 data-stream)
    data-stream))

(defun draw-logging (width height n log)
  (let* ((data-stream (make-loggy :width width :height height))
         (colors (color-table data-stream))
         (strips (get-strip-xs width n))
         (image (aref (images data-stream) 0)))
    (loop as x in strips
       do (draw-line image (ensure-color #xFF0000 colors) x 1 x (- height 2)))
    (output-data-stream data-stream #p"example.gif")
    (labels
        ((draw (n width count)
           (if (< n log)
               (let ((img (make-image :width (1- (car strips)) :height (- height 2)
                                      :top-position 1 :left-position 1
                                      :delay-time 300)))
                 (fill (image-data img) (ensure-color #x00FF00 colors))
                 (add-image img data-stream)
                 (output-data-stream data-stream #P"examplem.gif"))
               (let ((splits (get-strip-xs width log)))
                 ;; draw the splits
                 (loop with line-w = 10
                    for xsplit in splits
                    do (loop as h from 0 below height by 20
                          as img = (make-image :width 1 :height line-w
                                               :disposal-method :none
                                               :top-position h :left-position xsplit
                                               :delay-time 10)
                          do (fill (image-data img) (ensure-color #x00FFFF colors))
                            (add-image img data-stream)))
                 (output-data-stream data-stream (format nil "example-~d.gif" count))
                 ;; gray out
                 (loop with height-step = (truncate height 10)
                    with left-position = (car splits)
                    with filename = (format nil "example_~d.gif" count)
                    as j from 0 below height by height-step
                    with fade-width = (- (width data-stream) left-position)
                    as img = (make-image :width fade-width :height height-step
                                         :disposal-method :none
                                         :top-position j :left-position left-position
                                         :delay-time 50)
                    do (fill (image-data img) (ensure-color #xAAAAAA colors))
                      (add-image img data-stream)
                    finally
                      (format t "writing to ~a~&"
                              (output-data-stream data-stream filename))
                      (draw (truncate n log) left-position (1+ count)))
                 data-stream))))
      (draw n width 1))))

(defun to-2-color (ds)
  "Converts DS to a 2 color image."
  (let ((ct  (color-table ds)))
    (loop as img across (images ds)
       do (loop as i from 0 below (width img)
             do (loop as j from 0 below (height img)
                   as clr = (color-table-entry ct (pixel-ref img i j))
                   as adj = (if (< clr #x800000) 1 0)
                   do (setf (pixel-ref img i j) adj))))
    ;; Now set the first two color table entries to white and black
    (setf (color-table-entry ct 0) #xFFFFFF
          (color-table-entry ct 1) #x000000)
    ds))

(defvar *number-streams*
  (loop
     with table = (make-hash-table :test #'eql)
     and get-img = (lambda (fn)
                     (aref (images (to-2-color (load-data-stream fn))) 0))
     as i across "0123456789"
     as filename = (format nil "~a.gif" i)
     as img = (funcall get-img filename)
     do (setf (gethash i table) img)
     finally
       (setf (gethash #\. table) (funcall get-img "dot.gif"))
       (return table))
  "hashtable of number images")

(defun number-canvas (number &optional (precision 2))
  (let* ((format (if (integerp number)
                     "~d"
                     (format nil "~~,~df" precision)))
         (num (format nil format number))
         (images (loop as d across num
                    collect (gethash d *number-streams*) into images
                    finally (return (apply #'vector images))))
         (ihei (apply #'max (map 'list #'height images)))
         (iwid (apply #'+ (map 'list #'width images)))
         (pixels (loop
                    as j from 0 below ihei
                    ;; collect the pixels of each row
                    nconc (loop as img across images
                             as iwidth = (width img)
                             and iheight = (height img)
                             nconc (loop as i from 0 below iwidth
                                      collect (if (< j iheight)
                                                  (pixel-ref img i j)
                                                  0)))))
         (pixels (make-image-data iwid ihei :initial-contents pixels)))
    (make-canvas :width iwid :height ihei :initial-contents pixels)))

(defun draw-unlogging (width height n log &optional (precision 2))
  (let* ((data-stream (make-loggy :width width :height height :extrap t))
         (colors (color-table data-stream))
         (splits (get-strip-xs-sub1 (- width 2) n))
         (red (ensure-color #xFF0000 colors))
         (greens (map 'vector (lambda (g) (ensure-color (rgb-color 0 g 0) colors))
                      (loop as i from 128 to 255 by (truncate 128 (abs (log n log)))
                         collecting i))))
    ;;; Setup the image
    ;;; draw the lines marking n, n*2, n*3, ... 1
    ;;;
    ;;; draw them on the first image because there's nothing more
    ;;; boring than seeing lines appearing.
    (loop with img = (aref (images data-stream) 0)
       for split in splits
       do (draw-line img red split 1 split (1- height)))

    ;; draw the shaded red area marking what we have
    (let ((img (make-image :width (car splits) :height (- height 2)
                           :disposal-method :none
                           :left-position 1 :top-position 1
                           :delay-time 50)))
      (fill (image-data img) red)
      (add-image img data-stream))

    ;;; draw the number markers
    ;; The numbers aren't optimally placed.  That'd be an interesting
    ;; dynamic programming problem.
    (loop
       with min-gap = 2
       for split in splits
       and i from 1
       as numimg = (canvas-image (number-canvas (* i n) precision))
       as numwidth = (width numimg)
       as xpos = (round (- split (/ numwidth 2)))
       and ypos = (+ height 5)
       with prev-xpos = most-negative-fixnum
       and prev-width = most-negative-fixnum
       do
       ;; no overlapping numbers and no numbers that would exceed
       ;; image width
         (when (and (> xpos (+ prev-xpos prev-width min-gap))
                    (< (+ xpos numwidth) width))
           (setq xpos (max xpos 2))
           (setq prev-xpos xpos
                 prev-width numwidth)
           (setf (left-position numimg) xpos
                 (top-position numimg) ypos
                 (disposal-method numimg) :none
                 (delay-time numimg) 1)
           (add-image numimg data-stream)))
    (loop with glen = (length greens)
       as count from 0
       and i = n then (* i log)
       and prev = 0 then shade-width
       as shade-width = (round (* i (- width 2)))
       as img = (make-image :width (- shade-width prev) :height (- height 2)
                            :disposal-method :none
                            :left-position (1+ prev) :top-position 1
                            :delay-time 200)
       and color = red then (aref greens (mod count glen))
       while (<= i 1)
       do
         (fill (image-data img) color)
         (if (= count 0) (setf (delay-time img) 200))
         (add-image img data-stream)
       finally
         (setf (delay-time img) 500))
    (output-data-stream data-stream #P"unlog.gif")))

;;; To generate a log sample, run:
;; * (draw-logging 800 100 128 2)
;; #P"examplem.gif"

;;; To generate an unlog sample, run:
;; * (draw-unlogging 800 100 1/81 2 10)
;; #P"unlog.gif"
