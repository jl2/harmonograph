;;;; harmonograph.lisp
;;;;
;;;; Copyright (c) 2016 Jeremiah LaRocco <jeremiah.larocco@gmail.com>

(in-package #:harmonograph)

(defstruct damped-sine
  (amplitude (make-transition-value :current 1.0d0))
  (frequency (make-transition-value :current 1.0d0))
  (phase (make-transition-value :current 1.0d0))
  (damping (make-transition-value :current 1.0d0)))

(defstruct damped-pendulum
  (first (make-damped-sine))
  (second (make-damped-sine)))

(defstruct harmonograph
  (steps 400 :type (unsigned-byte 32))
  (dt (/ pi 200.0d0) :type double-float)
  (dt-step 120 :type (unsigned-byte 32))
  (x-dim (make-damped-pendulum))
  (y-dim (make-damped-pendulum)))


(defmethod deep-copy ((ds damped-sine))
  (make-damped-sine 
   :amplitude (deep-copy (damped-sine-amplitude ds))
   :frequency (deep-copy (damped-sine-frequency ds))
   :phase (deep-copy (damped-sine-phase ds))
   :damping (deep-copy (damped-sine-damping ds))))

(defmethod deep-copy ((dp damped-pendulum))
  (make-damped-pendulum
   :first (deep-copy (damped-pendulum-first dp))
   :second (deep-copy (damped-pendulum-second dp))))

(defmethod deep-copy ((harm harmonograph))
  (make-harmonograph :steps (harmonograph-steps harm)
                     :dt (harmonograph-dt harm)
                     :dt-step (harmonograph-dt-step harm)
                     :x-dim (deep-copy (harmonograph-x-dim harm))
                     :y-dim (deep-copy (harmonograph-y-dim harm))))

(defun create-harmonograph
    (&key
       (steps 200) 
       (dt (/ 5000.0))
       (dt-step 120)
       a1 f1 p1 d1
       a2 f2 p2 d2
       a3 f3 p3 d3
       a4 f4 p4 d4)
  (make-harmonograph
   :steps steps :dt dt
   :x-dim (make-damped-pendulum :first (make-damped-sine :amplitude a1 :frequency f1 :phase p1 :damping d1)
                                :second (make-damped-sine :amplitude a2 :frequency f2 :phase p2 :damping d2))
   :y-dim (make-damped-pendulum :first (make-damped-sine :amplitude a3 :frequency f3 :phase p3 :damping d3)
                                :second (make-damped-sine :amplitude a4 :frequency f4 :phase p4 :damping d4))))

  
(defun random-harmonograph (&key (steps 400) (dt (/ pi 4000)) (dt-step 120))
  (let (
        (a-min -50.0d0)
        (a-max 50.0d0)
        (a-lower-min 1)
        (a-lower-max 10)
        (a-upper-min 11)
        (a-upper-max 20)
        (a-scale-min 0.0001d0)
        (a-scale-max 0.005d0)

        (f-min 50.0d0)
        (f-max 200.0d0)
        (f-lower-min 1)
        (f-lower-max 10)
        (f-upper-min 11)
        (f-upper-max 20)
        (f-scale-min 0.0001d0)
        (f-scale-max 0.05d0)

        (p-min (- pi))
        (p-max pi)
        (p-lower-min 1)
        (p-lower-max 10)
        (p-upper-min 11)
        (p-upper-max 20)
        (p-scale-min 0.01d0)
        (p-scale-max 0.5d0)

        (d-min 0.0d0)
        (d-max 0.05d0)
        (d-lower-min 15)
        (d-lower-max 20)
        (d-upper-min 21)
        (d-upper-max 25)
        (d-scale 0.001d0))

    (create-harmonograph
     :steps steps
     :dt dt
     :dt-step dt-step
     :a1 (harmonograph:make-transition-value :current (random-between a-min  a-max)
                                             :lower (random-between a-lower-min a-lower-max) 
                                             :upper (random-between a-upper-min a-upper-max)
                                             :scale (random-between a-scale-min a-scale-max))
     :a2 (harmonograph:make-transition-value :current (random-between a-min  a-max)
                                             :lower (random-between a-lower-min a-lower-max) 
                                             :upper (random-between a-upper-min a-upper-max)
                                             :scale (random-between a-scale-min a-scale-max))
     :a3 (harmonograph:make-transition-value :current (random-between a-min  a-max)
                                             :lower (random-between a-lower-min a-lower-max) 
                                             :upper (random-between a-upper-min a-upper-max)
                                             :scale (random-between a-scale-min a-scale-max))
     :a4 (harmonograph:make-transition-value :current (random-between a-min  a-max)
                                             :lower (random-between a-lower-min a-lower-max) 
                                             :upper (random-between a-upper-min a-upper-max)
                                             :scale (random-between a-scale-min a-scale-max))

     :f1 (harmonograph:make-transition-value :current (random-between f-min f-max)
                                             :lower (random-between f-lower-min f-lower-max) 
                                             :upper (random-between f-upper-min f-upper-max)
                                             :scale (random-between f-scale-min f-scale-max))
     :f2 (harmonograph:make-transition-value :current (random-between f-min f-max)
                                             :lower (random-between f-lower-min f-lower-max) 
                                             :upper (random-between f-upper-min f-upper-max)
                                             :scale (random-between f-scale-min f-scale-max))
     :f3 (harmonograph:make-transition-value :current (random-between f-min f-max)
                                             :lower (random-between f-lower-min f-lower-max) 
                                             :upper (random-between f-upper-min f-upper-max)
                                             :scale (random-between f-scale-min f-scale-max))
     :f4 (harmonograph:make-transition-value :current (random-between f-min f-max)
                                             :lower (random-between f-lower-min f-lower-max) 
                                             :upper (random-between f-upper-min f-upper-max)
                                             :scale (random-between f-scale-min f-scale-max))

     :p1 (harmonograph:make-transition-value :current (random-between p-min p-max)
                                             :lower (random-between p-lower-min p-lower-max) 
                                             :upper (random-between p-upper-min p-upper-max)
                                             :scale (random-between p-scale-min p-scale-max))
     :p2 (harmonograph:make-transition-value :current (random-between p-min p-max)
                                             :lower (random-between p-lower-min p-lower-max) 
                                             :upper (random-between p-upper-min p-upper-max)
                                             :scale (random-between p-scale-min p-scale-max))
     :p3 (harmonograph:make-transition-value :current (random-between p-min p-max)
                                             :lower (random-between p-lower-min p-lower-max) 
                                             :upper (random-between p-upper-min p-upper-max)
                                             :scale (random-between p-scale-min p-scale-max))
     :p4 (harmonograph:make-transition-value :current (random-between p-min p-max)
                                             :lower (random-between p-lower-min p-lower-max) 
                                             :upper (random-between p-upper-min p-upper-max)
                                             :scale (random-between p-scale-min p-scale-max))


     :d1 (harmonograph:make-transition-value :current (random-between d-min d-max)
                                             :lower (random-between d-lower-min d-lower-max) 
                                             :upper (random-between d-upper-min d-upper-max)
                                             :scale d-scale)
     :d2 (harmonograph:make-transition-value :current (random-between d-min d-max)
                                             :lower (random-between d-lower-min d-lower-max) 
                                             :upper (random-between d-upper-min d-upper-max)
                                             :scale d-scale)
     :d3 (harmonograph:make-transition-value :current (random-between d-min d-max)
                                             :lower (random-between d-lower-min d-lower-max) 
                                             :upper (random-between d-upper-min d-upper-max)
                                             :scale d-scale)
     :d4 (harmonograph:make-transition-value :current (random-between d-min d-max)
                                             :lower (random-between d-lower-min d-lower-max) 
                                             :upper (random-between d-upper-min d-upper-max)
                                             :scale d-scale)
     )))

(defun advance-sine (sine fft-data)
  (transition-value-advance-value (damped-sine-amplitude sine) fft-data)
  (transition-value-advance-value (damped-sine-frequency sine) fft-data)
  (transition-value-advance-value (damped-sine-phase sine) fft-data)
  (transition-value-advance-value (damped-sine-damping sine) fft-data))

(defun advance-pendulum (pend fft-data)
  (advance-sine (damped-pendulum-first pend) fft-data)
  (advance-sine (damped-pendulum-second pend) fft-data))

(defun advance-harmonograph (harm left-fft-data right-fft-data)
  (advance-pendulum (harmonograph-x-dim harm) left-fft-data)
  (advance-pendulum (harmonograph-y-dim harm) right-fft-data))

(defun compute-harmonograph (harm tval)
  (cons 
   (+ (* (gv (damped-sine-amplitude (damped-pendulum-first (harmonograph-x-dim harm))))
         (sin (+ (* tval (gv (damped-sine-frequency (damped-pendulum-first (harmonograph-x-dim harm)))))
                 (gv (damped-sine-frequency (damped-pendulum-first (harmonograph-x-dim harm))))))
         (exp (* -1.0 tval (gv (damped-sine-damping (damped-pendulum-first (harmonograph-x-dim harm))))))
         )

      (* (gv (damped-sine-amplitude (damped-pendulum-second (harmonograph-x-dim harm))))
         (sin (+ (* tval (gv (damped-sine-frequency (damped-pendulum-second (harmonograph-x-dim harm)))))
                 (gv (damped-sine-frequency (damped-pendulum-second (harmonograph-x-dim harm))))))
         (exp (* -1.0 tval (gv (damped-sine-damping (damped-pendulum-second (harmonograph-x-dim harm))))))
         ))

   (+ (* (gv (damped-sine-amplitude (damped-pendulum-first (harmonograph-y-dim harm))))
         (sin (+ (* tval (gv (damped-sine-frequency (damped-pendulum-first (harmonograph-y-dim harm)))))
                 (gv (damped-sine-frequency (damped-pendulum-first (harmonograph-y-dim harm))))))
         (exp (* -1.0 tval (gv (damped-sine-damping (damped-pendulum-first (harmonograph-y-dim harm))))))
         )

      (* (gv (damped-sine-amplitude (damped-pendulum-second (harmonograph-y-dim harm))))
         (sin (+ (* tval (gv (damped-sine-frequency (damped-pendulum-second (harmonograph-y-dim harm)))))
                 (gv (damped-sine-frequency (damped-pendulum-second (harmonograph-y-dim harm))))))
         (exp (* -1.0 tval (gv (damped-sine-damping (damped-pendulum-second (harmonograph-y-dim harm))))))
         ))))

;; map-val is used to map logical coordinates to screen coordinates.
(defun map-val (x xmin xmax new-xmin new-xmax)
  "map a value from the range xmin,xmax to the range new-xmin,new-xmax"
  (+ (* (/ (- x xmin) (- xmax xmin)) (- new-xmax new-xmin)) new-xmin))

(defun inner-draw-harmonograph (harm width height line-function
                                color-function)

  ;; set max-radius to an estimate on the maximum radius of the curve, given the
  ;; current values of a, b, and h.
  (let ((x-max-radius (* 1.15d0 (+
                                 (abs (gv (damped-sine-amplitude (damped-pendulum-first (harmonograph-x-dim harm)))))
                                 (abs (gv (damped-sine-amplitude (damped-pendulum-second (harmonograph-x-dim harm))))))))
        (y-max-radius (* 1.15d0 (+
                                 (abs (gv (damped-sine-amplitude (damped-pendulum-first (harmonograph-y-dim harm)))))
                                 (abs (gv (damped-sine-amplitude (damped-pendulum-second (harmonograph-y-dim harm))))))))

        (x-aspect-ratio (if (< height width)
                            (/ height width 1.0)
                            1.0))
        (y-aspect-ratio (if (< height width)
                            1.0
                            (/ width height 1.0))))
    
    ;; define some local functions for convenience
    (flet (
           ;; xmapper maps logical x coordinates in the range x-min to x-max to
           ;; screen coordinates in the range 0 to width
           (xmapper (x) (map-val (* x-aspect-ratio x) (- x-max-radius) x-max-radius 0 width))

           ;; ymapper does the same thing, but for y coordinates
           (ymapper (y) (map-val (* y-aspect-ratio y) (- y-max-radius) y-max-radius 0 height)))
           
      ;; draw the curve
      (loop
         for i below (harmonograph-steps harm)
         for cur-t = 0.0 then (+ cur-t (harmonograph-dt harm))
         do
           (funcall
            color-function
            0.0
            (+ 0.3 (/ (* i 0.6) (harmonograph-steps harm)))
            0.0
            0.95)
           (let ((first-points (compute-harmonograph harm cur-t))
                 (second-points (compute-harmonograph harm (+ (* (harmonograph-dt-step harm) (harmonograph-dt harm)) cur-t))))
           (funcall
            line-function
            (truncate (xmapper (car first-points)))
            (truncate (ymapper (cdr first-points)))
            (truncate (xmapper (car second-points)))
            (truncate (ymapper (cdr second-points)))))))))

(defun cairo-draw-harmonograph (file-name harm width height)
  "use cairo to draw a spirograph, saving in the specified file."
  (cl-cairo2:with-png-file (file-name :argb32 width height)

    (cl-cairo2:set-source-rgba 0.0 0.0 0.0 1.0)
    (cl-cairo2:paint)
    
    (cl-cairo2:scale 1 1)
    (cl-cairo2:set-line-width 1.25)
    (cl-cairo2:set-source-rgba 0.0 0.0 0.8 0.95)
    (flet ((cairo-line (x1 y1 x2 y2)
             (cl-cairo2:move-to x1 y1)
             (cl-cairo2:line-to x2 y2)
             (cl-cairo2:stroke)))


      (inner-draw-harmonograph harm width height
                               #'cairo-line #'cl-cairo2:set-source-rgba))))
(defun from-mp3 (&key
                   mp3-file-name output-directory
                   (harmonograph (random-harmonograph))
                   (movie-file-name "mp3-animation.mpg")
                   (keep-pngs nil)
                   (keep-soundless nil)
                   (width 800) (height 800)
                   (bit-rate (* 4 1024))
                   (fps 30)
                   (verbose t)
                   (threads 4)
                   (fft-window-size 1024)
                   (movie-duration nil)
                   (temp-movie-name "temporary.mpg"))
  "Generate an animation from an MP3 file."
  
  (let* ((real-dir-name (fix-directory output-directory))

         (mp3-file (read-mp3-file mp3-file-name))

         (song-duration (mp3-file-duration-in-seconds mp3-file))
         (real-movie-duration (if movie-duration
                                  (min song-duration movie-duration)
                                  song-duration))
         
         (total-frames (ceiling (* real-movie-duration fps)))

         (files-created nil)
         (harmonographs nil)
         (real-harmonograph (deep-copy harmonograph))
         (full-movie-name (format nil "~a~a" real-dir-name movie-file-name))
         (full-temp-movie-name (format nil "~a~a" real-dir-name temp-movie-name))

         (kernel (lparallel:make-kernel threads))
         (futures nil))
    
    (when verbose (format t "Creating animation with ~a frames." total-frames))

    (dotimes (cur-frame total-frames)
      (let* ((file-name (format nil
                                "~aframe~5,'0d.png" real-dir-name cur-frame))
             
             (win-center (ceiling (* 44100 (interpolate 0.0 song-duration
                                                        cur-frame total-frames))))
             (left-fft-data (bordeaux-fft:windowed-fft (mp3-file-left-channel mp3-file) win-center fft-window-size))
             (right-fft-data (bordeaux-fft:windowed-fft (mp3-file-right-channel mp3-file) win-center fft-window-size)))

        (advance-harmonograph real-harmonograph left-fft-data right-fft-data)

        ;; (when verbose (format t "~a~%" spiro))
        (push (cons (deep-copy real-harmonograph) file-name) harmonographs)
        (push file-name files-created)))

    (format t "Last harmonograph:~% ~a~%" real-harmonograph)

    (setf lparallel:*kernel* kernel)
    (unwind-protect

         (dolist (harm harmonographs)
           (push (lparallel:future
                   ;; (format t "Drawing ~a~%" harm)
                   (cairo-draw-harmonograph (cdr harm)
                                            (car harm)
                                            width height)
                   (push (cdr harm) files-created))
                 futures))
      (when futures (dolist (fut futures) (lparallel:force fut)))
      (when kernel (lparallel:end-kernel :wait t)))

    (make-movie :directory real-dir-name
                :image-type "png"
                :mp3-name mp3-file-name
                :file-name full-movie-name
                :remove-temp (not keep-soundless) 
                :temp-name full-temp-movie-name
                :bit-rate bit-rate)
  
    (if (not keep-pngs)
        (dolist (fname files-created)
          (if (probe-file fname)
              (delete-file fname)))))
  harmonograph)

