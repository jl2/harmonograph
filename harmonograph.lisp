;;;; harmonograph.lisp
;;;;
;;;; Copyright (c) 2016 Jeremiah LaRocco <jeremiah.larocco@gmail.com>

(in-package #:harmonograph)

(defstruct transition-value
  (current 0.0d0 :type double-float)
  (lower 1 :type (unsigned-byte 32))
  (upper 2 :type (unsigned-byte 32))
  (scale 1.0d0 :type double-float))

(defun deep-copy-transition-value (tv)
  (make-transition-value :current (transition-value-current tv)
                         :lower (transition-value-lower tv)
                         :upper (transition-value-upper tv)
                         :scale (transition-value-scale tv)))


(defun gv (tv)
  (transition-value-current tv))

(defun advance-value (trans fft-data)
  (incf (transition-value-current trans)
     (loop for idx from (transition-value-lower trans) below (transition-value-upper trans)
        summing (aref fft-data idx) into total
        finally (return (* (transition-value-scale trans) (/ (abs total) (- (transition-value-upper trans) (transition-value-lower trans))))))))

(defun interpolate (a b cur-step steps &optional (looping nil))
  "Linearly interpolate between a and b over a number of steps.
   If looping is t, interpolates between a and b when cur-step is less than 
   steps/2, and between b and a when cur-step is greater than steps/2."
  (if (not looping)
      (let ((da (/ (- b a) steps)))
        (+ a (* da cur-step)))
      (if (< cur-step (/ steps 2))
          (let ((da (/ (- b a) (/ steps 2))))
            (+ a (* da cur-step)))
          (let ((da (/ (- a b) (/ steps 2))))
            (+ b (* da (- cur-step (/ steps 2)) ))))))

(defstruct mp3-file
  (left-channel)
  (right-channel)
  (samples)
  (sample-rate 44100 :type (unsigned-byte 32))
  (channels 2 :type (unsigned-byte 32))
  (mpg123-type 208 :type (unsigned-byte 32)))

(defun duration-in-seconds (mp3)
  "Compute the duration of an mp3-file in seconds."
  (/ (length (mp3-file-samples mp3)) 
     (* (mp3-file-channels mp3) (mp3-file-sample-rate mp3))))

(defun next-power-of-2 (num)
  (loop
     for power from 0
     for cn = num then (floor (/ cn 2))
     until (< cn 2)
     finally (return (ash 1 (+ 1 power)))))


(defun read-mp3-file (fname)
  "Read the specified mp3 file into an mp3-file structure."
  (multiple-value-bind
        (samples sample-rate channels mt)
      (mpg123:decode-mp3-file fname :character-encoding :utf-8)
    
    (let* ((samples-per-channel (/ (length samples) channels))
           (left-channel (make-array samples-per-channel
                                     :element-type '(complex double-float)
                                     :initial-element (coerce 0.0 '(complex double-float))))
           (right-channel (make-array samples-per-channel
                                      :element-type '(complex double-float)
                                      :initial-element (coerce 0.0 '(complex double-float)))))
      (loop for i below samples-per-channel
         do
           (let ((left-raw (/ (aref samples (* 2 i)) 32768.0))
                 (right-raw (/ (aref samples (+ 1 (* 2 i))) 32768.0)))
             
             (setf (aref left-channel i)
                   (coerce left-raw '(complex double-float)))
             (setf (aref right-channel i)
                   (coerce right-raw '(complex double-float)))))
      (make-mp3-file :samples samples
                     :left-channel left-channel
                     :right-channel right-channel
                     :sample-rate sample-rate
                     :channels channels
                     :mpg123-type mt))))

(defun make-movie (&key directory file-name
                     (mp3-name nil)
                     (bit-rate (* 4 2014))
                     (temp-name "tmpmovie.mpg")
                     (image-type "png")
                     (remove-temp t))
  "Run ffmpeg to create a movie with audio."
  (if (probe-file temp-name)
      (delete-file temp-name))

  (let ((movie-command
         (format nil 
                 "ffmpeg -r 30 -i \"~aframe%05d.~a\" -b ~a -q 4 \"~a\""
                 directory image-type bit-rate temp-name))
        (audio-command
         (format nil
                 "ffmpeg -i \"~a\" -i \"~a\" -codec copy -shortest \"~a\""
                 temp-name mp3-name file-name))
        (mv-command
         (format nil "mv \"~a\" \"~a\"" temp-name file-name)))
    
    (format t "~a~%" movie-command)
    (uiop:run-program movie-command)

    (if (probe-file file-name)
        (delete-file file-name))

    (if mp3-name
        (progn 
          (format t "~a~%" audio-command)
          (uiop:run-program audio-command))
        (progn
          (format t "~a~%" mv-command)
          (uiop:run-program mv-command)))

    (if remove-temp
        (delete-file temp-name))))

(defun fix-directory (directory-name)
  "Make sure directory exists and has a / at the end."
  (ensure-directories-exist
   (if (char=  #\/ (aref directory-name (- (length directory-name) 1)))
       directory-name 
       (concatenate 'string directory-name "/"))))


(defstruct damped-sine
  (amplitude (make-transition-value :current 1.0d0))
  (frequency (make-transition-value :current 1.0d0))
  (phase (make-transition-value :current 1.0d0))
  (damping (make-transition-value :current 1.0d0)))

(defstruct damped-pendulum
  (first (make-damped-sine))
  (second (make-damped-sine)))

(defstruct harmonograph
  (steps 2000 :type (unsigned-byte 32))
  (dt (/ (* 80 pi) 2000.0))
  (x-dim (make-damped-pendulum))
  (y-dim (make-damped-pendulum)))


(defun deep-copy-damped-sine (ds)
  (make-damped-sine 
   :amplitude (deep-copy-transition-value (damped-sine-amplitude ds))
   :frequency (deep-copy-transition-value (damped-sine-frequency ds))
   :phase (deep-copy-transition-value (damped-sine-phase ds))
   :damping (deep-copy-transition-value (damped-sine-damping ds))))

(defun deep-copy-damped-pendulum (dp)
  (make-damped-pendulum
   :first (deep-copy-damped-sine (damped-pendulum-first dp))
   :second (deep-copy-damped-sine (damped-pendulum-second dp))))

(defun deep-copy-harmonograph (harm)
  (make-harmonograph :steps (harmonograph-steps harm)
                     :dt (harmonograph-dt harm)
                     :x-dim (deep-copy-damped-pendulum (harmonograph-x-dim harm))
                     :y-dim (deep-copy-damped-pendulum (harmonograph-y-dim harm))))

(defun create-harmonograph
    (&key
       (steps 2000) 
       (dt (/ (* 80 pi) 2000.0))
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

(defun random-double (dmin dmax)
  (+ dmin (random (- dmax dmin))))

(defun random-integer (imin imax)
  (+ imin (random (- imax imin))))

(defun random-harmonograph (&key (steps 5000) (dt (/ pi 5000)))
  (create-harmonograph
   :steps steps
   :dt dt
   :a1 (harmonograph:make-transition-value :current (random-double -50.0d0 50.0d0)
                                           :lower (random-integer 1 10) 
                                           :upper (random-integer 11 20)
                                           :scale (random-double 0.001d0 0.05d0))
   :a2 (harmonograph:make-transition-value :current (random-double -50.0d0 50.0d0)
                                           :lower (random-integer 1 10) 
                                           :upper (random-integer 11 20)
                                           :scale (random-double 0.001d0 0.05d0))
   :a3 (harmonograph:make-transition-value :current (random-double -50.0d0 50.0d0)
                                           :lower (random-integer 1 10) 
                                           :upper (random-integer 11 20)
                                           :scale (random-double 0.001d0 0.05d0))
   
   :a4 (harmonograph:make-transition-value :current (random-double -50.0d0 50.0d0)
                                           :lower (random-integer 1 10) 
                                           :upper (random-integer 11 20)
                                           :scale (random-double 0.001d0 0.05d0))

   :f1 (harmonograph:make-transition-value :current (random-double 50.0d0 1000.0d0)
                                           :lower (random-integer 1 10) 
                                           :upper (random-integer 11 20)
                                           :scale (random-double 0.001d0 0.08d0))

   :f2 (harmonograph:make-transition-value :current (random-double 50.0d0 1000.0d0)
                                           :lower (random-integer 1 10) 
                                           :upper (random-integer 11 20)
                                           :scale (random-double 0.001d0 0.08d0))

   :f3 (harmonograph:make-transition-value :current (random-double 50.0d0 1000.0d0)
                                           :lower (random-integer 1 10) 
                                           :upper (random-integer 11 20)
                                           :scale (random-double 0.001d0 0.1d0))
   :f4 (harmonograph:make-transition-value :current (random-double 50.0d0 1000.0d0)
                                           :lower (random-integer 1 10) 
                                           :upper (random-integer 11 20)
                                           :scale (random-double 0.001d0 0.08d0))

   :p1 (harmonograph:make-transition-value :current (random-double (- pi) pi)
                                           :lower (random-integer 1 10) 
                                           :upper (random-integer 11 20)
                                           :scale (random-double 0.001d0 0.05d0))

   :p2 (harmonograph:make-transition-value :current (random-double (- pi) pi)
                                           :lower (random-integer 1 10) 
                                           :upper (random-integer 11 20)
                                           :scale (random-double 0.001d0 0.05d0))

   :p3 (harmonograph:make-transition-value :current (random-double (- pi) pi)
                                           :lower (random-integer 1 10) 
                                           :upper (random-integer 11 20)
                                           :scale (random-double 0.001d0 0.05d0))

   :p4 (harmonograph:make-transition-value :current (random-double (- pi) pi)
                                           :lower (random-integer 1 10) 
                                           :upper (random-integer 11 20)
                                           :scale (random-double 0.001d0 0.05d0))


   :d1 (harmonograph:make-transition-value :current (random-double 0.95d0 1.0d0)
                                           :lower (random-integer 15 20) 
                                           :upper (random-integer 21 25)
                                           :scale 0.0001d0)

   :d2 (harmonograph:make-transition-value :current (random-double 0.95d0 1.0d0)
                                           :lower (random-integer 15 20) 
                                           :upper (random-integer 21 25)
                                           :scale 0.0001d0)

   :d3 (harmonograph:make-transition-value :current (random-double 0.95d0 1.0d0)
                                           :lower (random-integer 15 20) 
                                           :upper (random-integer 21 25)
                                           :scale 0.0001d0)

   :d4 (harmonograph:make-transition-value :current (random-double 0.95d0 1.0d0)
                                           :lower (random-integer 15 20) 
                                           :upper (random-integer 21 25)
                                           :scale 0.0001d0)
   ))

(defun advance-sine (sine fft-data)
  (advance-value (damped-sine-amplitude sine) fft-data)
  (advance-value (damped-sine-frequency sine) fft-data)
  (advance-value (damped-sine-phase sine) fft-data)
  (advance-value (damped-sine-damping sine) fft-data))

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
                 (second-points (compute-harmonograph harm (+ (harmonograph-dt harm) cur-t))))
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
    (cl-cairo2:set-line-width 0.5)
    (cl-cairo2:set-source-rgba 0.0 0.0 0.8 0.95)
    (flet ((cairo-line (x1 y1 x2 y2)
             (cl-cairo2:move-to x1 y1)
             (cl-cairo2:line-to x2 y2)
             (cl-cairo2:stroke)))


      (inner-draw-harmonograph harm width height
                               #'cairo-line #'cl-cairo2:set-source-rgba))))
(defun from-mp3 (&key
                   mp3-file-name output-directory
                   harmonograph
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

         (song-duration (duration-in-seconds mp3-file))
         (real-movie-duration (if movie-duration
                                  (min song-duration movie-duration)
                                  song-duration))
         
         (total-frames (ceiling (* real-movie-duration fps)))

         (files-created nil)
         (harmonographs nil)

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

        (advance-harmonograph harmonograph left-fft-data right-fft-data)

        ;; (when verbose (format t "~a~%" spiro))
        (push (cons (deep-copy-harmonograph harmonograph) file-name) harmonographs)))

    (format t "Last harmonograph:~% ~a~%" harmonograph)

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
          (delete-file fname)))
    harmonographs))

