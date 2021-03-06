;;;; package.lisp
;;;;
;;;; Copyright (c) 2016 Jeremiah LaRocco <jeremiah.larocco@gmail.com>

(defpackage #:harmonograph
  (:use #:cl :anim-utils)
  (:export
   #:from-mp3
   #:make-transition-value
   #:create-harmonograph
   #:advance-harmonograph
   #:random-harmonograph
   #:cairo-draw-harmonograph
   #:make-damped-sine
   #:deep-copy-damped-sine
   #:harmonograph-steps
   #:harmonograph-dt
   #:harmonograph-dt-step
   ))

