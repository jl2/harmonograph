;;;; harmonograph.asd
;;;;
;;;; Copyright (c) 2016 Jeremiah LaRocco <jeremiah.larocco@gmail.com>

(asdf:defsystem #:harmonograph
  :description "Describe harmonograph here"
  :author "Jeremiah LaRocco <jeremiah.larocco@gmail.com>"
  :license "ISC (BSD-like)"
  :depends-on (#:cl-cairo2
               #:lparallel
               #:mpg123-ffi
               #:bordeaux-fft
               #:uiop
               #:anim-utils
               #:cl-spark)
  :serial t
  :components ((:file "package")
               (:file "harmonograph")))

