Generate animations based on harmonograph curves that sync to MP3 files.

Example usage:

```commonlisp
    CL-USER> (ql:quickload :harmonograph)
    To load "harmonograph":
      Load 1 ASDF system:
        harmonograph
    ; Loading "harmonograph"
    ...........
    (:HARMONOGRAPH)
    CL-USER> (defparameter *harm* (harmonograph:random-harmonograph :steps 4000 :dt (/ pi 12000)))
    *HARM*
    CL-USER> (harmonograph:from-mp3 :harmonograph harm :mp3-file-name "/mnt/externalhd/PhotoBackup_backup/my_music/Trazer/Burning/01-10- Floating in Time.mp3" :output-directory "/home/jeremiah/harmonographs/sample/" :keep-pngs nil :bit-rate (* 16 1024) :width 1600 :height 1200 :movie-duration 10)
    ;; ...
    CL-USER> 
```

