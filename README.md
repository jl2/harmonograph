Generate animations based on harmonograph curves that sync to MP3 files.

Example usage:

```commonlisp
    (ql:quickload :harmonograph)
    
    (let ((random-hgraph (harmonograph:random-harmonograph :steps 4000 :dt (/ pi 12000)))
          (mp3-name "/mnt/externalhd/PhotoBackup_backup/my_music/Trazer/Burning/01-10- Floating in Time.mp3")
          (out-dir "/home/jeremiah/harmonographs/sample/"))

    (harmonograph:from-mp3 :harmonograph random-hgraph
                             :mp3-file-name mp3-name
                             :output-directory out-dir
                             :width 1600 :height 1200
                             :bit-rate (* 16 1024)
                             :movie-duration 10))
```

