(defsystem "sdl2-tutorial"
  :description "SDL2 Tutorial based upon http://lazyfoo.net/tutorials/SDL"
  :version "0.0.1"
  :licence "Public Domain"
  :depends-on ("bordeaux-threads" "sdl2" "sdl2-image" "sdl2-ttf")
  :components ((:file "01-hello-sdl")
               (:file "02-getting-an-image-on-the-screen")
               (:file "03-event-driven-programming")
               (:file "04-key-presses")
               (:file "05-optimized-surface-loading-and-soft-stretching")
               (:file "06-extension-libraries-and-loading-other-image-formats")
               (:file "07-texture-loading-and-rendering")
               (:file "08-geometry-rendering")
               (:file "09-the-viewport")
               (:file "10-color-keying")
               (:file "11/tutorial-11")
               (:file "12/tutorial-12")
               (:file "13/tutorial-13")
               (:file "14/tutorial-14")
               (:file "15/tutorial-15")
               (:file "16/tutorial-16")))
