(defsystem "sdl2-tutorial"
  :description "SDL2 Tutorial based upon http://lazyfoo.net/tutorials/SDL"
  :version "0.2.0"
  :author "TatriX <tatrics@gmail.com>"
  :licence "Public Domain"
  :depends-on ("bordeaux-threads" "sdl2" "sdl2-image" "sdl2-ttf")
  :components ((:file "utils")
               (:file "01-hello-sdl")
               (:file "02-getting-an-image-on-the-screen")
               (:file "03-event-driven-programming")
               (:file "04-key-presses")
               (:file "05-optimized-surface-loading-and-soft-stretching")
               (:file "06-extension-libraries-and-loading-other-image-formats")
               (:file "07-texture-loading-and-rendering")
               (:file "08-geometry-rendering")
               (:file "09-the-viewport")
               (:file "10-color-keying")
               (:file "11-clip-rendering-and-sprite-sheets")
               (:file "12-color-modulation")
               (:file "13-alpha-blending")
               (:file "14/tutorial-14")
               (:file "15/tutorial-15")
               (:file "16/tutorial-16"))
  :in-order-to ((test-op (test-op "sdl2-tutorial/tests"))))


(defsystem "sdl2-tutorial/tests"
  :depends-on ("parachute")
  :components ((:file "tests/tests"))
  :perform (test-op (op c) (symbol-call :parachute :test :sdl2-tutorial-tests)))
