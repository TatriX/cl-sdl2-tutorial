(defpackage #:sdl-tutorial-2
  (:use :common-lisp)
  (:export :main))

(in-package :sdl-tutorial-2)

(defparameter *screen-width* 640)
(defparameter *screen-height* 480)

(defmacro with-window-surface ((window surface) &body body)
  `(sdl2:with-init (:video)
     (sdl2:with-window (,window
                        :title "SDL2 Tutorial"
                        :w *screen-width*
                        :h *screen-height*
                        :flags '(:shown))
       (let ((,surface (sdl2:get-window-surface ,window)))
         ,@body))))

(defun main(&key (delay 2000))
  (with-window-surface (window screen-surface)
    (sdl2:blit-surface (sdl2:load-bmp "2/hello.bmp")
                       nil
                       screen-surface
                       nil)
    (sdl2:update-window window)
    (sdl2:delay delay)))

