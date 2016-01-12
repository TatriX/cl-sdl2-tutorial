(defpackage #:sdl-tutorial-3
  (:use :common-lisp :sdl2)
  (:export :main))

(in-package :sdl-tutorial-3)

(defparameter *screen-width* 640)
(defparameter *screen-height* 480)

(defmacro with-window-and-renderer (window renderer &body body)
  `(sdl2:with-init (:video)
    (sdl2:with-window (,window :title "SDL2 Tutorial" :w *screen-width* :h *screen-height*)
      (sdl2:with-renderer (,renderer ,window)
        ,@body))))

(defun main()
  (with-window-and-renderer window renderer
    (let* ((image (sdl2:load-bmp "exit.bmp"))
           (texture (sdl2:create-texture-from-surface renderer image)))
      (sdl2:with-event-loop (:method :poll)
        (:quit () t)
        (:idle ()
         (sdl2:render-copy renderer texture)
         (sdl2:render-present renderer)))
      (sdl2:destroy-texture texture)
      (sdl2:free-surface image))))
