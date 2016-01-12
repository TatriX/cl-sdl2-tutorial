(defpackage #:sdl-tutorial-2
  (:use :common-lisp :sdl2)
  (:export :c-main :main))

(in-package :sdl-tutorial-2)

(defparameter *screen-width* 640)
(defparameter *screen-height* 480)

(defmacro with-window-and-renderer (window renderer &body body)
  `(sdl2:with-init (:video)
    (sdl2:with-window (,window :title "SDL2 Tutorial" :w *screen-width* :h *screen-height*)
      (sdl2:with-renderer (,renderer ,window)
        ,@body))))

(defun main(&key (delay 2000))
  (with-window-and-renderer window renderer
    (let* ((image (sdl2:load-bmp "hello.bmp"))
           (texture (sdl2:create-texture-from-surface renderer image)))
      (sdl2:render-copy renderer texture)
      (sdl2:render-present renderer)
      (sdl2:delay delay)
      (sdl2:destroy-texture texture)
      (sdl2:free-surface image))))
