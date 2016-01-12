(defpackage #:sdl-tutorial-1
  (:use :common-lisp :sdl2)
  (:export :c-main :main))

(in-package :sdl-tutorial-1)

(defparameter *screen-width* 640)
(defparameter *screen-height* 480)

(defun c-main()
  (sdl2:init :video)
  (let* ((window (sdl2:create-window :title "SDL2 Window" :w *screen-width* :h *screen-height*))
         (renderer (sdl2:create-renderer window)))
    (sdl2:set-render-draw-color renderer #xFF #xFF #xFF 255)
    (sdl2:render-fill-rect renderer nil)
    (sdl2:render-present renderer)
    (sdl2:delay 2000)
    (sdl2:destroy-renderer renderer)
    (sdl2:destroy-window window))
  (sdl2:quit))

(defun main()
  (sdl2:with-init (:video)
    (sdl2:with-window (window :title "SDL2 Window" :w *screen-width* :h *screen-height*)
      (sdl2:with-renderer (renderer window)
        (sdl2:set-render-draw-color renderer 255 255 255 255)
        (sdl2:render-fill-rect renderer nil)
        (sdl2:render-present renderer)
        (sdl2:delay 2000)))))
