(defpackage #:sdl2-tutorial-01
  (:use :cl)
  (:export :run))

(in-package :sdl2-tutorial-01)

(defparameter *screen-width* 640)
(defparameter *screen-height* 480)

(defun run()
  (sdl2:with-init (:video)
    (sdl2:with-window (window :title "SDL2 Tutorial 01"
                              :w *screen-width*
                              :h *screen-height*
                              :flags '(:shown))
      (let ((screen-surface (sdl2:get-window-surface window)))
        (sdl2:fill-rect screen-surface
                        nil
                        (sdl2:map-rgb (sdl2:surface-format screen-surface) 255 255 255))
        (sdl2:update-window window)
        (sdl2:delay 2000)))))
