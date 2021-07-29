(defpackage #:sdl2-tutorial-07-texture-loading-and-rendering
  (:use :cl)
  (:export :run)
  (:import-from :sdl2-tutorial-utils :asset-pathname))

(in-package :sdl2-tutorial-07-texture-loading-and-rendering)

(defparameter *screen-width* 640)
(defparameter *screen-height* 480)

(defmacro with-window-renderer ((window renderer) &body body)
  `(sdl2:with-init (:video)
     (sdl2:with-window (,window
                        :title "SDL2 Tutorial 07"
                        :w *screen-width*
                        :h *screen-height*
                        :flags '(:shown))
       (sdl2:with-renderer (,renderer ,window :index -1 :flags '(:accelerated))
         ,@body))))


(defun load-texture (renderer pathname)
  (let ((surface (sdl2-image:load-image pathname)))
    (prog1 (sdl2:create-texture-from-surface renderer surface)
      (sdl2:free-surface surface))))

(defun run ()
  (with-window-renderer (window renderer)
    (sdl2-image:init '(:png))
    (sdl2:set-render-draw-color renderer #xFF #xFF #xFF #xFF)
    (let ((texture (load-texture renderer (asset-pathname #P"./assets/07/texture.png"))))
      (sdl2:with-event-loop (:method :poll)
        (:quit () t)
        (:idle ()
               (sdl2:render-clear renderer)
               (sdl2:render-copy renderer texture)
               (sdl2:render-present renderer)))

      ;; clean up
      (sdl2:destroy-texture texture))))
