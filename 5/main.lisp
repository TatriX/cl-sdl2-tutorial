(defpackage #:sdl-tutorial-5
  (:use :common-lisp :sdl2)
  (:export :main))

(in-package :sdl-tutorial-5)

(defparameter *screen-width* 640)
(defparameter *screen-height* 480)

(defmacro with-window-and-renderer (window renderer &body body)
  `(sdl2:with-init (:video)
     (sdl2:with-window (,window :title "SDL2 Tutorial" :w *screen-width* :h *screen-height*)
       (sdl2:with-renderer (,renderer ,window)
         ,@body))))

(defun load-texture (window renderer filename)
  (sdl2:create-texture-from-surface renderer
                                    (sdl2:convert-surface-format (sdl2:load-bmp filename)
                                                                 (sdl2:surface-format-format
                                                                  (sdl2:get-window-surface window)))))

(defun main()
  (with-window-and-renderer window renderer
    (let ((texture (load-texture window renderer "stretch.bmp")))
      (sdl2:with-event-loop (:method :poll)
        (:quit () t)
        (:idle ()
               (sdl2:render-copy renderer texture)
               (sdl2:render-present renderer))))))
