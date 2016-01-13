(defpackage #:sdl-tutorial-7
  (:use :common-lisp)
  (:export :main))

(in-package :sdl-tutorial-7)

(defparameter *screen-width* 640)
(defparameter *screen-height* 480)

(defmacro with-window-renderer ((window renderer) &body body)
  `(sdl2:with-init (:video)
     (sdl2:with-window (,window
                        :title "SDL2 Tutorial"
                        :w *screen-width*
                        :h *screen-height*
                        :flags '(:shown))
       (sdl2:with-renderer (,renderer ,window :index -1 :flags '(:accelerated))
                           ,@body))))

(defun load-texture (renderer filename)
  (sdl2:create-texture-from-surface renderer (sdl2-image:load-image filename)))

(defun main()
  (with-window-renderer (window renderer)
    (sdl2-image:init '(:png))
    (sdl2:set-render-draw-color renderer #xFF #xFF #xFF #xFF)
    (let ((texture (load-texture renderer "7/texture.png")))
      (sdl2:with-event-loop (:method :poll)
        (:quit () t)
        (:idle ()
               (sdl2:render-clear renderer)
               (sdl2:render-copy renderer texture)
               (sdl2:render-present renderer))))))
