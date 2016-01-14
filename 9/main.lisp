(defpackage #:sdl-tutorial-9
  (:use :common-lisp)
  (:export :main))

(in-package :sdl-tutorial-9)

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

    (let ((texture (load-texture renderer "9/texture.png")))
          (sdl2:with-event-loop (:method :poll)
            (:quit () t)
            (:idle ()
                   (sdl2:set-render-draw-color renderer #xFF #xFF #xFF #xFF)
                   (sdl2:render-clear renderer)

                   (sdl2:render-set-viewport renderer
                                             (sdl2:make-rect 0
                                                             0
                                                             (/ *screen-width* 2)
                                                             (/ *screen-height* 2)))
                   (sdl2:render-copy renderer texture)

                   (sdl2:render-set-viewport renderer
                                             (sdl2:make-rect (/ *screen-width* 2)
                                                             0
                                                             (/ *screen-width* 2)
                                                             (/ *screen-height* 2)))
                   (sdl2:render-copy renderer texture)

                   (sdl2:render-set-viewport renderer
                                             (sdl2:make-rect 0
                                                             (/ *screen-height* 2)
                                                             *screen-width*
                                                             (/ *screen-height* 2)))
                   (sdl2:render-copy renderer texture)

                   (sdl2:render-present renderer))))))
