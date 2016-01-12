(defpackage #:sdl-tutorial-4
  (:use :common-lisp :sdl2)
  (:export :c-main :main))

(in-package :sdl-tutorial-4)

(defparameter *screen-width* 640)
(defparameter *screen-height* 480)

(defmacro with-window-and-renderer (window renderer &body body)
  `(sdl2:with-init (:video)
     (sdl2:with-window (,window :title "SDL2 Tutorial");;  :w *screen-width* :h *screen-height*
       (sdl2:with-renderer (,renderer ,window)
         ,@body))))

(defun load-texture (renderer filename)
  (sdl2:create-texture-from-surface renderer (sdl2:load-bmp filename)))

(defun load-media (renderer)
  (list :default (load-texture renderer "press.bmp")
        :up (load-texture renderer "up.bmp")
        :down (load-texture renderer "down.bmp")
        :left (load-texture renderer "left.bmp")
        :right (load-texture renderer "right.bmp")))

(defun main()
  (with-window-and-renderer window renderer
    (let* ((textures (load-media renderer))
           (texture (getf textures :default)))
      (format t ":::~a~%" texture)
      (sdl2:with-event-loop (:method :poll)
        (:quit () t)
        (:keydown
         (:keysym keysym)
         (case (sdl2:scancode keysym)
           (:scancode-up (setf texture (getf textures :up)))
           (:scancode-down (setf texture (getf textures :down)))
           (:scancode-left (setf texture (getf textures :left)))
           (:scancode-right (setf texture (getf textures :right)))
           (t (setf texture (getf textures :default)))))
        (:idle
         ()
         (sdl2:render-copy renderer texture)
         (sdl2:render-present renderer))))))
