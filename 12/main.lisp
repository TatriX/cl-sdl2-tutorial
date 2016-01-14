(defpackage #:sdl-tutorial-12
  (:use :common-lisp)
  (:export :main))

(in-package :sdl-tutorial-12)

(defparameter *screen-width* 640)
(defparameter *screen-height* 480)

(defclass ltexture ()
  ((filename
    :initarg :filename
    :initform (error "Must supply a filename"))
   (renderer
    :initarg :renderer
    :initform (error "Must supply a renderer"))
   (width
    :accessor width
    :initform 0 )
   (height
    :accessor height
    :initform 0)
   (texture
    :accessor texture
    :initform nil)))

(defmethod initialize-instance :after ((tex ltexture) &key)
  (with-slots (filename renderer texture  width height) tex
    (let ((surface (sdl2-image:load-image filename)))
      (setf width (sdl2:surface-width surface))
      (setf height (sdl2:surface-height surface))
      (sdl2:set-color-key surface :true (sdl2:map-rgb (sdl2:surface-format surface)
                                                  0 #xFF #xFF))
      (setf texture (sdl2:create-texture-from-surface renderer surface)))))

(defun set-color (tex r g b)
  (sdl2:set-texture-color-mod (slot-value tex 'texture) r g b))

(defun render (tex x y &key clip)
  (with-slots (renderer texture width height) tex
    (sdl2:render-copy renderer
                      texture
                      :source-rect clip
                      :dest-rect (sdl2:make-rect x
                                                 y
                                                 (if clip (sdl2:rect-width clip) width)
                                                 (if clip (sdl2:rect-height clip) height)))))

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
    (let ((texture (make-instance 'ltexture :filename "12/texture.png" :renderer renderer))
          (r 255)
          (g 255)
          (b 255)
          (delta 32))
          (sdl2:with-event-loop (:method :poll)
            (:quit () t)
            (:keydown
             (:keysym keysym)
             (case (sdl2:scancode keysym)
               (:scancode-q (incf r delta))
               (:scancode-w (incf g delta))
               (:scancode-e (incf b delta))
               (:scancode-a (decf r delta))
               (:scancode-s (decf g delta))
               (:scancode-d (decf b delta))))
            (:idle ()
                   (sdl2:set-render-draw-color renderer #xFF #xFF #xFF #xFF)
                   (sdl2:render-clear renderer)
                   (set-color texture r g b)
                   (render texture 0 0)
                   (sdl2:render-present renderer))))))
