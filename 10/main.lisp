(defpackage #:sdl-tutorial-10
  (:use :common-lisp)
  (:export :main))

(in-package :sdl-tutorial-10)

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

(defun render (tex x y)
  (with-slots (renderer texture width height) tex
    (sdl2:render-copy renderer texture :dest-rect (sdl2:make-rect x y width height))))

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
    (let ((background-tex (make-instance 'ltexture :filename "10/background.png" :renderer renderer))
          (player-tex (make-instance 'ltexture :filename "10/player.png" :renderer renderer)))
          (sdl2:with-event-loop (:method :poll)
            (:quit () t)
            (:idle ()
                   (sdl2:set-render-draw-color renderer #xFF #xFF #xFF #xFF)
                   (sdl2:render-clear renderer)

                   (render background-tex 0 0)
                   (render player-tex 240 190)

                   (sdl2:render-present renderer))))))
