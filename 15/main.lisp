(defpackage #:sdl-tutorial-15
  (:use :common-lisp)
  (:export :main))

(in-package :sdl-tutorial-15)

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
    :accessor tex-width
    :initform 0 )
   (height
    :accessor tex-height
    :initform 0)
   (texture
    :accessor tex-texture
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
  (sdl2:set-texture-color-mod (tex-texture tex) r g b))

(defun render (tex x y &key clip angle center flip)
  (with-slots (renderer texture width height) tex
    (sdl2:render-copy-ex renderer
                         texture
                         :source-rect clip
                         :dest-rect (sdl2:make-rect x
                                                    y
                                                    (if clip (sdl2:rect-width clip) width)
                                                    (if clip (sdl2:rect-height clip) height))
                         :angle angle
                         :center center
                         :flip (list flip))))

(defmacro with-window-renderer ((window renderer) &body body)
  `(sdl2:with-init (:video)
     (sdl2:with-window (,window
                        :title "SDL2 Tutorial"
                        :w *screen-width*
                        :h *screen-height*
                        :flags '(:shown))
       (sdl2:with-renderer (,renderer ,window :index -1 :flags '(:accelerated))
         ,@body))))

(defun main()
  (with-window-renderer (window renderer)
    (sdl2-image:init '(:png))
    (let ((texture (make-instance 'ltexture :filename "15/arrow.png" :renderer renderer))
          (flip :flip-none)
          (degrees 0)
          (delta 60))
      (sdl2:with-event-loop (:method :poll)
        (:quit () t)
        (:keydown
         (:keysym keysym)
         (case (sdl2:scancode keysym)
           (:scancode-a (decf degrees delta))
           (:scancode-d (incf degrees delta))
           (:scancode-q (setf flip :horizontal))
           (:scancode-w (setf flip :none))
           (:scancode-e (setf flip :vertical))))
        (:idle ()
               (sdl2:set-render-draw-color renderer #xFF #xFF #xFF #xFF)
               (sdl2:render-clear renderer)
               (render texture
                       (round (/ (- *screen-width* (tex-width texture)) 2))
                       (round (/ (- *screen-height* (tex-height texture)) 2))
                       :angle degrees
                       :flip flip)
               (sdl2:render-present renderer))))))
