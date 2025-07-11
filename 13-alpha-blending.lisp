(defpackage #:sdl2-tutorial-13-alpha-blending
  (:use :cl)
  (:export :run)
  (:import-from :sdl2-tutorial-utils :asset-pathname))

(in-package #:sdl2-tutorial-13-alpha-blending)

(defparameter *screen-width* 640)
(defparameter *screen-height* 480)

(defclass tex ()
  ((renderer
    :initarg :renderer
    :initform (error "Must supply a renderer"))
   (width
    :accessor tex-width
    :initform 0)
   (height
    :accessor tex-height
    :initform 0)
   (texture
    :accessor tex-texture
    :initform nil)))

(defun free-tex (tex)
  (with-slots (texture) tex
    (sdl2:destroy-texture texture)))

(defun load-texture-from-file (renderer filename)
  (let ((tex (make-instance 'tex :renderer renderer)))
    (with-slots (renderer texture  width height) tex
      (let ((surface (sdl2-image:load-image filename)))
        (setf width (sdl2:surface-width surface))
        (setf height (sdl2:surface-height surface))
        (sdl2:set-color-key surface :true (sdl2:map-rgb (sdl2:surface-format surface)
                                                        0 #xFF #xFF))
        (setf texture (sdl2:create-texture-from-surface renderer surface))
        (sdl2:free-surface surface)))
    tex))

(defun set-color (tex r g b)
  (sdl2:set-texture-color-mod (tex-texture tex) r g b))

(defun set-blend-mode (tex blending)
  (sdl2:set-texture-blend-mode (tex-texture tex) blending))

(defun set-alpha (tex alpha)
  (sdl2:set-texture-alpha-mod (tex-texture tex) alpha))

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
                        :title "SDL2 Tutorial 13"
                        :w *screen-width*
                        :h *screen-height*
                        :flags '(:shown))
       (sdl2:with-renderer (,renderer ,window :index -1 :flags '(:accelerated))
         ,@body))))

(defun clamp (x)
  (max 0 (min 255 x)))

(defmacro clamp-incf (x delta)
  `(setf ,x (clamp (+ ,x ,delta))))

(defmacro clamp-decf (x delta)
  `(setf ,x (clamp (- ,x ,delta))))

(defun run ()
  (with-window-renderer (window renderer)
    (sdl2-image:init '(:png))
    (let ((bg-texture (load-texture-from-file renderer (asset-pathname "assets/13/fadein.png")))
          (modulated-texture (load-texture-from-file renderer (asset-pathname "assets/13/fadeout.png")))
          (alpha 255)
          (delta 32))
      (set-blend-mode modulated-texture :blend)
      (sdl2:with-event-loop (:method :poll)
        (:quit () t)
        (:keydown (:keysym keysym)
                  (case (sdl2:scancode keysym)
                    (:scancode-w (clamp-incf alpha delta))
                    (:scancode-s (clamp-decf alpha delta))))
        (:idle ()
               (sdl2:set-render-draw-color renderer #xFF #xFF #xFF #xFF)
               (sdl2:render-clear renderer)
               (render bg-texture 0 0)
               (set-alpha modulated-texture alpha)
               (render modulated-texture 0 0)
               (sdl2:render-present renderer)))

      ;; Clean up
      (free-tex bg-texture)
      (free-tex modulated-texture)
      (sdl2-image:quit))))
