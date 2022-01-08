(defpackage #:sdl2-tutorial-14-animated-sprites-and-vsync
  (:use :cl)
  (:export :run)
  (:import-from :sdl2-tutorial-utils :asset-pathname))

(in-package #:sdl2-tutorial-14-animated-sprites-and-vsync)

(defparameter *screen-width* 640)
(defparameter *screen-height* 480)

(defclass tex ()
  ((renderer
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

(defun load-texture-from-file (renderer filename)
  (let ((tex (make-instance 'tex :renderer renderer)))
    (with-slots (renderer texture  width height) tex
      (let ((surface (sdl2-image:load-image filename)))
        (setf width (sdl2:surface-width surface))
        (setf height (sdl2:surface-height surface))
        (sdl2:set-color-key surface :true (sdl2:map-rgb (sdl2:surface-format surface)
                                                        0 #xFF #xFF))
        (setf texture (sdl2:create-texture-from-surface renderer surface))))
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
                        :title "SDL2 Tutorial 14"
                        :w *screen-width*
                        :h *screen-height*
                        :flags '(:shown))
       (sdl2:with-renderer (,renderer ,window :index -1 :flags '(:accelerated :presentvsync))
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
    (let ((spritesheet-tex (load-texture-from-file renderer (asset-pathname "assets/14/character.png")))
          (clip (sdl2:make-rect 0 0 64 96))
          (sprite-frames 4)
          (current-sprite-frame 0)
          (frame 0)
          (frames-per-sprite 10))
      (sdl2:with-event-loop (:method :poll)
        (:quit () t)
        (:idle ()
               (sdl2:set-render-draw-color renderer #xFF #xFF #xFF #xFF)
               (sdl2:render-clear renderer)

               (render spritesheet-tex
                       (round (/ (- *screen-width* (sdl2:rect-width clip)) 2))
                       (round (/ (- *screen-height* (sdl2:rect-height clip)) 2))
                       :clip clip)
               (sdl2:render-present renderer)

               (incf frame)
               (when (zerop (rem frame frames-per-sprite))
                 (incf current-sprite-frame)
                 (when (>= current-sprite-frame sprite-frames)
                   (setf current-sprite-frame 0))
                 (setf (sdl2:rect-x clip) (* current-sprite-frame (sdl2:rect-width clip)))))))))
