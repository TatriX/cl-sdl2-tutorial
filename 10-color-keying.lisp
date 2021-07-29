(defpackage #:sdl2-tutorial-10-color-keying
  (:use :cl)
  (:export :run)
  (:import-from :sdl2-tutorial-utils :asset-pathname))

(in-package :sdl2-tutorial-10-color-keying)

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
    :initform nil)))

(defun free-tex (tex)
  (with-slots (texture) tex
    (sdl2:destroy-texture texture)))

(defun load-texture-from-file (renderer pathname)
  (let ((tex (make-instance 'tex :renderer renderer)))
    (with-slots (renderer texture  width height) tex
      (let ((surface (sdl2-image:load-image pathname)))
        (setf width (sdl2:surface-width surface))
        (setf height (sdl2:surface-height surface))
        (sdl2:set-color-key surface :true (sdl2:map-rgb (sdl2:surface-format surface)
                                                        0 #xFF #xFF))
        (setf texture (sdl2:create-texture-from-surface renderer surface))
        (sdl2:free-surface surface)))
    tex))

(defun render (tex x y)
  (with-slots (renderer texture width height) tex
    (sdl2:render-copy renderer texture :dest-rect (sdl2:make-rect x y width height))))

(defmacro with-window-renderer ((window renderer) &body body)
  `(sdl2:with-init (:video)
     (sdl2:with-window (,window
                        :title "SDL2 Tutorial 10"
                        :w *screen-width*
                        :h *screen-height*
                        :flags '(:shown))
       (sdl2:with-renderer (,renderer ,window :index -1 :flags '(:accelerated))
         ,@body))))

(defun run()
  (with-window-renderer (window renderer)
    (sdl2-image:init '(:png))
    (let ((background-tex (load-texture-from-file renderer (asset-pathname #P"assets/10/background.png")))
          (player-tex (load-texture-from-file renderer (asset-pathname #P"assets/10/player.png"))))
      (sdl2:with-event-loop ()
        (:quit () t)
        (:idle ()
               (sdl2:set-render-draw-color renderer #xFF #xFF #xFF #xFF)
               (sdl2:render-clear renderer)

               (render background-tex 0 0)
               (render player-tex 240 190)

               (sdl2:render-present renderer)))
      ;; clean up
      (free-tex background-tex)
      (free-tex player-tex))))
