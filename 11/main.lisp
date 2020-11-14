(defpackage #:sdl2-tutorial-11
  (:use :common-lisp)
  (:export :main))

(in-package :sdl2-tutorial-11)

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

(defun main()
  (with-window-renderer (window renderer)
    (sdl2-image:init '(:png))
    (let ((spritesheet-tex (load-texture-from-file renderer "11/spritesheet.png"))
          (sprite-clips '(:top-left (0 0 100 100)
                          :top-right (100 0 100 100)
                          :bottom-left (0 100 100 100)
                          :bottom-right (100 100 100 100))))
      (sdl2:with-event-loop (:method :poll)
        (:quit () t)
        (:idle ()
               (sdl2:set-render-draw-color renderer #xFF #xFF #xFF #xFF)
               (sdl2:render-clear renderer)

               (let ((rect (apply #'sdl2::make-rect (getf sprite-clips :top-left))))
                 (render spritesheet-tex 0 0 :clip rect))

               (let ((rect (apply #'sdl2::make-rect (getf sprite-clips :top-right))))
                 (render spritesheet-tex
                         (- *screen-width* (sdl2:rect-width rect))
                         0
                         :clip rect))

               (let ((rect (apply #'sdl2::make-rect (getf sprite-clips :bottom-left))))
                 (render spritesheet-tex
                         0
                         (- *screen-height* (sdl2:rect-height rect))
                         :clip rect))

               (let ((rect (apply #'sdl2::make-rect (getf sprite-clips :bottom-right))))
                 (render spritesheet-tex
                         (- *screen-width* (sdl2:rect-width rect))
                         (- *screen-height* (sdl2:rect-height rect))
                         :clip rect))

               (sdl2:render-present renderer))))))
