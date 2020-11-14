(defpackage #:sdl2-tutorial-6
  (:use :common-lisp)
  (:export :main))

(in-package :sdl2-tutorial-6)

(defparameter *screen-width* 640)
(defparameter *screen-height* 480)

(defmacro with-window-surface ((window surface) &body body)
  `(sdl2:with-init (:video)
     (sdl2:with-window (,window
                        :title "SDL2 Tutorial"
                        :w *screen-width*
                        :h *screen-height*
                        :flags '(:shown))
       (let ((,surface (sdl2:get-window-surface ,window)))
         ,@body))))

(defun load-surface (filename &optional pixel-format)
  (sdl2:convert-surface-format (sdl2-image:load-image filename) pixel-format))

(defun main()
  (with-window-surface (window screen-surface)
    (sdl2-image:init '(:png))
    (let ((image-surface (load-surface "6/loaded.png" (sdl2:surface-format-format screen-surface)))
          (rect (sdl2:make-rect 0 0 *screen-width* *screen-height*)))
      (sdl2:with-event-loop (:method :poll)
        (:quit () t)
        (:idle ()
               (sdl2:blit-scaled image-surface
                                 nil
                                 screen-surface
                                 rect)
               (sdl2:update-window window))))))
