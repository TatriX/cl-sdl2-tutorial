(defpackage #:sdl-tutorial-5
  (:use :common-lisp)
  (:export :main))

(in-package :sdl-tutorial-5)

(defparameter *screen-width* 640)
(defparameter *screen-height* 480)

(defmacro with-window (window &body body)
  `(sdl2:with-init (:video)
     (sdl2:with-window (,window
                        :title "SDL2 Tutorial"
                        :w *screen-width*
                        :h *screen-height*
                        :flags '(:shown))
         ,@body)))

(defun load-surface (filename &optional pixel-format)
  (sdl2:convert-surface-format (sdl2:load-bmp filename) pixel-format))

(defun main()
  (with-window window
    (let* ((window-surface (sdl2:get-window-surface window))
           (image-surface (load-surface "stretch.bmp" (sdl2:surface-format-format window-surface))))
      (sdl2:with-event-loop (:method :poll)
        (:quit () t)
        (:idle ()
               (sdl2:blit-scaled image-surface
                                 nil
                                 window-surface
                                 (sdl2:make-rect 0 0 *screen-width* *screen-height*))
               (sdl2:update-window window))))))
