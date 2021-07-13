(defpackage #:sdl2-tutorial-3
  (:use :common-lisp)
  (:export :main))

(in-package :sdl2-tutorial-3)

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

(defun main()
  (with-window-surface (window screen-surface)
    (let ((image (sdl2:load-bmp "3/exit.bmp")))
      (sdl2:with-event-loop (:method :poll)
        (:quit () t)
        (:idle ()
               (sdl2:blit-surface image nil screen-surface nil)
               (sdl2:update-window window)
               (sdl2:delay 100))))))    ;reduce cpu usage
