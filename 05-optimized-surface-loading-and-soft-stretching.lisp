(defpackage #:sdl2-tutorial-05-optimized-surface-loading-and-soft-stretching
  (:use :cl)
  (:export :run))

(in-package :sdl2-tutorial-05-optimized-surface-loading-and-soft-stretching)

(defparameter *screen-width* 640)
(defparameter *screen-height* 480)

(defmacro with-window-surface ((window surface) &body body)
  `(sdl2:with-init (:video)
     (sdl2:with-window (,window
                        :title "SDL2 Tutorial 05"
                        :w *screen-width*
                        :h *screen-height*
                        :flags '(:shown))
       (let ((,surface (sdl2:get-window-surface ,window)))
         ,@body))))

(defun load-surface (pathname pixel-format)
  (let* ((fullpath (merge-pathnames pathname (asdf:system-source-directory :sdl2-tutorial)))
         (image (sdl2:load-bmp fullpath)))
    (if (autowrap:wrapper-null-p image)
        (error "cannot load image ~a (check that file exists)" fullpath)
        (sdl2:convert-surface-format image pixel-format))))

(defun run ()
  (with-window-surface (window screen-surface)
    (let ((image-surface (load-surface "assets/05/stretch.bmp" (sdl2:surface-format-format screen-surface)))
          (rect (sdl2:make-rect 0 0 *screen-width* *screen-height*)))
      (sdl2:with-event-loop (:method :poll)
        (:quit () t)
        (:idle ()
               (sdl2:blit-scaled image-surface
                                 nil
                                 screen-surface
                                 rect)
               (sdl2:update-window window))))))
