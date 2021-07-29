(defpackage #:sdl2-tutorial-06-extension-libraries-and-loading-other-image-formats
  (:use :cl)
  (:export :run)
  (:import-from :sdl2-tutorial-utils :asset-pathname))

(in-package :sdl2-tutorial-06-extension-libraries-and-loading-other-image-formats)

(defparameter *screen-width* 640)
(defparameter *screen-height* 480)

(defmacro with-window-surface ((window surface) &body body)
  `(sdl2:with-init (:video)
     (sdl2:with-window (,window
                        :title "SDL2 Tutorial 06"
                        :w *screen-width*
                        :h *screen-height*
                        :flags '(:shown))
       (let ((,surface (sdl2:get-window-surface ,window)))
         (sdl2-image:init '(:png))
         ,@body
         (sdl2-image:quit)))))

(defun load-surface (pathname pixel-format)
  (let ((surface (sdl2-image:load-image pathname)))
    (prog1 (sdl2:convert-surface-format surface pixel-format)
      (sdl2:free-surface surface))))

(defun run ()
  (with-window-surface (window screen-surface)
    (let ((image-surface (load-surface (asset-pathname #P"assets/06/loaded.png")
                                       (sdl2:surface-format-format screen-surface)))
          (rect (sdl2:make-rect 0 0 *screen-width* *screen-height*)))
      (sdl2:with-event-loop (:method :poll)
        (:quit () t)
        (:idle ()
               (sdl2:blit-scaled image-surface nil screen-surface rect)
               (sdl2:update-window window)))

      ;; cleanup
      (sdl2:free-surface image-surface))))
