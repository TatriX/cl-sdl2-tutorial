(defpackage #:sdl2-tutorial-02-getting-an-image-on-the-screen
  (:use :cl)
  (:export :run)
  (:import-from :sdl2-tutorial-utils :asset-pathname))

(in-package :sdl2-tutorial-02-getting-an-image-on-the-screen)

(defparameter *screen-width* 640)
(defparameter *screen-height* 480)

(defmacro with-window-surface ((window surface) &body body)
  `(sdl2:with-init (:video)
     (sdl2:with-window (,window
                        :title "SDL2 Tutorial 02"
                        :w *screen-width*
                        :h *screen-height*
                        :flags '(:shown))
       (let ((,surface (sdl2:get-window-surface ,window)))
         ,@body))))

(defun load-image (filename)
  (let ((image (sdl2:load-bmp filename)))
    (if (autowrap:wrapper-null-p image)
        (error "cannot load image ~a (check that file exists)" filename)
        image)))

(defun run()
  (with-window-surface (window screen-surface)
    (let ((image (load-image (asset-pathname #P"./assets/02/hello_world.bmp"))))
      (sdl2:blit-surface image nil screen-surface nil)
      (sdl2:update-window window)
      (sdl2:delay 2000)
      ;; clean up
      (sdl2:free-surface image))))
