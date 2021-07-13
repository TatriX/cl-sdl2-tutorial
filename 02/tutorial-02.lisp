(defpackage #:sdl2-tutorial-02
  (:use :cl)
  (:export :run))

(in-package :sdl2-tutorial-02)

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

(defun load-image (pathname)
  (let* ((fullpath (merge-pathnames pathname (asdf:system-source-directory :sdl2-tutorial)))
         (image (sdl2:load-bmp fullpath)))
    (if (autowrap:wrapper-null-p image)
        (error "cannot load image ~a (check that file exists)" fullpath)
        image)))

(defun run()
  (with-window-surface (window screen-surface)
    (let ((image (load-image #p"./02/hello_world.bmp")))
      (sdl2:blit-surface image nil screen-surface nil)
      (sdl2:update-window window)
      (sdl2:delay 2000)
      ;; clean up
      (sdl2:free-surface image))))
