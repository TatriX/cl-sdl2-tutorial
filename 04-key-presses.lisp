(defpackage #:sdl2-tutorial-04-key-presses
  (:use :cl)
  (:export :run))

(in-package :sdl2-tutorial-04-key-presses)

(defparameter *screen-width* 640)
(defparameter *screen-height* 480)

(defmacro with-window-surface ((window surface) &body body)
  `(sdl2:with-init (:video)
     (sdl2:with-window (,window
                        :title "SDL2 Tutorial 04"
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

(defun load-media ()
  (list :default (load-image "assets/04/press.bmp")
        :up (load-image "assets/04/up.bmp")
        :down (load-image "assets/04/down.bmp")
        :left (load-image "assets/04/left.bmp")
        :right (load-image "assets/04/right.bmp")))

(defun run ()
  (with-window-surface (window screen-surface)
    (let* ((images (load-media))
           (image (getf images :default)))
      (sdl2:with-event-loop (:method :poll)
        (:quit () t)
        (:keydown (:keysym keysym)
                  (setf image (getf images (case (sdl2:scancode keysym)
                                             (:scancode-up :up)
                                             (:scancode-down :down)
                                             (:scancode-left :left)
                                             (:scancode-right :right)
                                             (t :default)))))
        (:idle ()
               (sdl2:blit-surface image nil screen-surface nil)
               (sdl2:update-window window)
               ;; reduce cpu usage
               (sdl2:delay 100)))
      ;; clean up
      (mapc #'sdl2:free-surface (remove-if #'symbolp images)))))
