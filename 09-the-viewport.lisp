(defpackage #:sdl2-tutorial-09-the-viewport
  (:use :cl)
  (:export :run)
  (:import-from :sdl2-tutorial-utils :asset-pathname))

(in-package :sdl2-tutorial-09-the-viewport)

(defparameter *screen-width* 640)
(defparameter *screen-height* 480)

(defmacro with-window-renderer ((window renderer) &body body)
  `(sdl2:with-init (:video)
     (sdl2:with-window (,window
                        :title "SDL2 Tutorial 09"
                        :w *screen-width*
                        :h *screen-height*
                        :flags '(:shown))
       (sdl2:with-renderer (,renderer ,window :index -1 :flags '(:accelerated))
         ,@body))))

(defun load-texture (renderer pathname)
  (let ((surface (sdl2-image:load-image pathname)))
    (prog1 (sdl2:create-texture-from-surface renderer surface)
      (sdl2:free-surface surface))))

(defun run ()
  (with-window-renderer (window renderer)
    (sdl2-image:init '(:png))

    (let ((texture (load-texture renderer (asset-pathname #P"assets/09/texture.png"))))
      (sdl2:with-event-loop (:method :poll)
        (:quit () t)
        (:idle ()
               ;; Clear screen
               (sdl2:set-render-draw-color renderer #xFF #xFF #xFF #xFF)
               (sdl2:render-clear renderer)

               ;; Top left corner viewport
               (sdl2:with-rects ((top-left-viewport 0
                                                    0
                                                    (/ *screen-width* 2)
                                                    (/ *screen-height* 2))
                                 (top-right-viewport (/ *screen-width* 2)
                                                     0
                                                     (/ *screen-width* 2)
                                                     (/ *screen-height* 2))
                                 (bottom-viewport 0
                                                  (/ *screen-height* 2)
                                                  *screen-width*
                                                  (/ *screen-height* 2)))
                 (sdl2:render-set-viewport renderer top-left-viewport)
                 (sdl2:render-copy renderer texture) ; Render texture to screen

                 (sdl2:render-set-viewport renderer top-right-viewport)
                 (sdl2:render-copy renderer texture)

                 (sdl2:render-set-viewport renderer bottom-viewport)
                 (sdl2:render-copy renderer texture))

               (sdl2:render-present renderer)))
      ;; cleanup
      (sdl2:destroy-texture texture))))
