(defpackage :sdl2-tutorial
  (:use cl bt)
  (:export :main :create-swank-server))

(in-package :sdl2-tutorial)

(defun create-swank-server()
  (bt:make-thread (lambda () (swank:create-server :port 4005 :dont-close t))))

(defun main(fn)
  (sdl2:make-this-thread-main fn))
