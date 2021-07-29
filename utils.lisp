(defpackage #:sdl2-tutorial-utils
  (:use :cl)
  (:export :asset-path))

(in-package #:sdl2-tutorial-utils)

(defun asset-pathname (pathname)
  "Return an absolute filename for a given PATHNAME relative to
`:sdl2-tutorial' asdf system directory.

This function doesn't prepend 'assets/' to the PATHNAME to not
interfere with your editor filename completion."
  (merge-pathnames pathname (asdf:system-source-directory :sdl2-tutorial)))
