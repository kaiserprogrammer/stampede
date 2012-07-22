(cl:defpackage :stampede-system
  (:use :cl :asdf))
(cl:in-package :stampede-system)

(defsystem :stampede
  :version "0.1"
  :author "Jürgen Bickert <juergenbickert@gmail.com>"
  :maintainer "Jürgen Bickert <juergenbickert@gmail.com>"
  :description "A dead simple web server"
  :depends-on (:chanl
               :cl-ppcre
               :usocket
               :iolib
               :do-urlencode)
  :components ((:file "stampede")))
