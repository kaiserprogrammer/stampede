(cl:defpackage :stampede-system
  (:use :cl :asdf))
(cl:in-package :stampede-system)

(defsystem :stampede
  :version "0.1"
  :author "Jürgen Bickert <juergenbickert@gmail.com>"
  :maintainer "Jürgen Bickert <juergenbickert@gmail.com>"
  :description "A dead simple web server"
  :depends-on (:cl-ppcre
               :iolib
               :do-urlencode
               :alexandria
               :anaphora
               :local-time
               :lparallel)
  :components ((:file "stampede")))
