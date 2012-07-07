(defpackage :stampede
  (:use :cl :usocket)
  (:export
   :shutdown-server
   :create-server))
(in-package :stampede)

(defun create-server (host port handler)
  (let* ((socket (usocket:socket-listen host
                                        port
                                        :reuse-address t))
         (thread
          (sb-thread:make-thread
           (lambda ()
             (unwind-protect
                  (loop
                     (usocket:wait-for-input socket)
                     (let ((stream (usocket:socket-stream (usocket:socket-accept socket))))
                       (funcall handler stream)
                       (close stream)))
               (usocket:socket-close socket))))))
    (lambda ()
      (sb-thread:destroy-thread thread))))

(defun shutdown-server (server)
  (funcall server))
