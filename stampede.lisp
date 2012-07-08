(defpackage :stampede
  (:use :cl :usocket :cl-ppcre :chanl)
  (:export
   :shutdown-server
   :create-server
   :http-protocol-reader
   :http-protocol-writer))
(in-package :stampede)

(defun create-server (host port handler &key (worker-threads 1))
  (let* ((socket (usocket:socket-listen host
                                        port
                                        :reuse-address t))
         (channel (make-instance 'unbounded-channel))
         (thread
          (sb-thread:make-thread
           (lambda ()
             (unwind-protect
                  (loop
                     (usocket:wait-for-input socket)
                     (let ((stream (usocket:socket-stream (usocket:socket-accept socket))))
                       (send channel stream)))
               (usocket:socket-close socket)))))
         (worker-threads
          (loop repeat worker-threads
               collect
           (sb-thread:make-thread
            (lambda ()
              (loop
                 (let ((stream (recv channel)))
                   (ignore-errors
                    (funcall handler stream)
                    (close stream)))))))))
    (lambda ()
      (ignore-errors
        (sb-thread:destroy-thread thread)
        (loop for worker in worker-threads
           do (sb-thread:destroy-thread worker))))))

(defun shutdown-server (server)
  (funcall server))

(defun http-protocol-reader (stream)
  (let* ((groups (multiple-value-bind (match groups)
                     (scan-to-strings "(\\w+) (\\S+) (\\w+)/(\\S+)" (read-line stream))
                   (declare (ignore match))
                   groups))
         (method (svref groups 0))
         (url (svref groups 1))
         (http-protocol-version (svref groups 3)))
    (append (list (cons :method method)
                  (cons :url url)
                  (cons :version http-protocol-version))
            (loop for line = (read-line stream)
               until (or (equal "" line)
                         (equal "" line))
               collect (let ((data (split ":" line)))
                         (cons (first data) (string-trim " " (second data))))))))
(defun http-protocol-writer (data text stream)
  (format stream "HTTP/~a ~a~%" (cdr (assoc :version data)) (cdr (assoc :status data)))
  (loop for pair in data
     when (not
           (or (equal :version (car pair))
               (equal :status (car pair))))
     do (format stream "~a: ~a~%" (car pair) (cdr pair)))
  (format stream "~%")
  (format stream "~a~%" text))
