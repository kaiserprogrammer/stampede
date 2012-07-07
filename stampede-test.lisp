(defpackage :stampede-test
  (:use :cl :fiveam :stampede :usocket))
(in-package :stampede-test)

(def-suite stampede)
(in-suite stampede)

(test creating-closing-server
  (finishes
    (let ((server (create-server "127.0.0.1" 8080
                                 (lambda (stream)
                                   (declare (ignore stream))))))
      (shutdown-server server)
      (signals connection-refused-error
        (with-client-socket (socket stream "127.0.0.1" 8080)
          (declare (ignore stream))))
      (let ((server (create-server "127.0.0.1" 8080
                                   (lambda (stream)
                                     (declare (ignore stream))))))
        (shutdown-server server)
        (signals connection-refused-error
          (with-client-socket (socket stream "127.0.0.1" 8080)
            (declare (ignore stream))))))))

(run!)
