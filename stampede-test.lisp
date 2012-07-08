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

(test http-protocol-reader
  (is (equalp '((:method . "GET")
                (:url . "/url")
                (:version . "1.0")
                ("blub" . "that"))
              (with-input-from-string (s (format nil "GET /url HTTP/1.0~%blub: that~%~%"))
                (http-protocol-reader s))))
  (is (equalp '((:method . "GET")
                (:url . "/url")
                (:version . "1.0")
                ("blub" . "that"))
              (with-input-from-string (s (format nil "GET /url HTTP/1.0~%blub: that~%~%"))
                (http-protocol-reader s)))))

(test http-protocol-writer
  (is (equal "HTTP/1.1 200
Content-Type: text/html; charset=utf-8

blub
"
             (with-output-to-string (s)
               (http-protocol-writer '((:status . 200)
                                       (:version . "1.1")
                                       ("Content-Type" . "text/html; charset=utf-8"))
                                     "blub"
                                     s)))))

(run!)
