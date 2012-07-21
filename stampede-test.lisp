(defpackage :stampede-test
  (:use :cl :fiveam :stampede :usocket))
(in-package :stampede-test)

(def-suite stampede)
(in-suite stampede)

(test creating-closing-server
  (finishes
    (let ((server (create-server 8080
                                 (lambda (stream)
                                   (declare (ignore stream))))))
      (unwind-protect
           (progn
             (shutdown-server server)
             (sleep 0.002)              ; wait for server to shutdown
             (signals connection-refused-error
               (with-client-socket (socket stream "127.0.0.1" 8080)
                 (declare (ignore stream)))))
        (shutdown-server server))
      (let ((server (create-server 8080
                                   (lambda (stream)
                                     (declare (ignore stream))))))
        (unwind-protect
             (progn
               (shutdown-server server)
               (sleep 0.002)            ; wait for server to shutdown
               (signals connection-refused-error
                 (with-client-socket (socket stream "127.0.0.1" 8080)
                   (declare (ignore stream))))
               (shutdown-server server))
          (shutdown-server server))))))

(test connecting-to-server
  (finishes
    (let* (answer
           (server (create-server 8080
                                  (lambda (stream)
                                    (setf answer (read-line stream))))))
      (unwind-protect
           (progn
             (with-client-socket (socket stream "127.0.0.1" 8080)
               (format stream "client says~%")
               (force-output stream))
             (sleep 0.004)
             (is (string= "client says" answer))
             (shutdown-server server)
             (sleep 0.002))
        (shutdown-server server)))))

(test http-protocol-reader-get
  (let ((req (with-input-from-string
                 (s (format nil "GET /url HTTP/1.0~%blub: that~%~%"))
               (http-protocol-reader s))))
    (loop for (left right) in '((:method  "GET")
                                (:url "/url")
                                (:version "1.0")
                                (:params nil)
                                ("blub" "that"))
       do (progn
            (is (assoc left req :test #'equal))
            (is (equal right
                       (cdr (assoc left req :test #'equal)))))))
  (let ((req (with-input-from-string (s (format nil "GET /url HTTP/1.0~%blub: that~%~%"))
               (http-protocol-reader s))))
    (loop for (left right) in '((:method "GET")
                                  (:url "/url")
                                  (:version "1.0")
                                  ("blub" "that"))
       do (is (equal right
                     (cdr (assoc left req :test #'equal)))))))

(test http-protocol-reader-post
  (let ((req (with-input-from-string
                 (s (format nil "POST /authors HTTP/1.1
Host: juergenbickert.de
Content-Length: 34
name=Alan+Perlis&commit=Add+Author"))
               (http-protocol-reader s))))
    (loop for (left right) in '((:method  "POST")
                                (:url "/authors")
                                (:version "1.1")
                                (:content-length 34)
                                ("Host" "juergenbickert.de"))
       do (is (equal right
                     (cdr (assoc left req :test #'equal)))))
    (let ((params (cdr (assoc :params req))))
      (is (string= "Alan Perlis" (cdr (assoc "name" params :test #'string=))))
      (is (string= "Add Author" (cdr (assoc "commit" params :test #'string=)))))))

(test http-protocol-reader-put
  (let ((req (with-input-from-string
                 (s (format nil "POST /authors HTTP/1.1
Host: juergenbickert.de
Content-Length: 46
name=Alan+Perlis&commit=Add+Author&_method=put"))
               (http-protocol-reader s))))
    (loop for (left right) in '((:method  "PUT")
                                (:url "/authors")
                                (:version "1.1")
                                (:content-length 46)
                                ("Host" "juergenbickert.de"))
       do (is (equal right
                     (cdr (assoc left req :test #'equal)))))
    (let ((params (cdr (assoc :params req))))
      (is (string= "Alan Perlis" (cdr (assoc "name" params :test #'string=))))
      (is (string= "Add Author" (cdr (assoc "commit" params :test #'string=)))))))

(test url-parsing
  (let* ((parsed-url (parse-url "/?john=doe"))
        (url (elt parsed-url 0))
         (params (elt parsed-url 1)))
    (is (equal "/" url))
    (is (equal "doe" (cdr (assoc "john" params :test #'equal)))))
  (let* ((parsed-url (parse-url "/blub/this/that?john=doe&jonny=depp&encoded=%2F"))
        (url (elt parsed-url 0))
         (params (elt parsed-url 1)))
    (is (equal "/blub/this/that" url))
    (is (equal "doe" (cdr (assoc "john" params :test #'equal))))
    (is (equal "depp" (cdr (assoc "jonny" params :test #'equal))))
    (is (equal "/" (cdr (assoc "encoded" params :test #'string=))))))

(test query-parsing
  (let ((params
         (cdr (assoc
               :params
               (with-input-from-string (s (format nil "GET /?john=doe&jim=beam HTTP/1.0~%~%"))
                 (http-protocol-reader s))))))
    (is (equal "doe" (cdr (assoc "john" params :test #'equal))))
    (is (equal "beam" (cdr (assoc "jim" params :test #'equal))))))

(test http-protocol-writer
  (is (equal
       "HTTP/1.1 200
Location: /path/to/
Content-Type: text/html; charset=utf-8

blub"
       (with-output-to-string (s)
         (http-protocol-writer
          '(("Location" . "/path/to/")
            (:status . 200)
            (:version . "1.1")
            ("Content-Type" . "text/html; charset=utf-8"))
          "blub"
          s)))))

(test url-decoding
  (is (string= "http://www.test.org/John%202.jpg" (urlencode:urldecode "http%3A%2F%2Fwww.test.org%2FJohn%25202.jpg"))))

(run!)
