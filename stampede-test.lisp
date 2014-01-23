(defpackage :stampede-test
  (:use :cl :lisp-unit2 :stampede :usocket))
(in-package :stampede-test)

(remove-tests)

(define-test creating-closing-server ()
  (let ((server (run-server 9123
                            (lambda (stream)
                              (declare (ignore stream))))))
    (sleep 0.002)
    (unwind-protect
         (progn
           (stop server)
           (assert-error 'connection-refused-error
             (with-client-socket (socket stream "127.0.0.1" 9123)
               (declare (ignore stream)))))
      (stop server))
    (let ((server (run-server 9123
                              (lambda (stream)
                                (declare (ignore stream))))))
      (sleep 0.002)
      (unwind-protect
           (progn
             (stop server)
             (assert-error 'connection-refused-error
               (with-client-socket (socket stream "127.0.0.1" 9123)
                 (declare (ignore stream))))
             (stop server))
        (stop server)))))

(define-test connecting-to-server ()
  (let* (answer
         (server (run-server 9123
                             (lambda (stream)
                               (setf answer (read-line stream))))))
    (sleep 0.002)
    (unwind-protect
         (progn
           (with-client-socket (socket stream "127.0.0.1" 9123)
             (format stream "client says~%")
             (force-output stream))
           (sleep 0.04)
           (assert-equal "client says" answer)
           (stop server))
      (stop server))))

(define-test http-protocol-reader-get ()
  (let ((req (flexi-streams:with-input-from-sequence
                 (s (flexi-streams:string-to-octets (format nil "GET /url HTTP/1.0~%blub: that~%~%")))
               (http-protocol-reader s))))
    (loop for (left right) in '((:method  "GET")
                                (:url "/url")
                                (:version "1.0")
                                (:params nil)
                                ("blub" "that"))
       do (progn
            (assert-true (assoc left req :test #'equal))
            (assert-equal right
                         (cdr (assoc left req :test #'equal))))))
  (let ((req (flexi-streams:with-input-from-sequence (s (flexi-streams:string-to-octets (format nil "GET /url HTTP/1.0~%blub: that~%~%")))
               (http-protocol-reader s))))
    (loop for (left right) in '((:method "GET")
                                (:url "/url")
                                (:version "1.0")
                                ("blub" "that"))
       do (assert-equal right
                        (cdr (assoc left req :test #'equal))))))

(define-test http-protocol-reader-post ()
  (let ((req (flexi-streams:with-input-from-sequence
                 (s (flexi-streams:string-to-octets (format nil "POST /authors HTTP/1.1
Host: juergenbickert.de
Content-Length: 34

name=Alan+Perlis&commit=Add+Author")))
               (http-protocol-reader s))))
    (loop for (left right) in '((:method  "POST")
                                (:url "/authors")
                                (:version "1.1")
                                (:content-length 34)
                                ("Host" "juergenbickert.de"))
       do (assert-equal right
                       (cdr (assoc left req :test #'equal))))
    (let ((params (cdr (assoc :params req))))
      (assert-equal "Alan Perlis" (cdr (assoc "name" params :test #'string=)))
      (assert-equal "Add Author" (cdr (assoc "commit" params :test #'string=))))))

(define-test http-protocol-reader-put ()
  (let ((req (flexi-streams:with-input-from-sequence
                 (s (flexi-streams:string-to-octets (format nil "POST /authors HTTP/1.1
Host: juergenbickert.de
Content-Length: 46

name=Alan+Perlis&commit=Add+Author&_method=put")))
               (http-protocol-reader s))))
    (loop for (left right) in '((:method  "PUT")
                                (:url "/authors")
                                (:version "1.1")
                                (:content-length 46)
                                ("Host" "juergenbickert.de"))
       do (assert-equal right
                        (cdr (assoc left req :test #'equal))))
    (let ((params (cdr (assoc :params req))))
      (assert-equal "Alan Perlis" (cdr (assoc "name" params :test #'string=)))
      (assert-equal "Add Author" (cdr (assoc "commit" params :test #'string=))))))

(define-test url-parsing ()
  (let* ((parsed-url (parse-url "/?john=doe"))
         (url (elt parsed-url 0))
         (params (elt parsed-url 1)))
    (assert-equal "/" url)
    (assert-equal "doe" (cdr (assoc "john" params :test #'equal))))
  (let* ((parsed-url (parse-url "/blub/this/that?john=doe&jonny=depp&encoded=%2F"))
         (url (elt parsed-url 0))
         (params (elt parsed-url 1)))
    (assert-equal "/blub/this/that" url)
    (assert-equal "doe" (cdr (assoc "john" params :test #'equal)))
    (assert-equal "depp" (cdr (assoc "jonny" params :test #'equal)))
    (assert-equal "/" (cdr (assoc "encoded" params :test #'string=)))))

(define-test query-parsing ()
  (let ((params
         (cdr (assoc
               :params
               (flexi-streams:with-input-from-sequence (s (flexi-streams:string-to-octets (format nil "GET /?john=doe&jim=beam HTTP/1.0~%~%")))
                 (http-protocol-reader s))))))
    (assert-equal "doe" (cdr (assoc "john" params :test #'equal)))
    (assert-equal "beam" (cdr (assoc "jim" params :test #'equal)))))

(define-test http-protocol-writing ()
  (assert-equal
   "HTTP/1.1 200
Location: /path/to/
Content-Type: text/html; charset=utf-8

blub"
   (flexi-streams:octets-to-string
    (flexi-streams:with-output-to-sequence (s)
      (http-protocol-writer
       `(("Location" . "/path/to/")
         (:status . 200)
         (:version . "1.1")
         (:stream . ,s)
         ("Content-Type" . "text/html; charset=utf-8"))
       "blub"
       s)))))

(define-test url-decoding ()
  (assert-equal "http://www.test.org/John%202.jpg" (urlencode:urldecode "http%3A%2F%2Fwww.test.org%2FJohn%25202.jpg")))

(lisp-unit2:with-test-results ()
  (run-tests))
