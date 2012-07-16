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
  (let ((req (with-input-from-string
                 (s (format nil "GET /url HTTP/1.0~%blub: that~%~%"))
               (http-protocol-reader s))))
    (loop for (left right) in '((:method  "GET")
                                (:url "/url")
                                (:version "1.0")
                                ("blub" "that"))
       do (is (equal right
                     (cdr (assoc left req :test #'equal))))))
  (let ((req (with-input-from-string (s (format nil "GET /url HTTP/1.0~%blub: that~%~%"))
               (http-protocol-reader s))))
    (loop for (left right) in '((:method "GET")
                                  (:url "/url")
                                  (:version "1.0")
                                  ("blub" "that"))
       do (is (equal right
                     (cdr (assoc left req :test #'equal)))))))

(test url-parsing
  (let* ((parsed-url (parse-url "/?john=doe"))
        (url (elt parsed-url 0))
         (params (elt parsed-url 1)))
    (is (equal "/" url))
    (is (equal "doe" (cdr (assoc "john" params :test #'equal)))))
  (let* ((parsed-url (parse-url "/blub/this/that?john=doe&jonny=depp"))
        (url (elt parsed-url 0))
         (params (elt parsed-url 1)))
    (is (equal "/blub/this/that" url))
    (is (equal "doe" (cdr (assoc "john" params :test #'equal))))
    (is (equal "depp" (cdr (assoc "jonny" params :test #'equal))))))

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
Content-Type: text/html; charset=utf-8

blub"
       (with-output-to-string (s)
         (http-protocol-writer
          '((:status . 200)
            (:version . "1.1")
            ("Content-Type" . "text/html; charset=utf-8"))
          "blub"
          s)))))

(run!)
