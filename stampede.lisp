(defpackage :stampede
  (:use :cl :iolib :cl-ppcre :chanl :alexandria)
  (:export
   :shutdown-server
   :create-server
   :http-protocol-reader
   :http-protocol-writer
   :parse-url
   :create-http-server
   :defroute))
(in-package :stampede)

(defun create-server (port handler &key (worker-threads 1))
  (let* ((socket (make-socket :connect :passive
                              :address-family :internet
                              :type :stream))
         (channel (make-instance 'unbounded-channel))
         (connection-acceptor
          (create-listener socket channel port))
         (pooled-worker-threads
          (create-worker-threads worker-threads handler channel)))
    (lambda ()
      (loop for thread in (list* connection-acceptor pooled-worker-threads)
         do (ignore-errors
              (bt:destroy-thread thread))))))

(defun create-worker-threads (amount handler channel)
  (loop repeat amount
     collect
       (bt:make-thread
        (lambda ()
          (loop
             (let ((stream (recv channel)))
               (unwind-protect
                    (ignore-errors (funcall handler stream))
                 (close stream))))))))

(defun create-listener (socket channel port)
  (progn
    (bind-address socket +ipv4-unspecified+
                  :port port
                  :reuse-address t)
    (listen-on socket :backlog 5)
    (bt:make-thread
     (lambda ()
       (unwind-protect
            (loop
               (let ((stream (accept-connection socket :wait t)))
                 (send channel stream)))
         (close socket))))))

(defclass http-server ()
  ((routes :initform (list (list "GET")
                           (list "POST")
                           (list "PUT"))
           :accessor routes)
   (shutdown-function)))

(defgeneric shutdown-server (server))
(defmethod shutdown-server ((server http-server))
  (funcall (slot-value server 'shutdown-function)))
(defmethod shutdown-server ((server t))
  (funcall server))

(defun http-protocol-reader (stream)
  (let* ((groups (multiple-value-bind (match groups)
                     (scan-to-strings "(\\w+) (\\S+) (\\w+)/(\\S+)" (read-line stream))
                   (declare (ignore match))
                   groups))
         (method (svref groups 0))
         (parsed-url (parse-url (svref groups 1)))
         (url (elt parsed-url 0))
         (params (elt parsed-url 1))
         (http-protocol-version (svref groups 3)))
    (append (list (cons :url url)
                  (cons :version http-protocol-version))
            (if (string= method "GET")
                (list* (cons :params params)
                       (read-get-request stream))
                (read-post-request stream)))))

(defun read-post-request (stream)
  (let ((req
         (loop for line = (read-line stream)
            for (key . value) = (let ((data (split ":" line)))
                                  (cons (first data) (string-trim " " (second data))))
            when (string= "Content-Length" key)
            append (let ((length (parse-integer (subseq line 16))))
                     (let ((data (make-array length :element-type 'character)))
                       (read-line stream)
                       (read-sequence data stream)
                       (list (cons :content-length length)
                             (cons :params (collect-parameters (string data))))))
            until (string= "Content-Length" key)
            collect (cons key value))))
    (cons (cons :method (string-upcase
                         (or (cdr (assoc "_method" (cdr (assoc :params req)) :test #'string=))
                             "POST")))
          req)))

(defun read-get-request (stream)
  (cons (cons :method "GET")
        (loop for line = (read-line stream)
           until (or (string= "" line)
                     (string= "" line))
           collect (let ((data (split ":" line)))
                     (cons (first data) (string-trim " " (second data)))))))

(defun http-protocol-writer (data text stream)
  (format stream "HTTP/~a ~a~c~c" (cdr (assoc :version data)) (cdr (assoc :status data)) #\Return #\Newline)
  (loop for pair in data
     when (not
           (or (equal :version (car pair))
               (equal :status (car pair))))
     do (format stream "~a: ~a~c~c" (car pair) (cdr pair) #\Return #\Newline))
  (format stream "~c~c" #\Return #\Newline)
  (format stream "~a" text))

(defun parse-url (url)
  (let* ((splitted-url (split "\\?" url))
         (url (elt splitted-url 0))
         (query (when (> (length splitted-url) 1)
                  (elt splitted-url 1))))
    (list
     url
     (collect-parameters query))))

(defun collect-parameters (query)
  (let ((items (split "&" query)))
    (loop for item in items
       for (left right) = (split "=" item)
       collect (cons (urlencode:urldecode left :lenientp t :queryp t)
                     (urlencode:urldecode right :lenientp t :queryp t)))))

(defun create-http-server (port &key (worker-threads 1))
  (let* ((server (make-instance 'http-server))
         (shutdown-function
          (create-server port
                         (lambda (stream)
                           (let ((req (http-protocol-reader stream)))
                             (handler-case
                                 (let ((res (list (cons :version (cdr (assoc :version req)))
                                                  (cons :status 200)
                                                  (cons "Content-Type" "text/html"))))
                                   (http-protocol-writer res
                                                         (call-route (routes server) req res)
                                                         stream))
                               (t (e)
                                 (let ((res (list (cons :version (cdr (assoc :version req)))
                                                  (cons :status 200)
                                                  (cons "Content-Type" "text/plain"))))
                                   (http-protocol-writer res
                                                         (format nil "~w~%~%~a" req e)
                                                         stream))))))
                         :worker-threads worker-threads)))
    (setf (slot-value server 'shutdown-function) shutdown-function)
    server))

(defun call-route (routes req res)
  (let* ((url (cdr (assoc :url req)))
         (method (cdr (assoc :method req))))
    (loop for route in (cdr (assoc method routes :test #'string=))
       for (match . groups) = (multiple-value-bind (match groups)
                                  (scan-to-strings (car route) url)
                                (cons match groups))
       when match
       return (progn
                (when (not (alexandria:emptyp groups))
                  (loop for value across (the simple-vector groups)
                     for key in (cadr route)
                     do (push (cons key value) (cdr (assoc :params req)))))
                (funcall (the function (cddr route)) req res)))))


(defun defroute (server method reg fun)
  (let ((regex (regex-replace-all ":[^/]+" reg "([^/$]+)"))
        (params (extract-params-from-regex reg)))
    (let ((exists (assoc regex
                         (cdr (assoc method (routes server) :test #'string=))
                         :test #'string=)))
      (if exists
          (progn
            (setf (car exists) regex)
            (setf (cadr exists) params)
            (setf (cddr exists) fun))
          (let ((item (cons regex (cons params fun))))
            (push item (cdr (assoc method (routes server) :test #'string=))))))))

(defun extract-params-from-regex (reg)
  (let (params)
    (do-register-groups (param) ("/:([^/$]+)" reg)
      (push (make-keyword (string-upcase param)) params))
    (nreverse params)))

