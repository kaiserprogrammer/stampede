(defpackage :stampede
  (:use :cl :iolib :cl-ppcre :chanl)
  (:export
   :shutdown-server
   :create-server
   :http-protocol-reader
   :http-protocol-writer
   :parse-url))
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

(defun shutdown-server (server)
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
    (append (list (cons :method method)
                  (cons :url url)
                  (cons :version http-protocol-version))
            (if (string= method "GET")
                (list* (cons :params params)
                       (read-get-request stream))
                (read-post-request stream)))))

(defun read-post-request (stream)
  (loop for line = (read-line stream)
     for (key . value) = (let ((data (split ":" line)))
                           (cons (first data) (string-trim " " (second data))))
     when (string= "Content-Length" key)
     append (let ((length (parse-integer (subseq line 16))))
              (let ((data (make-array length :element-type 'character)))
                (read-sequence data stream)
                (list (cons :content-length length)
                      (cons :params (collect-parameters (string data))))))
     until (string= "Content-Length" key)
     collect (cons key value)))

(defun read-get-request (stream)
  (loop for line = (read-line stream)
     until (or (string= "" line)
               (string= "" line))
     collect (let ((data (split ":" line)))
               (cons (first data) (string-trim " " (second data))))))

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
  (let ((items (split "&" (substitute #\  #\+  query))))
    (loop for item in items
       for (left right) = (split "=" item)
       collect (cons left right))))
