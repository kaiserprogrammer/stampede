(defpackage :stampede
  (:use :cl :cl-ppcre :alexandria :anaphora)
  (:export
   :shutdown-server
   :run-server
   :http-protocol-reader
   :http-protocol-writer
   :parse-url
   :run-http-server
   :make-http-server
   :start
   :stop
   :running?
   :defroute
   :write-headers
   :set-response-written
   :write-response))
(in-package :stampede)

(defun run-server (port handler &key (worker-threads 1) (debug t))
  (let* ((socket (iolib:make-socket :connect :passive
                                    :address-family :internet
                                    :type :stream
                                    :external-format :latin1))
         (channel (sb-concurrency:make-mailbox))
         (pooled-worker-threads
          (list* (progn
                   (iolib:bind-address socket iolib:+ipv4-unspecified+
                                       :port port
                                       :reuse-address t)
                   (iolib:listen-on socket :backlog 200)
                   (bt:make-thread
                    (lambda ()
                      (unwind-protect
                           (loop
                              (let ((stream (iolib:accept-connection socket :wait t)))
                                (sb-concurrency:send-message channel stream)))
                        (iolib.sockets:shutdown socket :read t :write t)
                        (close socket)))
                    :name "acceptor"))
                 (loop repeat worker-threads
                    for i from 1
                    collect
                      (bt:make-thread
                       (lambda ()
                         (loop
                            (let ((stream (handler-case (sb-concurrency:receive-message channel :timeout 1)
                                            (sb-concurrency::timeout (e) (declare (ignore e)) nil))))
                              (when stream
                                (handler-case (bt:with-timeout (5)
                                                (unwind-protect
                                                     (funcall handler stream)
                                                  (close stream)))
                                  (bt:timeout (e) (declare (ignore e)) (close stream))
                                  (t (e) (unwind-protect
                                              (when debug
                                                (error e))
                                           (close stream))))))))
                       :name (format nil "worker~a" i))))))
    (lambda ()
      (progn (loop for thread in pooled-worker-threads
                do (ignore-errors
                     (bt:destroy-thread thread)))))))

(defclass http-server ()
  ((routes :initform (list (list "GET"))
           :accessor routes)
   (stop-function)
   (start-function)
   (running :initform nil
            :reader running?)
   (workers :initform 1
            :initarg :workers
            :accessor workers)
   (port :initarg :port
         :accessor port)
   (debug-mode :initarg :debug-mode
               :initform nil
               :accessor debug-mode)))

(defgeneric stop (server))
(defmethod stop ((server http-server))
  (when (slot-value server 'running)
    (setf (slot-value server 'running) nil)
    (funcall (slot-value server 'stop-function))))

(defmethod stop ((server t))
  (funcall server))

(defun my-read-line (stream)
  (bt:with-timeout (1)
    (with-output-to-string (str)
      (loop for byte = (read-byte stream nil nil)
         while byte
         when (= byte 13)
         do (progn (read-byte stream)
                   (return))
         do (write-char (code-char byte) str)))))

(defun http-protocol-reader (stream)
  (let* ((read (do ((read (my-read-line stream) (my-read-line stream)))
                   ((> (length read) 0) read)))
         (groups (multiple-value-bind (match groups)
                     (scan-to-strings "(\\w+) (\\S+) (\\w+)/(\\S+)" read)
                   (declare (ignore match))
                   (if groups
                       groups
                       (error 'simple-error))))
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
  (let* ((length nil)
         (req
          (loop for line = (my-read-line stream)
             when (string= line "")
             collect (if (> length 0)
                         (let ((data (make-array length :element-type '(unsigned-byte 8))))
                           (read-sequence data stream)
                           (cons :params (collect-parameters (sb-ext:octets-to-string data))))
                         (cons :params (list)))
             until (string= "" line)
             collect (let ((data (split ":" line :limit 2)))
                       (if (string= "Content-Length" (first data))
                           (progn (setf length (parse-integer (subseq line 16)))
                                  (cons :content-length length))
                           (cons (first data) (string-trim " " (second data))))))))
    (cons (cons :method (string-upcase
                         (or (cdr (assoc "_method" (cdr (assoc :params req)) :test #'string=))
                             "POST")))
          req)))

(defun read-get-request (stream)
  (cons (cons :method "GET")
        (loop for line = (my-read-line stream)
           until (or (string= "" line))
           collect (let ((data (split ":" line :limit 2)))
                     (cons (first data) (string-trim " " (second data)))))))

(defun write-headers (data)
  (anaphora:swhen (assoc :headers-written data)
    (setf (cdr anaphora:it) t))
  (let ((stream (assoc-value data :stream)))
    (write-sequence (sb-ext:string-to-octets (format nil "HTTP/~a ~a~c~c" (cdr (assoc :version data)) (cdr (assoc :status data)) #\Return #\Newline)) stream)
    (loop for pair in data
       when (not
             (or (eql :version (car pair))
                 (eql :status (car pair))
                 (eql :headers-written (car pair))
                 (eql :response-written (car pair))
                 (eql :stream (car pair))))
       do (write-sequence (sb-ext:string-to-octets (format nil "~a: ~a~c~c" (car pair) (cdr pair) #\Return #\Newline)) stream))
    (write-sequence (sb-ext:string-to-octets (format nil "~c~c" #\Return #\Newline)) stream)))

(defun write-response (text stream)
  (let ((text (princ-to-string text)))
    (write-sequence (sb-ext:string-to-octets text) stream)))

(defun set-response-written (data)
  (swhen (assoc :response-written data)
    (setf (cdr it) t)))

(defun http-protocol-writer (data text stream)
  (unless (assoc-value data :headers-written)
    (write-headers data))
  (unless (assoc-value data :response-written)
    (write-response text stream)))

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

(defun process-http (server stream)
  (let* ((ssl-stream stream)
         (res (list (cons :headers-written nil)
                    (cons :response-written nil)
                    (cons :stream ssl-stream)
                    (cons :version "1.1")
                    (cons :status 200)
                    (cons "Date"
                          (local-time:to-rfc1123-timestring
                           (local-time:now)))
                    (cons "Content-Type" "text/html")))
         (req (list (cons :remote-port (iolib:remote-port stream))
                    (cons :remote-host (iolib:remote-host stream)))))
    (unwind-protect
         (handler-case
             (progn
               (nconc req (http-protocol-reader ssl-stream))
               (http-response ssl-stream (call-route (routes server) req res) res))
           (bt:timeout (e) e)
           (t (e)
             (if (debug-mode server)
                 (error e)
                 (progn (setf (cdr (assoc "Content-Type" res :test #'equal)) "text/plain")
                        (http-response ssl-stream (with-output-to-string (*standard-output*)
                                                    (princ e)
                                                    (print req)
                                                    (print res)) res)))))
      (when (or (equal "1.0" (cdr (assoc :version req)))
                (equal (cdr (assoc "Connection" req :test #'equal)) "close"))
        (close stream)
        :closed))))

(defun http-response (stream response res)
  (unless (assoc "Content-Length" res :test #'equal)
    (nconc res (list (cons "Content-Length" (princ-to-string (length response))))))
  (http-protocol-writer res
                        response
                        stream)
  (force-output stream))

(defun default-start-function (server)
  (run-server (port server)
              (lambda (stream) (process-http server stream))
              :worker-threads (workers server)
              :debug (debug-mode server)))

(defun make-http-server (port &key (worker-threads 1) (debug-mode nil))
  (let* ((server (make-instance 'http-server :workers worker-threads :port port :debug-mode debug-mode))
         (start-function
          (lambda () (default-start-function server))))
    (setf (slot-value server 'start-function) start-function)
    server))

(defun run-http-server (port &key (worker-threads 1) (debug-mode nil))
  (let ((server (make-http-server port :worker-threads worker-threads :debug-mode debug-mode)))
    (start server)))

(defmethod start ((server http-server))
  (unless (running? server)
    (let ((stop-function
           (funcall (slot-value server 'start-function))))
      (setf (slot-value server 'running) t)
      (setf (slot-value server 'stop-function) stop-function)))
  server)

(define-condition no-route-found (error)
  ((url :accessor url :initarg :url))
  (:report (lambda (c s)
             (format s "No route found for url: ~s" (url c)))))

(defun call-route (routes req res)
  (let* ((url (cdr (assoc :url req)))
         (method (cdr (assoc :method req))))
    (let ((action
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
      (if action
          action
          (error 'no-route-found :url url)))))

(defun defroute (server method reg fun)
  (add-routing-method server method)
  (let ((regex (regex-replace-all ":\\w+" reg "([^/$]+?)"))
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

(defun add-routing-method (server method)
  (unless (assoc method (routes server) :test 'equal)
    (nconc (routes server) (list (list method)))))

(defun extract-params-from-regex (reg)
  (let (params)
    (do-register-groups (param) (":(\\w+)" reg)
      (push (make-keyword (string-upcase param)) params))
    (nreverse params)))
