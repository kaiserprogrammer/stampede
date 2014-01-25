(defpackage :queue-test
  (:use :cl :lisp-unit2))
(in-package :queue-test)

(defun perf-rep ()
  (declare (optimize (speed 3) (safety 0)))
  (let ((iterations 10000000000))
    (do ((rep iterations (1- rep))
         (start (get-internal-real-time)))
        ((zerop rep) (round (/ (* 1000 iterations)
                               (- (get-internal-real-time) start))))
      (declare (type fixnum rep)))))

(defun perf-test ()
  (declare (optimize (speed 3) (safety 0)))
  (let ((stopped? t))
    (declare (type boolean stopped?))
    (let ((thr (bt:make-thread (lambda ()
                                 (do ()
                                     ((not stopped?)
                                      (do ((runs 0 (1+ runs)))
                                          (stopped? runs)
                                        (declare (type fixnum runs)))))))))
      (sleep 0.001)
      (setf stopped? nil)
      (time (sleep 1))
      (setf stopped? t)
      (bt:join-thread thr))))

(defun perf-mailbox-test ()
  (declare (optimize (speed 3) (safety 0)))
  (let ((stopped? t))
    (declare (type boolean stopped?))
    (let ((thr (bt:make-thread (lambda ()
                                 (do ((queue (sb-concurrency:make-mailbox)))
                                     ((not stopped?)
                                      (do ((runs 0 (1+ runs)))
                                          (stopped? runs)
                                        (declare (type fixnum runs))
                                        (sb-concurrency:send-message queue 1)
                                        (sb-concurrency:receive-message queue))))))))
      (sleep 0.001)
      (setf stopped? nil)
      (time (sleep 1))
      (setf stopped? t)
      (bt:join-thread thr))))

(defun perf-list-test ()
  (declare (optimize (speed 3) (safety 0)))
  (let ((stopped? t))
    (declare (type boolean stopped?))
    (let ((thr (bt:make-thread (lambda ()
                                 (do ((queue (list)))
                                     ((not stopped?)
                                      (do ((runs 0 (1+ runs)))
                                          (stopped? runs)
                                        (declare (type fixnum runs))
                                        (push 1 queue)
                                        (pop queue))))))))
      (sleep 0.001)
      (setf stopped? nil)
      (time (sleep 1))
      (setf stopped? t)
      (bt:join-thread thr))))

(defun perf-queue-test ()
  (declare (optimize (speed 3) (safety 0)))
  (let* ((repitions (* 10000000))
         (test-value 777)
         (queue (sb-concurrency:make-mailbox))
         (start (get-internal-real-time))
         (producer (bt:make-thread (lambda ()
                                     (do ((i 0 (1+ i)))
                                         ((= i repitions))
                                       (declare (type fixnum i))
                                       (do ((result (not (sb-concurrency:send-message queue test-value)) (not (sb-concurrency:send-message queue test-value))))
                                           (result)
                                         (bt:thread-yield)))))))
    (do ((i 0 (1+ i)))
        ((= i repitions))
      (declare (type fixnum i))
      (do ((result (sb-concurrency:receive-message-no-hang queue) (sb-concurrency:receive-message-no-hang queue)))
          (result)
        (bt:thread-yield)))
    (bt:join-thread producer)
    (let* ((duration (- (get-internal-real-time) start))
           (ops (round (/ (* repitions 1000) duration))))
      (format t "ops/sec=~a -" ops))))

(defun perf-list2-test ()
  (declare (optimize (speed 3) (safety 0)))
  (let* ((repitions (* 10000000))
         (test-value 777)
         (queue (list))
         (start (get-internal-real-time)))
    (declare (type list queue))
    (let ((producer (bt:make-thread (lambda ()
                                      (do ((i 0 (1+ i)))
                                          ((= i repitions))
                                        (declare (type fixnum i))
                                        (do ((result (push test-value queue) (push test-value queue)))
                                            (result)
                                          (bt:thread-yield)))))))
      (do ((i 0 (1+ i)))
          ((= i repitions))
        (declare (type fixnum i))
        (do ((result (pop queue) (pop queue)))
            (result)
          (bt:thread-yield)))
      (bt:join-thread producer))
    (let* ((duration (- (get-internal-real-time) start))
           (ops (round (/ (* repitions 1000) duration))))
      (format t "ops/sec=~a -" ops))))

(defstruct conc
  (buffer #() :type simple-vector)
  (mask 0 :type fixnum)
  (size 0 :type fixnum)
  p1 p2 p3 p4 p5
  (tail 0 :type fixnum)
  r1 r2 r3 r4 r5
  (head 0 :type fixnum))

(defun make-array-queue (capacity)
  (let ((size (expt 2 (ceiling (log capacity 2)))))
    (make-conc :mask (1- size) :size size :buffer (make-array (list size) :initial-element nil))))
(defun offer (queue value)
  (declare (optimize (speed 3) (safety 0) (compilation-speed 3) (debug 0)))
  (sb-thread:barrier (:read)
    (if (null value)
        (error 'nil-value)
        (let* ((tail (conc-tail queue))
               (size (conc-size queue))
               (wrap (- tail size)))
          (declare (type fixnum tail size wrap))
          (if (<= (conc-head queue) wrap)
              nil
              (progn
                (setf (svref (conc-buffer queue) (logand tail (conc-mask queue))) value)
                (incf (conc-tail queue))))))))

(defun take (queue)
  (declare (optimize (speed 3) (safety 0) (compilation-speed 3) (debug 0)))
  (sb-thread:barrier (:read)
    (let ((head (conc-head queue)))
      (if (>= head (conc-tail queue) 0)
          nil
          (let* ((index (logand head (conc-mask queue)))
                 (e (svref (conc-buffer queue) index)))
            (setf (svref (conc-buffer queue) index) nil)
            (incf (conc-head queue))
            e)))))

(defun perf-single-thread-array-test ()
  (declare (optimize (speed 3) (safety 0)))
  (sb-ext:gc :full t)
  (let* ((repitions (* 100000000))
         (queue (make-array-queue (* 32 1024)))
         (start (get-internal-real-time)))
    (do ((i 0 (1+ i)))
        ((= i repitions))
      (declare (type fixnum i))
      (do ()
          ((offer queue i)))
      (do ()
          ((take queue))))
    (let* ((duration (- (get-internal-real-time) start))
           (ops (round (/ (* repitions 1000) duration))))
      (format t "ops/sec=~a -" ops))))


(defun perf-array-test ()
  (declare (optimize (speed 3) (safety 0)))
  (sb-ext:gc :full t)
  (let* ((repitions (* 100000000))
         (queue (make-array-queue (* 32 1024)))
         (start (get-internal-real-time)))
    (let ((producer (bt:make-thread (lambda ()
                                      (do ((i 0 (1+ i)))
                                          ((= i repitions))
                                        (declare (type fixnum i))
                                        (do ()
                                            ((offer queue i))
                                          ;; better result than
                                          ;; bt:thread-yield
                                          (sleep 0.0000001)))))))
      (do ((i 0 (1+ i)))
          ((= i repitions))
        (declare (type fixnum i))
        (do ()
            ((take queue))))
      (bt:join-thread producer))
    (let* ((duration (- (get-internal-real-time) start))
           (ops (round (/ (* repitions 1000) duration))))
      (format t "ops/sec=~a -" ops))))
