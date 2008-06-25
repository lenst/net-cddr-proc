;;;; count-down-latch.lisp


(in-package :net.cddr.proc)


(defun missing-initarg (&optional keyword)
  (if keyword
      (error "Missing initarg ~S" keyword)
      (error "Missing initarg")))


(defclass count-down-latch (synchronized)
  ((counter  :initarg :count    :accessor count-of
             :initform (missing-initarg :count))
   (waiting  :initform 0        :accessor waiting-of)))


(defmethod count-down ((latch count-down-latch))
  (with-synchronization latch
    (when (zerop (decf (count-of latch)))
      (synch-notify latch))))


(defmethod await-latch ((latch count-down-latch))
  (with-synchronization latch
    (incf (waiting-of latch))
    (iter (until (zerop (count-of latch)))
          (synch-locked-wait latch))
    (unless (zerop (decf (waiting-of latch)))
      (synch-notify latch))))



(deftest count-down-latch ()
  (let* ((n 3)
         (latch (make-instance 'count-down-latch :count n))
         (lock (make-lock "m"))
         (m 0))
    (iter (repeat n)
          (start-process "count-down-latch test"
                         (lambda ()
                           (count-down latch)
                           (await-latch latch)
                           (with-lock lock (incf m)))))
    (iter (repeat 10)
          (until (= n (with-lock lock m)))
          (sleep 0.1))
    (should (= n m)))  )



;;; count-down-latch.lisp ends here
