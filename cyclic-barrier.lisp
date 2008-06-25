;;;; cyclic-barrier.lisp


(in-package :net.cddr.proc)


(defclass cyclic-barrier (synchronized)
  ((counter             :initarg :count                 :accessor count-of
                        :initform (missing-initarg :count))
   (ready               :initform 0                     :accessor ready-of)
   (barrier-action      :initarg :barrier-action        :accessor barrier-action-of
                        :initform nil) ))


(defmethod await-latch ((barrier cyclic-barrier))
  (with-synchronization barrier
    (assert (< (ready-of barrier) (count-of barrier)))
    (incf (ready-of barrier))
    (cond ((= (ready-of barrier) (count-of barrier))
           (when (barrier-action-of barrier)
             (funcall (barrier-action-of barrier)))
           (setf (ready-of barrier) 0)
           ;; wake others
           (iter (repeat (1- (count-of barrier)))
                 (synch-notify barrier)))
          (t
           (synch-locked-wait barrier)))))


(deftest cyclic-barrier ()
  (let* ((n 5)
         (x 0)
         (latch (make-instance 'cyclic-barrier :count n
                               :barrier-action (lambda () (incf x))))
         (lock (make-lock "m"))
         (m 0))
    (iter (repeat n)
          (start-process "cyclic-barrier test"
                         (lambda ()
                           (await-latch latch)
                           (should (= x 1))
                           (with-lock lock (incf m)))))
    (iter (repeat 10)
          (until (= n (with-lock lock m)))
          (sleep 0.1))
    (should (= n m)))  )



;;; cyclic-barrier.lisp ends here
