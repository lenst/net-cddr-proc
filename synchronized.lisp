;;;; synchronized.lisp -- Mixin for synchronized objects


(in-package "NET.CDDR.PROC")


(defgeneric lock (obj)
  (:documentation
   "Return the synchronization lock for object"))

(defgeneric waitqueue (obj)
  (:documentation
   "Return the waitqueue/semaphore/condition etc. for the object."))

(defclass synchronized ()
  ((lock       :initform (make-lock "syn")   :reader lock)
   (waitqueue  :initform (make-waitqueue)    :reader waitqueue)))


(defclass synchronized-lazy (synchronized)
  ((waitqueue  :initform nil  )))

(defmethod waitqueue ((obj synchronized-lazy))
  (or (slot-value obj 'waitqueue)
      (setf (slot-value obj 'waitqueue)
            (make-waitqueue))))


(defmacro with-synchronization (obj &body body)
  `(with-lock (lock ,obj)
     ,@body))


(defun synch-locked-wait (obj)
  "Synchronize wait on locked object. 
Object should have (lock _) and (waitqueue _)"
  (wq-locked-wait (waitqueue obj) (lock obj)))


(defun synch-notify (obj)
  (wq-notify (waitqueue obj)))


(defun synch-wait-on-condition (obj wait-func &rest wait-args)
  (with-synchronization obj
    (apply #'wq-locked-wait-on-condition
           (waitqueue obj) (lock obj)
           wait-func wait-args)))



;;; synchronized.lisp ends here
