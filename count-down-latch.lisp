;;;; count-down-latch.lisp


(in-package :net.cddr.proc)


(defclass count-down-latch (synchronized)
  ((counter  :initarg :count  :accessor count-of)))


(defmethod count-down ((latch count-down-latch))
  (with-synchronization latch
    (when (zerop (decf (count-of latch)))
      (synch-notify latch))))


(defmethod await-latch ((latch count-down-latch))
  (with-synchronization latch
    (iter (until (zerop (count-of latch)))
          (synch-locked-wait latch))))




;;; count-down-latch.lisp ends here
