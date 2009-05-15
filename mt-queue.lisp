;;; mt-queue.lisp -- Multithreaded queues

(in-package "NET.CDDR.PROC")



;;;; Shared Queue


(defclass shared-queue (synchronized)
  ((queue  :initform nil  :accessor queue)))


(defmethod enqueue ((q shared-queue) obj)
  (with-synchronization q
    (enqf (queue q) obj)
    (synch-notify q))
  t)


(defmethod dequeue ((q shared-queue) &optional non-blocking)
  (with-synchronization q
    (if non-blocking
        (deqf (queue q))
        (loop
           (if (emptyqp (queue q))
               (synch-locked-wait q)
               (return (deqf (queue q))))))))



;;;; Execution Queue


(defclass execution-queue (synchronized)
  ((executor      :initarg :executor  :initform 'eq-exec :accessor executor)
   (name-template :initarg :name-template :initform "eq" :accessor name-template)  
   (max-processes :initarg :max-processes :initform 10   :accessor max-processes)
   (max-handler   :initarg :max-handler   :initform nil  :accessor max-handler)
   (max-idle      :initarg :max-idle      :initform nil  :accessor max-idle)
   (queue                               :initform nil :accessor eq-queue)
   (idle-count                          :initform 0   :accessor idle-count)
   (process-count                       :initform 0   :accessor process-count)
   (process-table                       :initform nil :accessor process-table)
   (name-count                          :initform 0   :accessor name-count)))


(defmacro with-execution-queue (q &body body)
  `(with-accessors ((max-processes max-processes)
                    (max-handler max-handler)
                    (max-idle max-idle)
                    (queue eq-queue) (idle-count idle-count)
                    (process-table process-table) (process-count process-count)
                    (executor executor) (name-template name-template)
                    (name-count name-count))
                   ,q
     ,@body))


(defmethod reset-queue ((q execution-queue))
  (let ((old-queue 
         (with-execution-queue q
           (setf idle-count 0
                 process-count 0
                 process-table nil)
           (iter (while (not (emptyqp queue)))
                 (collect (deqf queue))))))
    (iter (for x in old-queue)
          (enqueue q x))))


(defmethod next-process-name ((q execution-queue))
  (with-execution-queue q
    (format nil "~A ~D" name-template (incf name-count))))


(defun eq-main (q obj entry)
  (let ((process (current-process)))
    (setf (car entry) process)
    (loop
       (labels ((set-status (status &optional obj)
                  (let ((x (cdr entry)))
                    (setf (car x) status)
                    (setf (cdr x) obj))))
         (set-status :working obj)
         (handler-case (funcall (executor q) obj)
           (serious-condition (condition)
             (warn "Thread fails: ~A thr ~A" condition (current-process))
             (set-status :broken condition)))
         (with-synchronization q
           (with-execution-queue q
             (flet ((exit ()            ; Terminate process
                      (decf process-count)
                      (deletef entry process-table)
                      (return)))
               (cond ((eq (cadr entry) :broken)
                      (exit))
                     ((< idle-count 0)
                      (incf idle-count)
                      (setq obj (deqf queue)))
                     ((or (null max-idle) (< idle-count max-idle))
                      (incf idle-count)
                      (set-status :idle)
                      (do () ((not (emptyqp queue)))
                        (synch-locked-wait q))
                      (setq obj (deqf queue)))
                     (t
                      (exit))))))))))




(defun eq-exec (obj)
  (etypecase obj
    (function (funcall obj))
    (cons (apply (car obj) (cdr obj)))))



(defmethod enqueue ((q execution-queue) obj)
  (with-execution-queue q
    (with-synchronization q
      (flet ((ok-make-more-processes ()
               (or (< process-count max-processes)
                   (if max-handler
                       (funcall max-handler q obj)))))
        (cond ((and (<= idle-count 0) (ok-make-more-processes))
               (incf process-count)
               (let ((entry (list nil nil)))
                 (push entry process-table)
                 (start-process (next-process-name q) #'eq-main q obj entry)))
              (t 
               (when (> idle-count 0)
                 (synch-notify q))
               (enqf queue obj)
               (decf idle-count)
               nil))))))


(defun garb-threads (q)
  (with-execution-queue q
    (with-synchronization q
      (setq process-table
            (remove-if (lambda (entry)
                         (let ((p (car entry)))
                           (unless (process-running-p p)
                             (decf process-count)
                             (when (eql (cadr entry) :idle)
                               (decf idle-count))
                             t)))
                       process-table)))))
