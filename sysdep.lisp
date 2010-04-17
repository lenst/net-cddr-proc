(in-package "NET.CDDR.PROC")

;;;; Macros

(defmacro %sysdep (desc &rest forms)
  (cond ((null forms)
         (warn "No system dependent code to ~A" desc)
         `(error "No system dependent code to ~A" ,desc))
        (t
         (car forms))))



;;;; Multi process support



(defun current-process ()
  (%SYSDEP
   "return the current process"
   #+(or mcl openmcl) ccl:*current-process*
   #+sb-thread sb-thread:*current-thread*
   :current-process))


(defun start-process (options proc &rest args)
  (declare (ignorable options))
  (%SYSDEP
   "start a process"
   #+(or mcl openmcl)
   (apply #'ccl:process-run-function options proc args)
   #+sb-thread
   (sb-thread:make-thread (lambda () (apply proc args)) :name options)
   #+acl-compat
   (apply #'acl-compat.mp:process-run-function options func args)
   ;; Default: just do it
   (progn (apply proc args)
          nil)))


(defun end-process (process)
  (and process
       (%SYSDEP "end process"
                #+openmcl
                (ccl:process-kill process)
                #+mcl (ccl:process-reset process nil :kill)
                #+sb-thread (sb-thread:terminate-thread process)
                #+acl-compat
                (acl-compat.mp:process-kill process)
                nil)))


(defun process-running-p (process)
  (%SYSDEP "check if process is running"
           #+openmcl (ccl::process-active-p process)
           ;; (member process (ccl:all-processes))
           #+mcl (ccl::process-active-p process)
           ;; (ccl::process-exhausted-p ??)
           #+sb-thread (sb-thread:thread-alive-p process)
           #+acl-compat
           (acl-compat.mp:process-active-p process)
           (progn process)))


(defun make-lock (name)
  (%SYSDEP "make synchronization lock"
           #+(or mcl openmcl)
           (ccl:make-lock name)
           #+sb-thread
           (sb-thread:make-mutex :name name)
           #+acl-compat
           (acl-compat.mp:make-process-lock :name name)
           ;; Default
           `(:lock , name)))


(defmacro with-lock (lock &body body)
  "Execute body with lock held"
  (declare (ignorable lock))
  (%SYSDEP
   "execute body with lock held"
   #+(or mcl openmcl)
   `(ccl:with-lock-grabbed (,lock) ,@body)
   #+sb-thread
   `(sb-thread:with-mutex (,lock) ,@body)
   ;; Default
   `(progn ,@body)))


#+Digitool
(defclass mcl-waitqueue ()
  ((notify   :initform nil  :accessor mcl-notify)))


(defun make-waitqueue ()
  (%SYSDEP
   "Make a condition variable"
   #+sb-thread (sb-thread:make-waitqueue)
   #+openmcl (ccl::make-semaphore)
   #+digitool (make-instance 'mcl-waitqueue)))


(defun wq-locked-wait (wq lock)
  "Wait on a waitqueue, called with lock held."
  (%SYSDEP
   "wq-locked-wait"
   #+sb-thread (sb-thread:condition-wait wq lock)
   #+openmcl (progn
               (ccl:release-lock lock)
               (unwind-protect
                    (ccl:wait-on-semaphore wq)
                 (ccl:grab-lock lock)))
   #+digitool (progn
                (setf (mcl-notify wq) nil)
                (ccl:process-unlock lock)
                (unwind-protect
                  (ccl:process-wait "wq-wait" #'mcl-notify wq)
                  (ccl:process-lock lock)))))


(defun wq-locked-wait-on-condition (wq lock func &rest args)
  #-(or openmcl sb-thread digitool)
  (declare (ignore wq lock))
  (%SYSDEP
   "Wait for (func args) to be true, check when obj is notified"

   #+(or openmcl sb-thread digitool)
   (loop until (apply func args)
        do (wq-locked-wait wq lock))
   
   ;; Default
   (loop until (apply func args)
        do (sleep 0.01))))


(defun wq-notify (wq)
  "Notify waitqueue, wake at least one process waiting on the waitqueue.
Should be called with corresponding lock held."
  (%SYSDEP
   "wq-notify"
   #+sb-thread (sb-thread:condition-notify wq)
   #+openmcl (ccl:signal-semaphore wq)
   #+digitool (setf (mcl-notify wq) t)))



