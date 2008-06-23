(in-package :cl-user)


(defpackage "NET.CDDR.PROC"
  (:use "COMMON-LISP" "ITERATE" "NET.CDDR.LUNB")

  (:export
   ;; Processes
   #:current-process #:start-process #:end-process #:process-running-p
   ;; Locks etc.
   #:make-lock #:with-lock #:make-waitqueue #:wq-locked-wait #:wq-notify
   ;; Synchronization
   #:lock #:waitqueue #:synchronized #:synchronized-lazy
   #:with-synchronization #:synch-locked-wait #:synch-notify #:synch-wait-on-condition
   ;; Shared Queue
   #:shared-queue
   ;; Execution Queue
   #:execution-queue #:reset-queue #:garb-threads
   ;; Count Down Latch
   #:count-down-latch #:count-down #:await-latch)

  (:import-from
   "NET.CDDR.LIB"

   ;; Misc
   "DEFLEXICAL"

   ;; GENCOL
   "REF" "REMKEY" "REF*" "SIZE" "MAPREF" "MAPGEN" "KEYS"
   "KEYED-COLLECTION" "GENCOL-PROXY" "REAL-OBJ-OF"
   "WITH-REFS" "ITERATOR" "ITER-NEXT"
   "BASE-UNION" "HASH-UNION" "PLIST-UNION" "HASH-TABLE-UNION"
   "SETREF"

   ;; Macros
   "DELETEF" "IFS" "WHEN-LET" "IF-LET" "REC" "WITH-CACHE-SLOT"
   "COLLECTING"

   ;; Simple-Queue
   "DEQF" "ENQF" "EMPTYQP" "SIMPLE-QUEUE" "ENQUEUE" "DEQUEUE" ) )
