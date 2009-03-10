(in-package :cl-user)


(packer:declare-package
 :package "NET.CDDR.PROC"
 :need '("iterate" :net.cddr.lunb (:net.cddr.lib :queue))
 :modules '((nil "package" "sysdep" "mt-queue"
             "count-down-latch" "cyclic-barrier")))

