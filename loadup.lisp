(in-package :cl-user)

(packer:require-module :net.cddr.lib :queue)

(packer:declare-package
 :package "NET.CDDR.PROC"
 :modules '((nil "package" "sysdep" "mt-queue")))

