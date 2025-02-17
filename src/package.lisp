(defpackage #:cl-opensubsonic
  (:use :cl)
  (:nicknames :sonic)
  (:local-nicknames (:jzon :com.inuoe.jzon)))

(defpackage #:cl-opensubsonic.client
  (:use :cl-opensubsonic)
  (:nicknames :sonic.client))

(in-package :sonic)
