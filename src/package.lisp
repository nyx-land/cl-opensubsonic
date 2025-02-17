(defpackage #:cl-opensubsonic
  (:use :cl)
  (:nicknames :sonic)
  (:export :split-api :remove-kebab))

(defpackage #:cl-opensubsonic.client
  (:use :cl :cl-opensubsonic)
  (:nicknames :sonic.client)
  (:local-nicknames (:jzon :com.inuoe.jzon)))

(in-package :sonic)
