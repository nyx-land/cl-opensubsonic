(defsystem "cl-opensubsonic"
  :depends-on ("drakma"
               "com.inuoe.jzon"
               "ironclad"
               "quri")
  :serial t
  :components ((:module "src"
                :components
                ((:file "package")
                 (:file "util")
                 (:file "client")))))
