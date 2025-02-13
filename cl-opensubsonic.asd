(defsystem "cl-opensubsonic"
  :depends-on ("drakma"
               "com.inuoe.jzon"
               "ironclad"
               "quri")
  :components ((:module "src"
                :components
                ((:file "package")
                 (:file "cl-opensubsonic")))))
