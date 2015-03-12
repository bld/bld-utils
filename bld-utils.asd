(asdf:defsystem :bld-utils
  :name "bld-utils"
  :author "Benjamin L. Diedrich <ben@solarsails.info>"
  :maintainer "Benjamin L. Diedrich <ben@solarsails.info>"
  :license "MIT"
  :description "Utilities for a variety of tasks"
  :depends-on ("alexandria" #|"let-over-lambda"|#)
  :serial t
  :components
  ((:file "package")
   (:file "utils")))
