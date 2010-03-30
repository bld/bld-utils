(defpackage :bld.utils.system 
    (:use :asdf :cl))
(in-package :bld.utils.system)

(defsystem :bld-utils
  :name "bld-utils"
  :author "Benjamin L. Diedrich <ben@solarsails.info>"
  :version "0.0.1"
  :maintainer "Benjamin L. Diedrich <ben@solarsails.info>"
  :license "MIT"
  :description "Utilities for a variety of tasks"

  :components
  ((:file "package")
   (:file "utils" :depends-on ("package"))))
