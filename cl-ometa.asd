(in-package :asdf)
(defpackage :cl-ometa (:use :cl :asdf))

(in-package :cl-ometa)

(defsystem :cl-ometa
  :name "cl-ometa"
  :author "Thago Silva <thiago@comum.org>"
  :description "A Common Lisp implementation of OMeta"
  :license "MIT"
  :components ((:module "src"
                :serial t
                :components ((:file "packages")
                             (:file "ometa-translator")
                             (:file "ometa-parser")
                             (:file "ometa-base")
                             (:file "stream")
                             (:file "error")))))
