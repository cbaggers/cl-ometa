;;;; cl-ometa.asd

(asdf:defsystem #:cl-ometa
  :name "cl-ometa"
  :author "Thago Silva <thiago@comum.org>"
  :description "A Common Lisp implementation of OMeta"
  :license "MIT"
  :serial t
  :components ((:module "src"
                :serial t
                :components ((:file "packages")
                             (:file "util")
                             (:file "ometa-translator")
                             (:file "ometa-parser")
                             (:file "ometa-base")
                             (:file "stream")
                             (:file "api")
                             (:file "error")))))
