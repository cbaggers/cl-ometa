(in-package :asdf)
(defpackage :cl-ometa (:use :cl :asdf))

(in-package :cl-ometa)

(defsystem :cl-ometa
  :name "cl-ometa"
  :author "Thago Silva <thiago@comum.org>"
  :description "A Common Lisp OMeta implementation"
  :license "MIT"
  :components ((:module "src"
                :serial t
                :components ((:file "packages")
                             (:file "ometa-parser")
                             (:file "ometa-base")
                             (:file "stream")
                             (:file "error")))))
;; (defpackage ometaes
;;   (:use :cl))

;; (in-package :ometa)

;; (defun c-and-l (files)
;;   (dolist (f files)
;;     (compile-file f)
;;     (load f)))

;; (c-and-l '("error" "stream" "ometa-base" "ometa-parser" "test"))
