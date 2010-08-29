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
                             (:file "specials")
                             (:file "utils")
                             (:file "tree")
                             (:file "tests")))))

;; (require :asdf)
;; (require :asdf-install)
;; (asdf-install:install :avl-tree)


;(in-package :cl-user)
;(asdf:oos 'asdf:load-op :avl-tree)

;; (defpackage ometa
;;   (:use :cl :avl-tree))

(defpackage ometa
  (:use :cl))

(in-package :ometa)

(defun c-and-l (files)
  (dolist (f files)
    (compile-file f)
    (load f)))

(c-and-l '("error" "stream" "ometa-base" "ometa-parser" "test"))
