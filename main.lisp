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


(load "error")
(load "stream")
(load "ometa-base")
(load "ometa-parser")
(load "test")