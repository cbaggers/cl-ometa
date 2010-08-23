(in-package :ometa)


(defun file-string (path)
  (with-open-file (s path)
    (let* ((len (file-length s))
           (data (make-string len)))
      (values data (read-sequence data s)))))

(defun test ()
  (let ((src (file-string "test.g")))
    (ometa-match src 'o-ometa)))


(defun test-s ()
  (let ((res (test))) (if (eq (car res) 'error) 'error res)))

;; (defun file-forms (path)
;;   "Sucks up an entire file from PATH into a list of forms (sexprs),
;;       returning two values: the list of forms and the number of forms read."
;;   (with-open-file (s path)
;;     (loop with eof = (list nil)
;;           for form = (read s nil eof)
;;           until (eq form eof)
;;           collect form into forms
;;           counting t into form-count
;;           finally (return (values forms form-count)))))


;(format t "~A" (ometa-match "ometa X { ra rb . }" 'rule-ometa))

;; (coerce '(#\a #\b) 'string)

;; (make-string "")
;; ;; (defun ometa-match (input rule)
;; ;;   (let ((o (make-instance 'ometa-parser :input (make-ometa-stream input))))
;; ;;     (core-apply o rule)))


