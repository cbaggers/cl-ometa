
(defun file-string (path)
  (with-open-file (s path)
    (let* ((len (file-length s))
           (data (make-string len)))
      (values data (read-sequence data s)))))

(defun test ()
  (let ((src (file-string "ometa-parser.g")))
    (ometa-match src 'o-ometa)))


(defun texp ()  
  (let ((res (ometa-match "t?;" 'o-choice)))
    (format t "~A" res)
    res))

;; (defun texp ()  
;;   (let ((res (ometa-match "xyzR" 'test1)))
;;     (format t "~A" res)
;;     res))


;; (setq x (make-hash-table))
;; (setf (gethash 'r1 x) '(a b c))
;; (setf (gethash 'r2 x) '(d e f))

;; (setq z (gethash 'um x)) 
;; (maphash (lambda (k v) (cons k (list v))) x)
;; (push 'a (gethash 'um x))

;; (push 'a z)

;; (map 'string (lambda (k) k) x)

;; (with-hash-table-iterator (my-iterator x)
;;     (loop
;;       (multiple-value-bind (entry-p key value)
;;           (my-iterator)
;;         (if entry-p
;;             (print-hash-entry key value)
;;             (return)))))

;; (setq res '())
;; (maphash (lambda (k v) (setq res (cons (list k v) res))) x)
;; res
;; (defun test-s ()
;;   (let ((res (test))) (if (eq (car res) 'error) 'error res)))



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


