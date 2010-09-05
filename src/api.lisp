(defun file-string (path)
  (with-open-file (s path)
    (let* ((len (file-length s))
           (data (make-string len)))
      (values data (read-sequence data s)))))


(defun o-report (actf resf)
  (multiple-value-bind (tag val)  (funcall actf)
    (if (eq tag 'error)
        (format nil "Error: ~a" val)
        (funcall resf val))))


(defun ast-for (filepath)
  (let ((data (file-string filepath)))
    (o-report
     (lambda ()
       (ometa-match data 'ometa-parser 'ometa))
     (lambda (ast)
       ast))))


(defun compile-grammar (filepath dest)
  (let ((data (file-string filepath)))
    (o-report
     (lambda ()
       (ometa-match data 'ometa-parser 'ometa))
     (lambda (ast)
       (o-report
        (lambda () (ometa-match ast 'ometa-translator 'ometa))
        (lambda (res)
          (with-open-file (f (ensure-directories-exist dest)
                             :direction :output
                             :if-exists :supersede)
            (format f "~(~{~w ~% ~}~)" res))))))))
