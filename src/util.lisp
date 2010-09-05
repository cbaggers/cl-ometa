(defun newline-p (chr)
  (eq chr #\Newline))

(defun nil-paren-print (stream _)
  (format stream "()"))

(defun str-trim (str)
  (string-trim '(#\Space #\Tab #\Newline) str))

(defun list->hash-table (lst)
  (let ((h (make-hash-table)))
    (dolist (l lst)
      (let ((rname (car l))
            (vars (cdr l)))
        (setf (gethash rname h) vars)))
    h))

(defun read-all-from-string (str)
  (labels ((read-all (str r)
             (let* ((st (string-right-trim '(#\Space) str))
                    (res (multiple-value-list (read-from-string st)))
                    (seq (car res))
                    (pos (cadr res)))
               (if (eq (array-total-size st)  pos)
                   (progn
                     (cons seq r))
                   (progn
                     (read-all (subseq str pos) (cons seq r)))))))
    (reverse (read-all str nil))))
    