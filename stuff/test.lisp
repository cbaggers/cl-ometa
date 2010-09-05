;; (let ((ometa-path (concatenate 'string 
;;                                (directory-namestring 
;;                                 *default-pathname-defaults*) 
;;                                "../")))
;;   (push ometa-path asdf:*central-registry*))


;; --

(require :asdf)
(require :cl-ometa)

(defun file-string (path)
  (with-open-file (s path)
    (let* ((len (file-length s))
           (data (make-string len)))
      (values data (read-sequence data s)))))


 ;;; matching and returning

(defun parse-p (parser)
  (let ((src (file-string "src/ometa-parser.g")))
    (ometa-match src parser 'ometa)))

(defun tr-p (parser)
  (let ((ast (parse-p parser)))
    (ometa-match ast 'ometa-translator 'ometa)))

(defun parse-t (parser)
  (let ((src (file-string "src/ometa-translator.g")))
    (ometa-match src parser 'ometa)))

(defun tr-t (parser)
  (let ((ast (parse-t parser)))
    (ometa-match ast 'ometa-translator 'ometa)))


;;; matching and writing

(defun write-past (parser)
  (let ((ast (parse-p parser)))
    (with-open-file (f (ensure-directories-exist "gen/ometa-parser.ast") 
                       :direction :output
                       :if-exists :supersede)
      (format f "~w" ast))
    ast))


(defun write-psrc (parser)
  (let ((ast (write-past parser)))
    (with-open-file (f (ensure-directories-exist "gen/ometa-parser.lisp") 
                       :direction :output
                       :if-exists :supersede)
      (let ((res (ometa-match ast 'ometa-translator 'ometa)))
        (format f "~(~{~w ~% ~}~)" res)
        res))))


(defun write-tast (parser)
  (let ((ast (parse-p parser)))
    (with-open-file (f (ensure-directories-exist "gen/ometa-translator.ast") 
                       :direction :output
                       :if-exists :supersede)
      (format f "~w" ast))
    ast))


(defun write-tsrc (parser)
  (let ((ast (write-past parser)))
    (with-open-file (f (ensure-directories-exist "gen/ometa-translator.lisp") 
                       :direction :output
                       :if-exists :supersede)
      (let ((res (ometa-match ast 'ometa-translator 'ometa)))
        (format f "~(~{~w ~% ~}~)" res)
        res))))


;;

(defun test-x ()
  (let ((src (file-string "x.g")))
    (break "x")
    (ometa-match src 'ometa-parser 'rule)))

(defun t-err1 ()
  (let* ((src (file-string "x.g")))
    (multiple-value-bind (tag val)  (ometa-match src 'ometa-parser 'ometa)
      (if (eq tag 'error)
          (format t "Error: ~a~%" val)
          val))))

(defun t-err2 ()
  (let ((data (file-string "x.g")))
    (let ((o (make-instance 'ometa-parser :input (make-ometa-stream data))))
      (let ((res (catch 'ometa (core-apply o 'ometa))))
        res))))