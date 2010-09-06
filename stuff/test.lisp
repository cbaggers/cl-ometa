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


(defun o-report (actf resf)
  (multiple-value-bind (tag val)  (funcall actf)
    (if (eq tag 'error)
        (format nil "Error: ~a" val)
        (funcall resf val))))
    
 ;;; matching and returning

(defun parse-p ()
  (let ((src (file-string "src/ometa-parser.g")))
    (o-report 
     (lambda ()
       (ometa-match src 'ometa-parser 'ometa))
     (lambda (res)
       res))))

(defun tr-p ()
  (let ((ast (parse-p)))
    (if ast 
        (o-report 
         (lambda ()
           (ometa-match ast 'ometa-translator 'ometa))
         (lambda (res)
           res)))))

(defun parse-t ()
  (let ((src (file-string "src/ometa-translator.g")))
    (o-report
     (lambda ()
       (ometa-match src 'ometa-parser 'ometa))
     (lambda (res) res))))

(defun tr-t ()
  (let ((ast (parse-t)))
    (if ast
        (o-report
         (lambda ()
           (ometa-match ast 'ometa-translator 'ometa))
         (lambda (res) res)))))


;;; matching and writing

(defun write-past ()
  (let ((ast (parse-p)))
    (if ast
        (with-open-file (f (ensure-directories-exist "gen/ometa-parser.ast") 
                           :direction :output
                           :if-exists :supersede)
          (format f "~w" ast)))
    ast))


(defun write-psrc ()
  (let ((ast (write-past)))
    (if ast
        (o-report 
         (lambda () (ometa-match ast 'ometa-translator 'ometa))
         (lambda (res)
           (with-open-file (f (ensure-directories-exist "gen/ometa-parser.lisp") 
                              :direction :output
                              :if-exists :supersede)
             (format f "~(~{~w ~% ~}~)" res)))))))

(defun write-tast ()
  (let ((ast (parse-t)))
    (if ast
        (with-open-file (f (ensure-directories-exist "gen/ometa-translator.ast") 
                           :direction :output
                           :if-exists :supersede)
          (format f "~w" ast)))
    ast))


(defun write-tsrc ()
  (let ((ast (write-tast)))
    (if ast
        (o-report
         (lambda () (ometa-match ast 'ometa-translator 'ometa))
         (lambda (res)
           (with-open-file (f (ensure-directories-exist "gen/ometa-translator.lisp") 
                              :direction :output
                              :if-exists :supersede)
             (format t "~(~{~w ~% ~}~)" res)
             (format f "~(~{~w ~% ~}~)" res)))))))


(defun gen1 ()
  (write-psrc)
  (write-tsrc))

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



(defun t-exp ()
  (multiple-value-bind (tag val)  (ometa-match "ometa x { 1a = x; }" 'ometa-parser 'ometa)
    (if (eq tag 'error)
        val
        (ometa-match  val 'ometa-translator 'ometa))))
  

(defun match (filepath dest)
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
