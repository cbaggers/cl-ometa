;; Copyright (c) 2010 Thiago Silva <thiago@comum.org>

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
;; THE SOFTWARE.

(defun file-string (path)
  (with-open-file (s path)
    (let* ((len (file-length s))
           (data (make-string len)))
      (values data (read-sequence data s)))))


(defun o-report (actf resf)
  (multiple-value-bind (tag val)  (funcall actf)
    (if (eq tag 'error)
        (values nil (format nil "Error: ~a" val))
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
            (format f "~(~{~w ~% ~}~)" res))
          t))))))
