;; Copyright (c) 2010 Thiago Silva <thiago@metareload.com>

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
