(defclass ometa-translator (ometa-base) 
  ((grammar-name :accessor grammar-name
                 :initform nil)
   (local-variables  :accessor ometa-local-variables)))


;;   ometa ::= #grammar { atom:name => (.set state-self 'grammar-name name) }
;;                inheritance:i
;;                locals
;;                rules:r => (let* ((gname (.get state-self 'grammar-name))
;;                                    (gnamestr (symbol->string gname))
;;                                    (initf (string->symbol (string-append gnamestr "-init"))))
;;                               `(define (,initf)
;;                                  (let ((,gname (send ,i 'delegated '())))
;;                                    ,@r
;;                                    ,gname)));
(defmethod ometa ((o ometa-translator))
  (core-apply-with-args o 'exactly 'grammar)
  (let ((name (core-apply o 'an-atom)))
    (setf (grammar-name o) name))
  (let ((i (core-apply o 'inheritance)))
    (core-apply o 'locals)
    (let ((r (core-apply o 'rules)))
      (let* ((gname (grammar-name o)))
        (format nil "~a~%~%~{~w~^ ~% ~}" `(defclass ,gname (,i) nil) r)))))

;;   inheritance ::= (#inherit-from atom:i) => i
;;               ;
(defmethod inheritance ((o ometa-translator))
  (core-form o
             (lambda ()
               (core-apply-with-args o 'exactly 'parent)
               (core-apply o 'an-atom))))

;;   locals ::= (#locals ( (atom:rname (atom+:vars))* )) => `(,rname ,@vars) )* ):lst
;;                => (.set state-self 'locals (list->table lst));

(defmethod locals ((o ometa-translator))
  (let ((lst (core-form o
                        (lambda ()
                          (core-apply-with-args o 'exactly 'locals)
                          (core-form o
                                     (lambda ()
                                       (core-many o
                                                  (lambda ()
                                                    (core-form o
                                                               (lambda ()
                                                                 (let ((rname (core-apply o 'an-atom)))
                                                                   (let ((vars (core-form o
                                                                                          (lambda ()
                                                                                            (core-many1 o
                                                                                                        (lambda ()
                                                                                                          (core-apply o 'an-atom)))))))
                                                                     `(,rname ,@vars)))))))))))))
    (setf (ometa-local-variables o) (list->hash-table lst))))

;;   rules ::=  rule+;
(defmethod rules ((o ometa-translator))
  (core-many1 o
              (lambda ()
                (core-apply o 'rule))))

;;   rule ::= (#rule atom:name choice:p)
;;             =>
;;               (let* ((locals (table-ref (.get state-self 'locals) name '()))
;;                      (lt (if (null? locals) p `(let ,(map (lambda (x) (list x ''())) locals) ,p))))
;;                 `(send ,(table-ref (object-layout-state state-self) 'grammar-name '())
;;                    'add-method
;;                    ',name
;;                     (lambda (self state-self)
;;                       (let ((bk (send (.get state-self 'stream) 'get-input)))
;;                         (o-try-catch
;;                          (lambda () ,lt)
;;                          (lambda ()
;;                            (send (.get state-self 'stream) 'set-input bk)
;;                            (send (.get state-self 'stream) 'set-error ',name)
;;                            (ometa-raise ',name)))))));
(defmethod rule ((o ometa-translator))
  (core-form o
             (lambda ()
               (core-apply-with-args o 'exactly 'rule)
               (let ((rname (core-apply o 'an-atom)))
                 (let ((p (core-apply o 'choice)))
                   (let ((locals (gethash rname (ometa-local-variables o))))
                     `(defmethod ,rname ((o ,(grammar-name o)))
                        ,(if (null locals) p `(let ,(map 'cons (lambda (x) (list x nil)) locals) ,p)))))))))
                      
;;   choice ::= (#and choice+:p) => `(begin ,@p)
;;            | (#and)           => ''()
;;            | (#or  choice+:p) => `(send self 'or ,@(map (lambda (x) `(lambda () ,x)) p))
;;            | pattern
;;            ;
(defmethod choice ((o ometa-translator))
  (core-or o
           (lambda ()
             (core-form o
                        (lambda ()
                          (core-apply-with-args o 'exactly 'and)
                          (let ((p (core-many1 o (lambda () (core-apply o 'choice)))))
                            `(progn ,@p)))))
           (lambda ()
             (core-form o (lambda () (core-apply-with-args o 'exactly 'and)))
             ''())
           (lambda ()
             (core-form o 
                        (lambda () 
                          (core-apply-with-args o 'exactly 'or)
                          (let ((p (core-many1 o (lambda () (core-apply o 'choice)))))
                            `(core-or o ,@(map 'list (lambda (x) `(lambda () ,x)) p))))))
           (lambda ()
             (core-apply o 'pattern))))

;;   pattern ::= (#bind atom:a choice:e) => `(begin (set! ,a ,e) ,a)
;;            |  (#action string:ac) => (read (open-input-string ac))
;;            |  expression
;;            ;
(defmethod pattern ((o ometa-translator))
  (core-or o
           (lambda ()
             (core-form o
                        (lambda ()
                          (core-apply-with-args o 'exactly 'bind)
                          (let ((a (core-apply o 'an-atom)))
                            (let ((e (core-apply o 'choice)))
                              `(setq ,a ,e))))))
           (lambda ()
             (core-form o
                        (lambda ()
                          (core-apply-with-args o 'exactly 'action)
                          (let ((ac (core-apply o 'str)))
                            (read-from-string ac)))))
           (lambda ()
             (core-apply o 'expression))))

;;   expression ::= apply-operation
;;               |  apply-w-args-operation
;;               |  apply-super-w-args-operation
;;               |  seq-operation
;;               |  many-operation
;;               |  many1-operation
;;               |  not-operation
;;               |  optional-operation
;;               |  form-operation
;;               |  symbol-operation
;;               |  predicate
;;               |  lookahead-operation
;;               ;
(defmethod expression ((o ometa-translator))
  (core-or o
           (lambda ()
             (core-apply o 'apply-operation))
           (lambda ()
             (core-apply o 'apply-with-args-operation))
           ;; (lambda ()
           ;;   (core-apply o 'apply-super-with-args-operation))
           (lambda ()
             (core-apply o 'apply-super-operation))
           (lambda ()
             (core-apply o 'seq-operation))
           (lambda ()
             (core-apply o 'many-operation))
           (lambda ()
             (core-apply o 'many1-operation))
           (lambda ()
             (core-apply o 'not-operation))
           (lambda ()
             (core-apply o 'optional-operation))
           (lambda ()
             (core-apply o 'form-operation))
           (lambda ()
             (core-apply o 'symbol-operation))
           (lambda ()
             (core-apply o 'predicate))
           (lambda ()
             (core-apply o 'lookahead-operation))))
             

;;   apply-operation ::= (#apply atom:s) => `(send self 'apply ',s);
(defmethod apply-operation ((o ometa-translator))
  (core-form o
             (lambda ()
               (core-apply-with-args o 'exactly 'apply)
               (let ((s (core-apply o 'an-atom)))
                 `(core-apply o ',s)))))

;; apply-super ::= (#apply-super atom:x) => `(core-apply-super o x)
 (defmethod apply-super-operation ((o ometa-translator))
   (core-form o
              (lambda ()
                (core-apply-with-args o 'exactly 'apply-super)
                (let ((r (core-apply o 'an-atom)))
                  `(call-next-method o)))))

;;   apply-w-args-operation ::= (#apply-w-args atom:r (#arguments {atom | (#symbol atom:k => `',k)}*:a)) 
;;                                => `(send self 'apply-with-args ',r ,@a)
;;                                ;
(defmethod apply-with-args-operation ((o ometa-translator))
  (core-form o
             (lambda ()
               (core-apply-with-args o 'exactly 'apply-with-args)
               (let ((r (core-apply o 'an-atom)))
                 (core-form o
                            (lambda ()
                              (core-apply-with-args o 'exactly 'arguments)
                              (let ((a (core-many o
                                                  (lambda ()
                                                    (core-or o
                                                             (lambda ()
                                                               (core-apply o 'an-atom))
                                                             (lambda ()
                                                               (core-form o
                                                                          (lambda ()
                                                                            (core-apply-with-args o 'exactly 'symbol)
                                                                            (let ((k (core-apply o 'an-atom)))
                                                                              `',k)))))))))
                                `(core-apply-with-args o ',r ,@a))))))))

;;   apply-super-w-args-operation ::= (#apply-super-w-args atom:r (#arguments {atom | (#symbol atom:k => `',k)}*:a))
;;                                    => `(send self 'super ',r ,@a)
;;                                 ;
;; (defmethod apply-super-with-args-operation ((o ometa-translator))
;;   (core-form o
;;              (lambda ()
;;                (core-apply-with-args o 'exactly 'apply-super-with-args)
;;                (let ((r (core-apply o 'an-atom)))
;;                  (core-form o
;;                             (lambda ()
;;                               (core-apply-with-args o 'exactly 'arguments)
;;                               (let ((a (core-many o
;;                                                   (lambda ()
;;                                                     (core-or o
;;                                                              (lambda ()
;;                                                                (core-apply o 'an-atom))
;;                                                              (lambda ()
;;                                                                (core-apply-with-args o 'exactly 'symbol)
;;                                                                (let ((k (core-apply o 'an-atom)))
;;                                                                  `',k)))))))
;;                                 `(core-apply-super o ',r ,@a))))))))

;;   seq-operation   ::= (#seq string:s) => (if (eq? (string-length s) 1)
;;                                             `(send self 'apply-with-args 'exactly ',(string-ref s 0))
;;                                             `(send self 'apply-with-args 'seq ',(string->list s)));
(defmethod seq-operation ((o ometa-translator))
  (core-form o
             (lambda ()
               (core-apply-with-args o 'exactly 'seq)
               (let ((s (core-apply o 'str)))
                 (if (eq (array-total-size s) 1)
                     `(core-apply-with-args o 'exactly ,(aref s 0))
                     `(core-apply-with-args o 'seq ',(concatenate 'list s)))))))

;;   many-operation ::= (#many choice:x) => (if (list? (car x))
;;                                                             `(send self 'many (lambda () ,@x))
;;                                                             `(send self 'many (lambda () ,x)));
(defmethod many-operation ((o ometa-translator))
  (core-form o
             (lambda ()
               (core-apply-with-args o 'exactly 'many)
               (let ((x (core-apply o 'choice)))
                 (if (listp (car x))
                     `(core-many o (lambda () ,@x))
                     `(core-many o (lambda () ,x)))))))

;;   many1-operation ::= (#many1 choice:x) => (if (list? (car x))
;;                                                              `(send self 'many1 (lambda () ,@x))
;;                                                              `(send self 'many1 (lambda () ,x)));
(defmethod many1-operation ((o ometa-translator))
  (core-form o 
             (lambda ()
               (core-apply-with-args o 'exactly 'many1)
               (let ((x (core-apply o 'choice)))
                 (if (listp (car x))
                     `(core-many1 o (lambda () ,@x))
                     `(core-many1 o (lambda () ,x)))))))

;;   not-operation ::= (#not choice:x) => (if (list? (car x))
;;                                                          `(send self 'not (lambda () ,@x))
;;                                                          `(send self 'not (lambda () ,x)));
(defmethod not-operation ((o ometa-translator))
  (core-form o
             (lambda ()
               (core-apply-with-args o 'exactly 'not)
               (let ((x (core-apply o 'choice)))
                 (if (listp (car x))
                     `(core-not o (lambda () ,@x))
                     `(core-not o (lambda () ,x)))))))
;;   lookahead-operation ::= (#lookahead choice:x) => (if (list? (car x))
;;                                                                       `(send self 'lookahead (lambda () ,@x))
;;                                                                        `(send self 'lookahead (lambda () ,x)));
(defmethod lookahead-operation ((o ometa-translator))
  (core-form o 
             (lambda ()
               (core-apply-with-args o 'exactly 'lookahead)
               (let ((x (core-apply o 'choice)))
                 (if (listp (car x))
                     `(core-lookahead o (lambda () ,@x))
                     `(core-lookahead o (lambda () ,x)))))))

;;   optional-operation ::= (#optional choice:x) => (if (list? (car x))
;;                                                                    `(send self 'or (lambda () ,@x) (lambda () '()))
;;                                                                    `(send self 'or (lambda () ,x) (lambda () '())));
(defmethod optional-operation ((o ometa-translator))
  (core-form o
             (lambda ()
               (core-apply-with-args o 'exactly 'optional)
               (let ((x (core-apply o 'choice)))
                 (if (listp (car x))
                     `(core-opt (lambda () ,@x))
                     `(core-opt (lambda () ,x)))))))

;;   form-operation ::= (#form choice:x) => `(send self 'form (lambda () ,x));
(defmethod form-operation ((o ometa-translator))
  (core-form o
             (lambda ()
               (core-apply-with-args o 'exactly 'form)
               (let ((x (core-apply o 'choice)))
                 `(core-form o (lambda () ,x))))))

;;   symbol-operation ::= (#symbol atom:x) => `(send self 'apply-with-args 'exactly ',x);
(defmethod symbol-operation ((o ometa-translator))
  (core-form o
             (lambda () 
               (core-apply-with-args o 'exactly 'symbol)
               (let ((x (core-apply o 'an-atom)))
                 `(core-apply-with-args o 'exactly ,x)))))

;;   predicate ::= (#sem-predicate string:s) => `(send self 'pred ,(read (open-input-string s)));
(defmethod predicate ((o ometa-translator))
  (core-form o
             (lambda ()
               (core-apply-with-args o 'exactly 'sem-predicate)
               (let ((s (core-apply o 'str)))
                 `(core-pred o ,(read-from-string s))))))


;;   atom    ::= _:a => (begin (send self 'pred (symbol? a)) a);
(defmethod an-atom ((o ometa-translator))
  (let ((a (core-apply o 'anything)))
    (progn
      (core-pred o (symbolp a)) a)))



;; (defmethod ometa-translate (input rule)
;;   (let* ((o (make-instance 'ometa-translator :input (make-ometa-stream input))))
;;     (let ((res (catch 'ometa (core-apply o rule))))
;;       (if (ometa-error-p res)
;;           (cons 'error (ometa-reporting o))
;;           res))))
