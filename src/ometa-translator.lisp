(defclass ometa-translator (ometa-base)
          ((grammar-name :accessor grammar-name :initform nil)
           (local-variables :accessor ometa-local-variables))) 
 (defmethod ometa ((o ometa-translator))
   (let ((r nil) (ic nil) (sl nil) (i nil) (name nil))
     (core-or o
              (lambda ()
                (core-or o
                         (lambda ()
                           (progn
                            (core-apply-with-args o 'exactly 'grammar)
                            (core-or o
                                     (lambda ()
                                       (progn
                                        (setq name (core-apply o 'an-atom))
                                        (setf (grammar-name o) name))))
                            (setq i (core-apply o 'inheritance))
                            (core-apply o 'locals)
                            (setq sl (core-apply o 'slots))
                            (setq ic (core-apply o 'inline-code))
                            (setq r (core-apply o 'rules))
                            (core-apply o 'end)
                            (if ic
                                `((defclass ,name (,i) ,sl) ,@ic ,@r)
                                `((defclass ,name (,i) ,sl) ,@r))))))))) 
 (defmethod inheritance ((o ometa-translator))
   (core-or o
            (lambda ()
              (core-or o
                       (lambda ()
                         (progn
                          (core-form o
                                     (lambda ()
                                       (progn
                                        (core-apply-with-args o 'exactly
                                                              'parent)
                                        (core-apply o 'an-atom)))))))))) 
 (defmethod locals ((o ometa-translator))
   (let ((lst nil) (vars nil) (rname nil))
     (core-or o
              (lambda ()
                (core-or o
                         (lambda ()
                           (progn
                            (setq lst
                                    (core-form o
                                               (lambda ()
                                                 (progn
                                                  (core-apply-with-args o
                                                                        'exactly
                                                                        'locals)
                                                  (core-form o
                                                             (lambda ()
                                                               (progn
                                                                (core-many o
                                                                           (lambda
                                                                               ()
                                                                             (core-form
                                                                              o
                                                                              (lambda
                                                                                  ()
                                                                                (progn
                                                                                 (core-or
                                                                                  o
                                                                                  (lambda
                                                                                      ()
                                                                                    (progn
                                                                                     (setq rname
                                                                                             (core-apply
                                                                                              o
                                                                                              'an-atom))
                                                                                     (setq vars
                                                                                             (core-form
                                                                                              o
                                                                                              (lambda
                                                                                                  ()
                                                                                                (progn
                                                                                                 (core-many1
                                                                                                  o
                                                                                                  (lambda
                                                                                                      ()
                                                                                                    (core-apply
                                                                                                     o
                                                                                                     'an-atom)))))))
                                                                                     `(,rname
                                                                                       ,@vars))))))))))))))))
                            (setf (ometa-local-variables o)
                                    (list->hash-table lst))))))))) 
 (defmethod slots ((o ometa-translator))
   (let ((s nil))
     (core-or o
              (lambda ()
                (core-or o
                         (lambda ()
                           (progn
                            (core-form o
                                       (lambda ()
                                         (progn
                                          (core-apply-with-args o 'exactly
                                                                'slots)
                                          (core-or o
                                                   (lambda ()
                                                     (progn
                                                      (setq s
                                                              (core-apply o
                                                                          'str))
                                                      (read-from-string s)))
                                                   (lambda ()
                                                     (progn
                                                      (core-apply-with-args o
                                                                            'exactly
                                                                            'nil)))))))))))))) 
 (defmethod inline-code ((o ometa-translator))
   (let ((s nil))
     (core-or o
              (lambda ()
                (core-or o
                         (lambda ()
                           (progn
                            (core-form o
                                       (lambda ()
                                         (progn
                                          (core-apply-with-args o 'exactly
                                                                'inline)
                                          (core-or o
                                                   (lambda ()
                                                     (progn
                                                      (setq s
                                                              (core-apply o
                                                                          'str))
                                                      (read-all-from-string
                                                       s)))
                                                   (lambda ()
                                                     (progn
                                                      (core-apply-with-args o
                                                                            'exactly
                                                                            'nil)))))))))))))) 
 (defmethod rules ((o ometa-translator))
   (core-or o
            (lambda ()
              (core-or o
                       (lambda ()
                         (progn
                          (core-many1 o (lambda () (core-apply o 'rule))))))))) 
 (defmethod rule ((o ometa-translator))
   (let ((p nil) (rname nil))
     (core-or o
              (lambda ()
                (core-or o
                         (lambda ()
                           (progn
                            (core-form o
                                       (lambda ()
                                         (progn
                                          (core-apply-with-args o 'exactly
                                                                'rule)
                                          (setq rname (core-apply o 'an-atom))
                                          (setq p (core-apply o 'choice)))))
                            (let ((locals
                                   (gethash rname (ometa-local-variables o))))
                              (let ((m
                                     `(defmethod ,rname
                                                 ((o ,(grammar-name o))))))
                                (if (null locals)
                                    (append m (list p))
                                    (let ((llet
                                           (list 'let
                                                 (map 'cons
                                                      (lambda (x) (list x nil))
                                                      locals))))
                                      (append m
                                              (list
                                               (append llet
                                                       (list p))))))))))))))) 
 (defmethod choice ((o ometa-translator))
   (let ((p nil))
     (core-or o
              (lambda ()
                (core-or o
                         (lambda ()
                           (progn
                            (core-form o
                                       (lambda ()
                                         (progn
                                          (core-apply-with-args o 'exactly
                                                                'and)
                                          (setq p
                                                  (core-many1 o
                                                              (lambda ()
                                                                (core-apply o
                                                                            'choice)))))))
                            `(progn ,@p)))
                         (lambda ()
                           (progn
                            (core-form o
                                       (lambda ()
                                         (progn
                                          (core-apply-with-args o 'exactly
                                                                'and))))
                            ''nil))
                         (lambda ()
                           (progn
                            (core-form o
                                       (lambda ()
                                         (progn
                                          (core-apply-with-args o 'exactly 'or)
                                          (setq p
                                                  (core-many1 o
                                                              (lambda ()
                                                                (core-apply o
                                                                            'choice)))))))
                            `(core-or o
                                      ,@(map 'list
                                             (lambda (x) `(lambda ,nil ,x))
                                             p))))
                         (lambda () (progn (core-apply o 'pattern)))))))) 
 (defmethod pattern ((o ometa-translator))
   (let ((ac nil) (e nil) (a nil))
     (core-or o
              (lambda ()
                (core-or o
                         (lambda ()
                           (progn
                            (core-form o
                                       (lambda ()
                                         (progn
                                          (core-apply-with-args o 'exactly
                                                                'bind)
                                          (setq a (core-apply o 'an-atom))
                                          (setq e (core-apply o 'choice)))))
                            `(setq ,a ,e)))
                         (lambda ()
                           (progn
                            (core-form o
                                       (lambda ()
                                         (progn
                                          (core-apply-with-args o 'exactly
                                                                'action)
                                          (setq ac (core-apply o 'str)))))
                            (read-from-string ac)))
                         (lambda () (progn (core-apply o 'expression)))))))) 
 (defmethod expression ((o ometa-translator))
   (core-or o
            (lambda ()
              (core-or o (lambda () (progn (core-apply o 'apply-operation)))
                       (lambda ()
                         (progn (core-apply o 'apply-with-args-operation)))
                       (lambda ()
                         (progn (core-apply o 'apply-super-operation)))
                       (lambda () (progn (core-apply o 'seq-operation)))
                       (lambda () (progn (core-apply o 'many-operation)))
                       (lambda () (progn (core-apply o 'many1-operation)))
                       (lambda () (progn (core-apply o 'repeat-operation)))
                       (lambda () (progn (core-apply o 'not-operation)))
                       (lambda () (progn (core-apply o 'optional-operation)))
                       (lambda () (progn (core-apply o 'form-operation)))
                       (lambda () (progn (core-apply o 'symbol-operation)))
                       (lambda () (progn (core-apply o 'number-operation)))
                       (lambda () (progn (core-apply o 'predicate)))
                       (lambda ()
                         (progn (core-apply o 'lookahead-operation))))))) 
 (defmethod apply-operation ((o ometa-translator))
   (let ((s nil))
     (core-or o
              (lambda ()
                (core-or o
                         (lambda ()
                           (progn
                            (core-form o
                                       (lambda ()
                                         (progn
                                          (core-apply-with-args o 'exactly
                                                                'apply)
                                          (setq s (core-apply o 'an-atom)))))
                            `(core-apply o ',s)))))))) 
 (defmethod apply-super-operation ((o ometa-translator))
   (core-or o
            (lambda ()
              (core-or o
                       (lambda ()
                         (progn
                          (core-form o
                                     (lambda ()
                                       (progn
                                        (core-apply-with-args o 'exactly
                                                              'apply-super)
                                        (core-apply o 'an-atom))))
                          '(call-next-method o))))))) 
 (defmethod apply-with-args-operation ((o ometa-translator))
   (let ((a nil) (k nil) (r nil))
     (core-or o
              (lambda ()
                (core-or o
                         (lambda ()
                           (progn
                            (core-form o
                                       (lambda ()
                                         (progn
                                          (core-apply-with-args o 'exactly
                                                                'apply-with-args)
                                          (setq r (core-apply o 'an-atom))
                                          (core-form o
                                                     (lambda ()
                                                       (progn
                                                        (core-apply-with-args o
                                                                              'exactly
                                                                              'arguments)
                                                        (setq a
                                                                (core-many o
                                                                           (lambda
                                                                               ()
                                                                             (core-or
                                                                              o
                                                                              (lambda
                                                                                  ()
                                                                                (progn
                                                                                 (core-apply
                                                                                  o
                                                                                  'an-atom)))
                                                                              (lambda
                                                                                  ()
                                                                                (progn
                                                                                 (core-form
                                                                                  o
                                                                                  (lambda
                                                                                      ()
                                                                                    (progn
                                                                                     (core-apply-with-args
                                                                                      o
                                                                                      'exactly
                                                                                      'symbol)
                                                                                     (setq k
                                                                                             (core-apply
                                                                                              o
                                                                                              'an-atom))
                                                                                     `',k)))))))))))))))
                            `(core-apply-with-args o ',r ,@a)))))))) 
 (defmethod seq-operation ((o ometa-translator))
   (let ((s nil))
     (core-or o
              (lambda ()
                (core-or o
                         (lambda ()
                           (progn
                            (core-form o
                                       (lambda ()
                                         (progn
                                          (core-apply-with-args o 'exactly
                                                                'seq)
                                          (setq s (core-apply o 'str)))))
                            (if (eq (array-total-size s) 1)
                                `(core-apply-with-args o 'exactly ,(aref s 0))
                                `(core-apply-with-args o 'seq
                                                       ',(concatenate 'list
                                                                      s)))))))))) 
 (defmethod many-operation ((o ometa-translator))
   (let ((x nil))
     (core-or o
              (lambda ()
                (core-or o
                         (lambda ()
                           (progn
                            (core-form o
                                       (lambda ()
                                         (progn
                                          (core-apply-with-args o 'exactly
                                                                'many)
                                          (setq x (core-apply o 'choice)))))
                            (if (listp (car x))
                                `(core-many o (lambda ,nil ,@x))
                                `(core-many o (lambda ,nil ,x)))))))))) 
 (defmethod many1-operation ((o ometa-translator))
   (let ((x nil))
     (core-or o
              (lambda ()
                (core-or o
                         (lambda ()
                           (progn
                            (core-form o
                                       (lambda ()
                                         (progn
                                          (core-apply-with-args o 'exactly
                                                                'many1)
                                          (setq x (core-apply o 'choice)))))
                            (if (listp (car x))
                                `(core-many1 o (lambda ,nil ,@x))
                                `(core-many1 o (lambda ,nil ,x)))))))))) 
 (defmethod not-operation ((o ometa-translator))
   (let ((x nil))
     (core-or o
              (lambda ()
                (core-or o
                         (lambda ()
                           (progn
                            (core-form o
                                       (lambda ()
                                         (progn
                                          (core-apply-with-args o 'exactly
                                                                'not)
                                          (setq x (core-apply o 'choice)))))
                            (if (listp (car x))
                                `(core-not o (lambda ,nil ,@x))
                                `(core-not o (lambda ,nil ,x)))))))))) 
 (defmethod lookahead-operation ((o ometa-translator))
   (let ((x nil))
     (core-or o
              (lambda ()
                (core-or o
                         (lambda ()
                           (progn
                            (core-form o
                                       (lambda ()
                                         (progn
                                          (core-apply-with-args o 'exactly
                                                                'lookahead)
                                          (setq x (core-apply o 'choice)))))
                            (if (listp (car x))
                                `(core-lookahead o (lambda ,nil ,@x))
                                `(core-lookahead o (lambda ,nil ,x)))))))))) 
 (defmethod optional-operation ((o ometa-translator))
   (let ((x nil))
     (core-or o
              (lambda ()
                (core-or o
                         (lambda ()
                           (progn
                            (core-form o
                                       (lambda ()
                                         (progn
                                          (core-apply-with-args o 'exactly
                                                                'optional)
                                          (setq x (core-apply o 'choice)))))
                            (if (listp (car x))
                                `(core-opt o (lambda ,nil ,@x))
                                `(core-opt o (lambda ,nil ,x)))))))))) 
 (defmethod repeat-operation ((o ometa-translator))
   (let ((x nil) (n nil))
     (core-or o
              (lambda ()
                (core-or o
                         (lambda ()
                           (progn
                            (core-form o
                                       (lambda ()
                                         (progn
                                          (core-apply-with-args o 'exactly
                                                                'repeat)
                                          (setq n (core-apply o 'num))
                                          (setq x (core-apply o 'choice)))))
                            (if (listp (car x))
                                `(core-repeat o ,n (lambda ,nil ,@x))
                                `(core-repeat o ,n (lambda ,nil ,x)))))))))) 
 (defmethod form-operation ((o ometa-translator))
   (let ((x nil))
     (core-or o
              (lambda ()
                (core-or o
                         (lambda ()
                           (progn
                            (core-form o
                                       (lambda ()
                                         (progn
                                          (core-apply-with-args o 'exactly
                                                                'form)
                                          (setq x (core-apply o 'choice)))))
                            `(core-form o (lambda ,nil ,x))))))))) 
 (defmethod symbol-operation ((o ometa-translator))
   (let ((x nil))
     (core-or o
              (lambda ()
                (core-or o
                         (lambda ()
                           (progn
                            (core-form o
                                       (lambda ()
                                         (progn
                                          (core-apply-with-args o 'exactly
                                                                'symbol)
                                          (setq x (core-apply o 'an-atom)))))
                            `(core-apply-with-args o 'exactly ',x)))))))) 
 (defmethod number-operation ((o ometa-translator))
   (let ((x nil))
     (core-or o
              (lambda ()
                (core-or o
                         (lambda ()
                           (progn
                            (core-form o
                                       (lambda ()
                                         (progn
                                          (core-apply-with-args o 'exactly
                                                                'number)
                                          (setq x (core-apply o 'an-atom)))))
                            `(core-apply-with-args o 'exactly ,x)))))))) 
 (defmethod predicate ((o ometa-translator))
   (let ((s nil))
     (core-or o
              (lambda ()
                (core-or o
                         (lambda ()
                           (progn
                            (core-form o
                                       (lambda ()
                                         (progn
                                          (core-apply-with-args o 'exactly
                                                                'sem-predicate)
                                          (setq s (core-apply o 'str)))))
                            `(core-pred o ,(read-from-string s))))))))) 
 (defmethod an-atom ((o ometa-translator))
   (let ((a nil))
     (core-or o
              (lambda ()
                (core-or o
                         (lambda ()
                           (progn
                            (setq a (core-apply o 'anything))
                            (progn
                             (core-pred o (or (symbolp a) (numberp a)))
                             a)))))))) 
 