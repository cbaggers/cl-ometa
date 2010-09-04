ometa ometa-translator <: ometa-base {

  _slots ((grammar-name :accessor grammar-name
                 :initform nil)
          (local-variables  :accessor ometa-local-variables));

  ometa = #grammar {an-atom:name => (setf (grammar-name o) name)}
          inheritance:i locals slots:sl inline-code:ic
          rules:r 
        => (if inline-code
            `((defclass ,name (,i) ,sl) ,@ic ,@r)
            `((defclass ,name (,i) ,sl) ,@r));

  inheritance = (#parent an-atom);

  locals = (#locals ( { (an-atom:rname (an-atom+):vars )* => `(,rname ,@vars) }:lst ))
            => (setf (ometa-local-variables o) (list->hash-table lst));

  slots = (#slots { str:s => (read-from-string s) | => nil });

  inline-code = (#inline { str:s => (read-all-from-string s) | => nil});

  rules = rule+;

  rule = (#rule an-atom:rname choice:p) 
          => (let ((locals (gethash rname (ometa-local-variables o))))
              `(defmethod ,rname ((o ,(grammar-name o)))
                ,(if (null locals) p `(let ,(map 'cons (lambda (x) (list x nil)) locals) ,p))));

  choice = (#and choice+:p) => `(progn ,@p)
         | (#and) => ''nil
         | (#or choice+:p) => `(core-or o ,@(map 'list (lambda (x) `(lambda () ,x)) p))
         | pattern
         ;

  pattern = (#bind an-atom:a choice:e) => `(seq ,a ,e)
          | (#action str:ac)           => (read-from-string ac)
          | expression
          ;

  expression = apply-operation
             | apply-with-args-operation
             | apply-super-operation
             | seq-operation
             | many-operation
             | many1-operation
             | not-operation
             | optional-operation
             | form-operation
             | symbol-operation
             | predicate
             | lookahead-operation
             ;

  apply-operation = (#apply an-atom:s) => `(core-apply o ',s);
  
  apply-super-operation = (#apply-super an-atom) => `(call-next-method o);

  apply-with-args-operation = (#apply-with-args an-atom:r (#arguments {an:atom|(#symbol an-atom:k => `',k)}*:a))
                               => `(core-apply-with-args o ',r ,@a);

  seq-operation = (#seq str:s) => (if (eq (array-total-size s) 1)
                                      `(core-apply-with-args o 'exactly ,(aref s 0))
                                      `(core-apply-with-args o 'seq ',(concatenate 'list s)));


  many-operation = (#many choice:x) => (if (listp (car x))
                                           `(core-many o (lambda () ,@x))
                                           `(core-many o (lambda () ,x)));

  many1-operation = (#many1 choice:x) => (if (listp (car x))
                                             `(core-many1 o (lambda () ,@x))
                                             `(core-many1 o (lambda () ,x)));

  not-operation = (#not choice:x) => (if (listp (car x))
                                         `(core-not o (lambda () ,@x))
                                         `(core-not o (lambda () ,x)));

  lookahead-operation = (#lookahead choice:x) => (if (listp (car x))
                                                     `(core-lookahead o (lambda () ,@x))
                                                     `(core-lookahead o (lambda () ,x)));

  optional-operation = (#optional choice:x) => (if (listp (car x))
                                                     `(core-opt o (lambda () ,@x))
                                                     `(core-opt o (lambda () ,x)));

  form-operation = (#form choice:x) => `(core-form o (lambda () ,x));

  symbol-operation = (#symbol an-atom:x) => `(core-apply-with-args o 'exactly ,x);

  predicate = (#sem-predicate str:s) => `(core-pred o ,(read-from-string s));

  an-atom = _:a => (progn (core-pred o (symbolp a)) a);
}

