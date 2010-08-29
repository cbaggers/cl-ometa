ometa OMetaParser {
  space = '/*' { ~'*/' _ }* '*/'
        | ^
        ;

  ometa = spaces "ometa" identifier:name inheritance:i "{" rules:r "}" $
           => `(grammar ,name ,i
                  (locals ,@(send self 'locals-ast))
                   ,@r);


  inheritance = "<:" identifier:i => `(inherit-from ,i)
              |                     => `(inherit-from OMeta)
              ;

  rules = rule+;

  rule = &{rule-name:rname} <rule-part rname>+:p => `(rule ,rname (or ,@p));

  rule-part :rn = rule-name:rname %(equal? rname rn) rule-rest:r ";" => r;

  rule-rest = "::=" choices
            |  action
            |  argument+:args "::=" choices:c => `(and ,@args ,c)
            |  argument+:args  action:ac      => `(and ,@args ,ac)
            ;


  rule-name =  identifier:rname 
               => (begin (.set state-self 'current-rule rname) rname)
            ;


  argument = bind-expression
           | binding:b       => `(bind ,b (apply anything))
           ;

  choices  = choice:x { "|" choice:c => c}*:xs => `(or ,x ,@xs);

  choice   = top-expression*:x action:ac => `(and ,@x ,ac)
           | top-expression*:x           => `(and ,@x)
           ;

  top-expression =   bind-expression
                 |   repeated-expression
                 ;

  bind-expression = repeated-expression:e binding:b => `(bind ,b ,e)
                  ;

  repeated-expression = term:t "*" => `(many ,t)
                      |    term:t "+" => `(many1 ,t)
                      |    term:t "?" => `(optional ,t)
                      |    term
                      ;

  term  = '~'  element:e => `(not ,e)
        |  '&'  element:e => `(lookahead ,e)
        |  element
        ;

  binding = ':' identifier:i => (begin (send self 'add-local i) i)
          ;

  element = prod-app
          |  data-element
          |  '%' host-lang-expr:s => `(sem-predicate ,s)
          |  "{" choices:c "}" => c
          ;

  data-element = char-sequence
               |  char-sequence-s
               |  string-literal
               |  symbol
               |  s-expr
               |  any-symb
               |  end-symb
               ; 

  action     = "=>" host-lang-expr:s => `(action ,s);


  host-lang-expr   = host-lang-quote:q 
                     host-lang-expand:e 
                     host-lang-s-expr:s 
                     => (string-trim (string-append q e s));

  host-lang-quote   = {'`'+:q => (list->string q) 
                       | '\''+:q => (list->string q) 
                       | => (string)};

  host-lang-expand  = {','+:q => (list->string q) 
                       | => (string)}:qq  
                      {'@':a => (string a) 
                       | => (string)}:aa => (string-append qq aa);

  host-lang-s-expr  = host-lang-atom
                     |  "(" { host-lang-atom | host-lang-expr }*:x ")"
                           =>  (string-append "(" (strings-join x " ") ")")
                     ;

  host-lang-atom    = host-lang-quote:q host-lang-expand:e 
                      {   s-identifier 
                        | string-literal:l 
                          => (list->string `(#\" ,@l #\"))
                       }:a  => (string-append q e a);

  s-identifier = {~{space | ';' | '(' | ')' | '}' | '{' | '|' } 
                 char:c => c}+:xs spaces  => (list->string xs);


  prod-app = "<" identifier:p ">" 
                 => `(apply ,p)
            |  identifier:p
                 => `(apply ,p)
            ;

  prod-app = "<" identifier:p prod-arg-list:args ">" 
                 => `(apply-w-args ,p (arguments ,@args))
            ;

  prod-app = "^"
              => `(apply-super-w-args ,(.get self 'current-rule) (arguments))
            |  "<^" prod-arg-list:args ">"
              => `(apply-super-w-args ,(.get self 'current-rule) (arguments ,@args))
            ;

  prod-arg-list  = prod-arg:x {"," prod-arg:a => a}*:xs => (cons x xs);

  prod-arg       = data-element | identifier
                 ;


  char-sequence  = '\'' { '\\' '\'' | '\\' '\\' | ~'\'' char:c => c}+:cs "'" 
                      => `(seq ,(list->string cs));

  char-sequence-s = '"' { '\\' '"' 
                         | '\\' '\\' 
                         | ~'"'  char:c => c
                        }*:cs "\"" 
                          => `(and (seq ,(list->string cs))
                                   (apply spaces));

  string-literal = '``' {~'``' char:c => c}*:cs "''" 
                     => (exactly ,(list->string cs));

  symbol     =  '#' identifier:t => `(symbol ,t);

  s-expr     =  "(" choice:s ")" => `(form ,s);


  la-prefix      = "&";
  not-prefix     = "~";
  sem-prefix     = "%";
  end-symb       = "$" => `(apply end);
  any-symb       = "_" => `(apply anything);
}
