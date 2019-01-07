#lang racket
(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         parser-tools/yacc
         (prefix-in stx:"syntax.rkt")
         )
(provide (all-defined-out))

(define ... (void)) ;; indicates a part to be implemented
 
(define-tokens tokens-with-value
  (NUM ID))

(define-empty-tokens tokens-without-value
  (+ - * /
   < <= > >= == !=
   & && || =
   SEMI LPAR RPAR COMMA RETURN
   LBRA RBRA LBBRA RBBRA
   INT VOID
   IF ELSE WHILE FOR
   EOF))

(define-lex-trans uinteger
  (syntax-rules () ((_ d) (:* d))))

(define-lex-abbrevs
  (digit            (char-range "0" "9"))
  (digit-non-zero   (char-range "1" "9"))
  (number  (:or "0"
                (:: digit-non-zero
                    (uinteger digit))))
  (identifier-char (:or (char-range "a" "z")
                        (char-range "A" "Z")))
  (identifier (:: identifier-char
                  (:* (:or identifier-char digit "_")))))
 
(define small-c-lexer
  (lexer-src-pos
   ("+"        (token-+))
   ("-"        (token--))
   ("*"        (token-*))
   ("/"        (token-/))
   ("<"        (token-<))
   ("<="       (token-<=))
   (">"        (token->))
   (">="       (token->=))
   ("=="       (token-==))
   ("!="       (token-!=))
   ("&"        (token-&))
   ("&&"       (token-&&))
   ("||"       (token-||))
   ("="        (token-=))
   (";"        (token-SEMI))
   ("("        (token-LPAR))
   (")"        (token-RPAR))
   ("{"        (token-LBRA))
   ("}"        (token-RBRA))
   ("["        (token-LBBRA))
   ("]"        (token-RBBRA))
   (","        (token-COMMA))
   ("return"   (token-RETURN))
   ("if"       (token-IF))
   ("else"     (token-ELSE))
   ("while"    (token-WHILE))
   ("for"      (token-FOR))
   ("int"      (token-INT))
   ("void"     (token-VOID))
   (number     (token-NUM (string->number lexeme)))
   (identifier (token-ID (string->symbol lexeme)))
   (whitespace (return-without-pos (small-c-lexer input-port)))
   ((eof)      (token-EOF))))





(define (ints type declist)
  (if (list? declist)
      (if (eq? 1 (length declist))
          (stx:int-stmt type (car declist))
          (cons (stx:int-stmt type (car declist))
                (ints type (cdr declist))))
      (stx:int-stmt type declist)))







(define small-c-parser
  (parser
   (start program)
   (end EOF)
   (src-pos)
  ;; (debug "small-c-parser.tbl")
   (suppress)
   (error (lambda (tok-ok? tok-name tok-value start-pos end-pos)
            (eprintf (format "parse error:~a,~a: ~a is invalid\n"
                           (position-line start-pos)
                           (position-col start-pos)
                           (if tok-value tok-value tok-name)))))
   (tokens tokens-with-value tokens-without-value)
   (grammar
    (program
     ((addprint) (cons (stx:funpro-stmt
                        (stx:type-exp 'void 'print-proto)
                        (stx:fundec-stmt 'print
                                   (stx:paradec-stmt
                                     (stx:type-exp 'int 'print-proto)
                                     (stx:var-exp 'v 'print-proto))
                                         'print-proto))
                       $1)))
    (addprint
     ((external-declaration) $1)
     ((addprint external-declaration) (cons $1 $2)))
    (external-declaration
     ((declaration)  $1)
     ((function-prototype) $1)
     ((function-definition) $1))
    (declaration
     ((type-specifier declarator-list SEMI)
      ;(stx:int-stmt $1 $2)
      (ints $1 $2)
                                            ))
    (declarator-list
     ((declarator)  $1)
     ((declarator-list COMMA declarator)
      ;(cons $1 $3)
      (append (if (list? $1) $1 (list $1)) (list $3))
                                         ))
    (declarator
     ((direct-declarator)  (stx:decl-stmt $1))
     ((* direct-declarator) (stx:declpoint-stmt $2)))
    (direct-declarator
     ((ID) (stx:var-exp $1 $1-start-pos))
     ((ID LBBRA NUM RBBRA) (stx:arr-exp $1 $3 $1-start-pos)))
    (function-prototype
     ((type-specifier function-declarator SEMI)
                         (stx:funpro-stmt $1 $2)))
    (function-declarator
     ((ID LPAR parameter-type-list-opt RPAR)
          (stx:fundec-stmt $1 $3 $1-start-pos))
     ((* ID LPAR parameter-type-list-opt RPAR)
      (stx:fundecpoint-stmt
       (stx:varpoint-exp $2 $2-start-pos)
       $4 $2-start-pos)))
    (function-definition  
     ((type-specifier function-declarator compound-statement)
                   (stx:fundef-stmt $1 $2 $3)))
    (parameter-type-list-opt
     (() '())
     ((parameter-type-list) $1))
    (parameter-type-list
     ((parameter-declaration)  $1)
     ((parameter-type-list COMMA parameter-declaration) (cons $1 $3)))
    (parameter-declaration
     ((type-specifier parameter-declarator) (stx:paradec-stmt $1 $2)))
    (parameter-declarator
     ((ID) (stx:var-exp $1 $1-start-pos))
     ((* ID) (stx:varpoint-exp $2 $1-start-pos)))
    (type-specifier
     ((INT) (stx:type-exp 'int $1-start-pos))
     ((VOID) (stx:type-exp 'void $1-start-pos)))
    (statement
     ;((SEMI) '())
     ((expression-opt SEMI) (stx:exp-stmt $1 $1-start-pos))
     ((compound-statement) $1)     
     ((IF LPAR expression RPAR statement ELSE statement)
                          (stx:if-stmt $3 $5 $7 $1-start-pos))
     ((IF LPAR expression RPAR statement)
      (stx:if-stmt $3 $5 (stx:cmpd-stmt '() '()) $1-start-pos))
     ((WHILE LPAR expression RPAR statement)
      (stx:while-stmt $3 $5 $1-start-pos))
     ((FOR LPAR expression-opt SEMI expression-opt
           SEMI expression-opt RPAR statement)
      (stx:cmpd-stmt '() (cons $3 (stx:while-stmt $5
                  (stx:cmpd-stmt '() (cons $9 $7)) $5-start-pos))))
     ((RETURN expression-opt SEMI) (stx:ret-stmt $2 $1-start-pos)))
    (compound-statement
     ((LBRA declaration-list-opt statement-list-opt RBRA)
      (stx:cmpd-stmt $2 $3)))
    (declaration-list-opt
     (() '())
     ((declaration-list) $1))
    (declaration-list
     ((declaration)  $1)
     ((declaration-list declaration) (cons $1 $2)))
    (statement-list-opt
     (() '())
     ((statement-list) $1))
    (statement-list
     ((statement) $1)
     ((statement-list statement) (cons $1 $2)))
    (expression-opt
     (() '())
     ((expression)  $1))
    (expression
     ((assign-expr)  $1)
     ((expression COMMA assign-expr) (cons $1 $3)))
    (assign-expr
     ((logical-or-expr)  $1)
     ((logical-or-expr = assign-expr)
      (stx:assign-stmt $1 $3 $1-start-pos)))
    (logical-or-expr
     ((logical-and-expr)  $1)
     ((logical-or-expr || logical-and-expr)
      (stx:lop-exp '|| $1 $3 $2-start-pos)))
    (logical-and-expr
     ((equality-expr) $1)
     ((logical-and-expr && equality-expr)
      (stx:lop-exp '&& $1 $3 $2-start-pos)))
    (equality-expr
     ((relational-expr)  $1)
     ((equality-expr == relational-expr)
      (stx:rop-exp '== $1 $3 $2-start-pos))
     ((equality-expr != relational-expr)
      (stx:rop-exp '!= $1 $3 $2-start-pos)))
    (relational-expr
     ((add-expr) $1)
     ((relational-expr < add-expr)
      (stx:rop-exp '< $1 $3 $2-start-pos))
     ((relational-expr > add-expr)
      (stx:rop-exp '> $1 $3 $2-start-pos))
     ((relational-expr <= add-expr)
      (stx:rop-exp '<= $1 $3 $2-start-pos))
     ((relational-expr >= add-expr)
      (stx:rop-exp '>= $1 $3 $2-start-pos)))
    (add-expr
     ((mult-expr) $1)
     ((add-expr + mult-expr) (stx:aop-exp '+ $1 $3 $2-start-pos))
     ((add-expr - mult-expr) (stx:aop-exp '- $1 $3 $2-start-pos)))
    (mult-expr
     ((unary-expr)  $1)
     ((mult-expr * unary-expr) (stx:aop-exp '* $1 $3 $2-start-pos))
     ((mult-expr / unary-expr) (stx:aop-exp '/ $1 $3 $2-start-pos)))
    (unary-expr
     ((postfix-expr) $1)
     ((- unary-expr) (stx:aop-exp '- (stx:lit-exp 0 'syntaxsugar)
                                  $2 $1-start-pos))
     ;;((- unary-expr) (stx:neg-exp $2 $1-start-pos))
     ((& unary-expr)
      (if (stx:expkakko-stmt? $2)
          (if (stx:unary-stmt? (stx:expkakko-stmt-stmt $2))
              (if (equal? '* (stx:unary-stmt-mark
                              (stx:expkakko-stmt $2)))
                  (stx:unary-stmt-stmt (stx:expkakko-stmt-stmt $2))
                  (stx:unary-stmt '& $2 $1-start-pos))                  
                  (stx:unary-stmt '& $2 $1-start-pos))
          (if (stx:unary-stmt? $2)
               (if (equal? '* (stx:unary-stmt-mark $2))
                  (stx:unary-stmt-stmt $2)
                  (stx:unary-stmt '& $2 $1-start-pos))                  
                  (stx:unary-stmt '& $2 $1-start-pos))))
;;     ((& unary-expr) (stx:addr-exp $2 $1-start-pos))
     ((* unary-expr) (stx:deref-exp $2 $1-start-pos)))
    (postfix-expr
     ((primary-expr) $1)
     ((postfix-expr LBBRA expression RBBRA)
      (stx:unary-stmt '* (stx:expkakko-stmt
                          (stx:aop-exp '+ $1 $3 $1-start-pos)
                          $1-start-pos) $1-start-pos))
     ;;     ((postfix-expr LBBRA expression RBBRA)
     ;;(stx:arrvar-stmt $1 $3 $1-start-pos))
     ((ID LPAR argument-expression-list-opt RPAR)
                            (stx:kakko-exp $1 $3 $1-start-pos)))  
    (primary-expr
     ((ID) (stx:var-exp $1 $1-start-pos))
     ((NUM) (stx:lit-exp $1 $1-start-pos))
     ((LPAR expression RPAR) (stx:expkakko-stmt $2 $2-start-pos)))
    (argument-expression-list-opt
     (() '())
     ((argument-expression-list) $1))
    (argument-expression-list
     ((assign-expr) $1)
     ((argument-expression-list COMMA assign-expr) (cons $1 $3))))))

(define (parse-port port)
  (port-count-lines! port)
  (small-c-parser (lambda () (small-c-lexer port))))

;; 文字列を受け取って構文解析
(define (parse-string str)
  (parse-port (open-input-string str)))

;; ファイルを受け取って構文解析
(define (parse-file fname)
  (parse-port (open-input-file fname)))

;; 抽象構文木(実は任意のRacketデータ)を見やすく表示
(define (pretty-print-ast ast)
  (pretty-print ast))

(define a "

int main(){
int x,*y,z;
x=8;
if(x>5){
*y=z;
}
}")

(define b "

void main(){

int x;
int i;

x=29;

for(i=0;i<7;i=i+1){

x=x-1;

}
print(x);
}
")

(define c "

void main(){

int x[5];

x[5]=3;

print(x[5]);

}")

(define d "
int *f(int *a){

return a;

}

int main(){

return 0;
}")

;;test

(define ax (parse-string a))
(define bx (parse-string b))
(define cx (parse-string c))
(define dx (parse-string d))

