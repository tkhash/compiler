#lang racket
(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         parser-tools/yacc)
(require "environment.rkt")
(require (prefix-in stx: "syntax.rkt"))
(require (prefix-in psr: "parser.rkt"))
(require (prefix-in chu: "chukan.rkt"))

(provide (all-defined-out))

(define wordsize 4)

(define sp 0)

(define temp-stack '())

(struct fun-stack (fun vars) #:transparent)

(define stack '())


(struct obj-off (name lev kind type pos off) #:transparent)
(define (convert-obj ob off)
  (let* ((name (obj-name ob))
         (lev (obj-lev ob))
         (kind (obj-kind ob))
         (type (obj-type ob))
         (pos (obj-pos ob))
         (new-obj-off (obj-off name lev kind type pos off)))
  (set! temp-stack (flatten (append temp-stack (list new-obj-off))))
    new-obj-off))


(define (convert-iter i j vardecl-list)
  (cond ((null? vardecl-list)  '())
        (else 
         (let* ((ob (chu:var-decl-var (car vardecl-list)))
                (flag (cond ((type_array? (obj-type ob)) 1)
                            (else 0)))
                (car-convert (cond ((equal? 1 flag) 
                                    (list (chu:var-decl 
                                           (convert-obj 
                                            ob 
                                            (+ i 
                                               (* j
                                                  (- (type_array-size (obj-type ob)) 
                                                     1)))))))
                                   ((equal? 0 flag)
                                    (list (chu:var-decl (convert-obj ob i)))))))
           (cond ((equal? 0 flag) (set! sp (+ j i)))
                 ((equal? 1 flag) (set! sp (+ j (+ i 
                                                   (* j
                                                      (- (type_array-size (obj-type ob)) 
                                                         1)))))))
           (flatten 
            (append 
             car-convert
             (list (convert-iter sp j (cdr vardecl-list)))))))))


(define (sub-assign-add-intermed st)
  (cond ((chu:fun-def? st)
         (let* ((meaningless (set! temp-stack '()))
                (def-obj (chu:fun-def-var st))
                (vardecl-list (chu:fun-def-parms st))
                (meaningless (set! sp 0))
                (convert-vardecl (convert-iter wordsize wordsize vardecl-list))
                (meaningless (set! sp 0))
                (body (chu:fun-def-body st))
                (assigned-body (assign-add-cmpd body sp))
                (meaningless 
                (set! stack (flatten (append stack (list (fun-stack def-obj temp-stack)))))))
           (chu:fun-def def-obj 
                      convert-vardecl
                      assigned-body)))
        (else st)))

(define (assign-add-intermed in)
  (map sub-assign-add-intermed in))


(define (assign-add-cmpd st i)
  (cond ((chu:cmpd-stmt? st)
         (let* ((decls (chu:cmpd-stmt-decls st))
                (stmts (chu:cmpd-stmt-stmts st)))
           (chu:cmpd-stmt 
            (convert-iter i (- 0 wordsize) decls) 
            (map (lambda (x) (assign-add-cmpd x sp)) stmts))))
        ((chu:if-stmt? st)
         (let* ((var (chu:if-stmt-var st))
                (tlabel (chu:if-stmt-tlabel st))
                (elabel (chu:if-stmt-elabel st)))
           (chu:if-stmt var 
                      (assign-add-cmpd tlabel sp) 
                      (assign-add-cmpd elabel sp))))
        ((chu:while-stmt? st)
          (let* ((var (chu:while-stmt-var st))
                (stmt (chu:while-stmt-stmt st)))
           (chu:while-stmt var (assign-add-cmpd stmt sp))))
        (else st)
        ))


(define (ref-obj ob fun)
  (let* ((name (obj-name ob))
         (lev (obj-lev ob))
         (kind (obj-kind ob))
         (type (obj-type ob))
         (pos (obj-pos ob)))
    (cond 
      ((equal? 0 lev) ob)
      (else
       (let* ((l 
               (flatten 
                (list 
                 (filter
                  (lambda (x)
                    (and (equal? name (obj-off-name x))
                         (equal? lev (obj-off-lev x))
                         (equal? kind (obj-off-kind x))
                         (equal? type (obj-off-type x))
                         (equal? pos (obj-off-pos x))))
                  (fun-stack-vars　 
                   (car (filter (lambda (x) (equal? fun (fun-stack-fun x))) stack)
                        )))))))
         (cond ((null? l) ob)
               (else (car l)))))
      )))

(define (ref-add i fun)
  (cond ((obj? i) (ref-obj i fun))
        ((chu:cmpd-stmt? i)
         (let* ((decls (chu:cmpd-stmt-decls i))
                (stmts (chu:cmpd-stmt-stmts i)))
           (chu:cmpd-stmt decls 
                         (map (lambda (x) (ref-add x fun)) stmts))))
        ((chu:assign-stmt? i) 
         (let* ((var (chu:assign-stmt-var i))
                (exp (chu:assign-stmt-exp i)))
           (chu:assign-stmt (ref-add var fun) (ref-add exp fun))))
        ((chu:write-stmt? i)
         (let* ((dest (chu:write-stmt-dest i))
                (src (chu:write-stmt-src i)))
           (chu:write-stmt (ref-add dest fun) (ref-add src fun))))
        ((chu:read-stmt? i)
         (let* ((dest (chu:read-stmt-dest i))
                (src (chu:read-stmt-src i)))
           (chu:read-stmt (ref-add dest fun) (ref-add src fun))))
        ((chu:if-stmt? i)
         (let* ((var (chu:if-stmt-var i))
                (tlabel (chu:if-stmt-tlabel i))
                (elabel (chu:if-stmt-elabel i)))
           (chu:if-stmt (ref-add var fun) (ref-add tlabel fun) (ref-add elabel fun))))
        ((chu:while-stmt? i)
         (let* ((var (chu:while-stmt-var i))
                (stmt (chu:while-stmt-stmt i)))
           (chu:while-stmt (ref-add var fun) (ref-add stmt fun))))
        ((chu:call-stmt? i)
         (let* ((dest (chu:call-stmt-dest i))
                 (tgt (chu:call-stmt-tgt i))
                 (vars (chu:call-stmt-vars i)))
           (chu:call-stmt (ref-add dest fun) tgt
                          (map (lambda (x) (ref-add x fun)) vars))))
        ((chu:ret-stmt? i)
         (let* ((var (chu:ret-stmt-var i)))
           (chu:ret-stmt (ref-add var fun))))
        ((chu:print-stmt? i)
         (let* ((var (chu:print-stmt-var i)))
           (chu:print-stmt (ref-add var fun))))
        ((chu:label-stmt? i) i)
        ((chu:goto-stmt? i) i)
        ((chu:var-exp? i)
         (let* ((var (chu:var-exp-var i)))
           (chu:var-exp (ref-add var fun)))) 
        ((chu:lit-exp? i) i)
        ((chu:aop-exp? i)
         (let* ((op (chu:aop-exp-op i))
                (left (chu:aop-exp-left i))
                (right (chu:aop-exp-right i)))
           (chu:aop-exp op (ref-add left fun) (ref-add right fun))
           ))
        ((chu:rop-exp? i)
         (let* ((op (chu:rop-exp-op i))
                (left (chu:rop-exp-left i))
                (right (chu:rop-exp-right i)))
           (chu:rop-exp op (ref-add left fun) (ref-add right fun))))
        ((chu:addr-exp? i)
         (let* ((var (chu:addr-exp-var i)))
           (chu:addr-exp (ref-add var fun))))
        ((equal? '() i)
         (chu:null-stmt))
        ((chu:array_base_add? i)
         (find-base i fun))
        (else (error (format "\ncheck ~a in ref-add\n" i)))
        ))

(define (find-base i fun)
  (let* ((name (obj-name (chu:array_base_add-ar i)))
         (lev (obj-lev (chu:array_base_add-ar i)))
         (kind (obj-kind (chu:array_base_add-ar i)))
         (type (obj-type (chu:array_base_add-ar i))))
    (chu:lit-exp
     (obj-off-off
      (car (flatten 
            (list 
             (filter (lambda (x)
                       (and (equal? name (obj-off-name x))
                            (equal? lev (obj-off-lev x))
                            (equal? kind (obj-off-kind x))
                            (equal? type (obj-off-type x))))
                     (fun-stack-vars 
                      (car (filter (lambda (x) (equal? fun (fun-stack-fun x))) stack)
                           ))))))))))

(define (ref-add-intermed i)
  (map (lambda (x) 
         (cond ((chu:fun-def? x) 
                (let* ((var (chu:fun-def-var x))
                       (parms (chu:fun-def-parms x))
                       (body (chu:fun-def-body x)))
                  (chu:fun-def var parms (ref-add body var))))
               (else x)))
       i))

(struct itmd-and-stack (it st) #:transparent)

(define (gen-assigned-itmd i)
  (let* ((output (itmd-and-stack (ref-add-intermed i) stack))
         (meanigless (set! stack '())))
  output))

(define (addrmain fn) (gen-assigned-itmd (assign-add-intermed (chu:main fn))))

;;test
        
(define av (gen-assigned-itmd (assign-add-intermed chu:aw)))
(define bv (gen-assigned-itmd (assign-add-intermed chu:bw)))
(define cv (gen-assigned-itmd (assign-add-intermed chu:cw)))
(define dv (gen-assigned-itmd (assign-add-intermed chu:dw)))
