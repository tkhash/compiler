#lang racket
(require "environment.rkt")
(require (prefix-in stx: "syntax.rkt"))
(require (prefix-in psr: "parser.rkt"))
(provide (all-defined-out))

;(define current-lev 0)
;(define comp-lev 0)
(define env '())
(define para-env '())
(define comp-env '())

;consセルにmapを適用させる
(define (mapcons lam li)
  (cond ((cons? li) (append (mapcons lam (car li))
                              (mapcons lam (cdr li))))
        (else (cons (lam li) '()))))

(define (analy-decl st lev)
  ;;;;
  ;内部定義
  ;(stx:declarator_st...)と
  ;levと  
  ;'intもしくは'void
  ;を引数にとり
  ;obj
  ;を返す関数.
  (define (make-obj-from-decl decl type lev)
    (let* ((var (cond ((stx:decl-stmt? decl) 
                      (stx:decl-stmt-var decl))
                     ((stx:declpoint-stmt? decl)
                      (stx:declpoint-stmt-var decl))))
           (name (cond ((stx:var-exp? var) (stx:var-exp-tgt var))
                       ((stx:arr-exp? var) (stx:arr-exp-tgt var))))
           (flag (cond ((stx:decl-stmt? decl) 'nomal)
                       ((stx:declpoint-stmt? decl) 'pointer)))
           (kind 'var)
           (type (cond ((stx:arr-exp? var) (type_array type (stx:arr-exp-num var)))
                       (else (cond ((equal? flag 'nomal) type)
                                   ((equal? flag 'pointer) (type_pointer 'pointer type))))))
           (pos (cond ((stx:var-exp? var) 
                       (stx:var-exp-pos var))
                      ((stx:varpoint-exp? var)
                       (stx:varpoint-exp-pos var))
                      ((stx:arr-exp? var)
                       (stx:arr-exp-pos var)))))
      (obj name lev kind type pos)))
  (let* ((type (stx:int-stmt-type st))
         (declarator-list (stx:int-stmt-declist st))
         ;make obj-list
         (obj-list (mapcons 
                    (lambda (x) (make-obj-from-decl x (stx:type-exp-op type) lev))
                    declarator-list)))
    ;check error
    (map (lambda (x) (check-decl x env)) obj-list)
    (map (lambda (x) (check-decl x para-env)) obj-list)
    
    ;add hensuu
    (set! env (add-list obj-list env))
   ;make struct
    (stx:int-stmt type obj-list)))


(define (analy-funpro st)
  ;;inter definition 
  (define (make-obj-from-paralist para-list)
    (mapcons (lambda (para-decl) 
            (let* ((type (stx:type-exp-op (stx:paradec-stmt-type para-decl)))
                   (var (stx:paradec-stmt-para para-decl))
                   (flag (cond ((stx:var-exp? var) 'normal)
                               ((stx:varpoint-exp? var) 'pointer)))
                   (name (cond ((equal? flag 'normal) (stx:var-exp-tgt var))
                               ((equal? flag 'pointer)(stx:varpoint-exp-tgt var))))
                   (pos (cond ((stx:var-exp? var) (stx:var-exp-pos var))
                              ((stx:varpoint-exp? var) (stx:varpoint-exp-pos var))))
                   (lev 1)
                   (kind 'parm)            
                   (type (cond ((equal? flag 'normal) type)
                               ((equal? flag 'pointer)(type_pointer 'pointer type)))))
              (obj name lev kind type pos)))
          para-list))
  ;;end interdefinition             
  (let* ((spec (stx:funpro-stmt-val st))
         (decl (stx:funpro-stmt-tgt st))
         (flag (cond ((stx:fundec-stmt? decl)
                      (if (null? (stx:fundec-stmt-tgt decl))
                          (para_flag 'nomal 'none)
                          (para_flag 'normal 'normal)))
                     ((stx:fundecpoint-stmt? decl)
                     (if (null? (stx:fundecpoint-stmt-tgt decl))
                         (para_flag 'pointer 'none)
                         (para_flag 'pointer 'normal)))))
         (proto-name (cond ((stx:fundec-stmt? decl) 
                            (stx:fundec-stmt-val decl))
                           ((stx:fundecpoint-stmt? decl) 
                            (stx:fundecpoint-stmt-val decl))))
         (proto-pos (cond ((stx:fundec-stmt? decl) 
                           (stx:fundec-stmt-pos decl))
                          ((stx:fundecpoint-stmt? decl) 
                           (stx:fundecpoint-stmt-pos decl))))
         (para-list (cond ((stx:fundec-stmt? decl)
                           (if (null? (stx:fundec-stmt-tgt decl)) 'nopara (stx:fundec-stmt-tgt decl)))
                          ((stx:fundecpoint-stmt? decl) 
                           (if (null? (stx:fundecpoint-stmt-tgt decl)) 'nopara (stx:fundecpoint-stmt-tgt decl)))))
         (para-obj-list (cond ((equal? para-list 'nopara) 'nopara)
                              (else (make-obj-from-paralist para-list))))
         (proto-type (cond ((equal? 'normal (para_flag-out-type flag))
                            (type_fun 'fun 
                                      (stx:type-exp-op spec) 
                                      (cond ((equal? 'nopara para-obj-list)
                                             'nopara)
                                            (else (map (lambda (x) (obj-type x)) 
                                                       para-obj-list)))))
                           ((equal? 'pointer (para_flag-out-type flag))
                            (type_fun 'fun
                                      (type_pointer 'pointer (stx:int-stmt-type spec))
                                      (cond ((equal? 'nopara para-obj-list)
                                             'nopara)
                                            (else (map (lambda (x) (obj-type x)) 
                                                       para-obj-list)))))))
         (proto-obj (obj proto-name 0 'proto proto-type proto-pos)))
    ;check error
    (check-proto proto-obj env)
    ;add hensuu
    (set! env (extend-env proto-obj env))
    ;make struct
    (check-proto-para para-obj-list)
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    (stx:funpro-stmt spec (stx:fundec-stmt proto-obj para-obj-list proto-pos))))

(define (analy-fundef st)
  ;;inter definition 
  (define (make-obj-from-paralist para-list)
    (mapcons (lambda (para-decl) 
            (let* ((type (stx:type-exp-op (stx:paradec-stmt-type para-decl)))
                   (id (stx:paradec-stmt-para para-decl))
                   (flag (cond ((stx:var-exp? id) 'normal)
                               ((stx:varpoint-exp? id) 'pointer)))
                   (name (cond ((equal? flag 'normal) (stx:var-exp-tgt id))
                               ((equal? flag 'pointer)(stx:varpoint-exp-tgt id))))
                   (pos (cond ((stx:var-exp? id) (stx:var-exp-pos id))
                              ((stx:varpoint-exp? id) (stx:varpoint-exp-pos id))))
                   (lev 1)
                   (kind 'parm)            
                   (type (cond ((equal? flag 'normal) type)
                               ((equal? flag 'pointer)(type_pointer 'pointer type)))))
              (obj name lev kind type pos)))
          para-list))
  ;;end interdefinition               
  (let* ((spec (stx:fundef-stmt-val st))
         (decl (stx:fundef-stmt-tgt st))
         (compo (stx:fundef-stmt-body st))
         (flag (cond ((stx:fundec-stmt? decl)
                      (if (null? (stx:fundec-stmt-tgt decl))
                          (fundef_flag 'normal 'none)
                          (fundef_flag 'normal 'normal)))
                     ((stx:fundecpoint-stmt? decl)
                      (if (null? (stx:fundecpoint-stmt-tgt decl))
                          (fundef_flag 'pointer 'none)
                          (fundef_flag 'pointer 'normal)))))
         (fundef-name (cond ((stx:fundec-stmt? decl) 
                             (stx:fundec-stmt-val decl))
                            ((stx:fundecpoint-stmt? decl) 
                             (stx:fundecpoint-stmt-val decl))))        
         (fundef-pos (cond ((stx:fundec-stmt? decl) 
                            (stx:fundec-stmt-pos decl))
                           ((stx:fundecpoint-stmt? decl) 
                            (stx:fundecpoint-stmt-pos decl))))
          (para-list (cond ((stx:fundec-stmt? decl)
                           (if (null? (stx:fundec-stmt-tgt decl)) 'nopara (stx:fundec-stmt-tgt decl)))
                          ((stx:fundecpoint-stmt? decl) 
                           (if (null? (stx:fundecpoint-stmt-tgt decl)) 'nopara (stx:fundecpoint-stmt-tgt decl)))))
         (para-obj-list (cond ((equal? para-list 'nopara) 'nopara)
                              (else (make-obj-from-paralist para-list))))
         (fundef-type (cond ((equal? 'normal (fundef_flag-out-type flag))
                             (type_fun 'fun
                                       (stx:type-exp-op spec)
                                       (cond ((equal? 'nopara para-obj-list) 'nopara)
                                             (else(map (lambda (x) (obj-type x))
                                                       para-obj-list)))))
                            ((equal? 'pointer (fundef_flag-out-type flag))
                             (type_fun 'fun
                                       (type_pointer 'pointer (stx:type-exp-op spec))
                                       (cond ((equal? 'nopara para-obj-list) 'nopara)
                                             (else (map (lambda (x) (obj-type x)) 
                                                        para-obj-list)))))
                            (else (begin (eprintf "INVALID FUNCTION")
                                         (error (format "INVALID FUNCTION"))))))
         (fundef-obj (obj fundef-name 0 'fun fundef-type fundef-pos)))
    
    (check-func fundef-obj env)
    
    (set! env (extend-env fundef-obj env))
    
    (check-def-para para-obj-list)
    
    (set! para-env para-obj-list)
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
    (stx:fundef-stmt spec  
                     (stx:fundec-stmt fundef-obj para-obj-list fundef-pos)
                     (analy-cmpd compo 1 env fundef-obj)
                     )))

(define (analy-cmpd st lev outer-env func-tag)
  (let* ((flag (cond ((stx:cmpd-stmt? st)                      
                      (cond ((and (null? (stx:cmpd-stmt-decs st))
                                  (null? (stx:cmpd-stmt-stmts st))) (comp_flag 'nodecl 'nostmt))
                            ((null? (stx:cmpd-stmt-decs st)) (comp_flag 'nodecl 'normal))
                            ((null? (stx:cmpd-stmt-stmts st)) (comp_flag 'normal 'nostmt))
                            (else (comp_flag 'normal 'normal))))
                     ((and (stx:exp-stmt? st) (null? (stx:exp-stmt-stmt st))) (comp_flag 'nodecl 'nostmt))
                     (else (begin (eprintf (format "ERROR! UNEXPECTED STRUCTURE! ~a " st))
                                  (error (format "ERROR! UNEXPECTED STRUCTURE! ~a " st))))))
         (decl-list (cond ((equal? 'normal (comp_flag-decl flag))
                           (stx:cmpd-stmt-decs st))
                          (else 'nodecl)))
         (stat-list (cond ((equal? 'normal (comp_flag-stmt flag)) 
                           (stx:cmpd-stmt-stmts st))
                          (else 'nostmt)))
         ;start level1up 
         (this-lev (+ lev 1))
         (decl-list (cond ((equal? 'normal (comp_flag-decl flag)) 
                           (mapcons (lambda (x) (analy-compdecl x this-lev)) decl-list))                  
                          ((equal? 'nodecl (comp_flag-decl flag))
                           'nodecl)))
         (comp-env 
          (cond 
            ((equal? 'nodecl decl-list) 'nodecl)
            (else (flatten (map (lambda (x) (stx:int-stmt-declist x)) decl-list)))))
         (comp-env-check (check-comp-env comp-env))
         (comp-env-check (cond ((equal? 'nodecl comp-env) #t) 
                               (else (map (lambda (x) (check-decl x outer-env)) comp-env))))
         (comp-env-check (cond ((equal? 'nodecl comp-env) #t)
                               (else (map (lambda (x) (check-decl x para-env)) comp-env))))
         (new-comp-env (cond ((equal? 'nodecl comp-env) outer-env)
                             (else (append comp-env outer-env))))
         (stat-list (cond ((equal? 'normal (comp_flag-stmt flag)) 
                           (mapcons 
                            (lambda (x) (analy-compstate x this-lev new-comp-env func-tag))
                            stat-list))
                          ((equal? 'nostmt (comp_flag-stmt flag)) 
                           'nostmt))))
    (stx:cmpd-stmt decl-list stat-list)))

(define (analy-compdecl st lev)
  ;;;;
  ;interdefinition
  
  (define (make-obj-from-decl decl type lev)
    (let* ((id (cond ((stx:decl-stmt? decl) 
                      (stx:decl-stmt-var decl))
                     ((stx:declpoint-stmt? decl)
                      (stx:declpoint-stmt-var decl))))
           (name (cond ((stx:var-exp? id) (stx:var-exp-tgt id))
                       ((stx:varpoint-exp? id) (stx:varpoint-exp-tgt id))
                       ((stx:arr-exp? id) (stx:arr-exp-tgt id))))  
           (pos (cond ((stx:var-exp? id) (stx:var-exp-pos id))
                       ((stx:varpoint-exp? id) (stx:varpoint-exp-pos id))
                       ((stx:arr-exp? id) (stx:arr-exp-pos id))))  
           (flag (cond ((stx:decl-stmt? decl) 'nomal)
                       ((stx:declpoint-stmt? decl) 'pointer)))
           (kind 'var)
           (type (cond ((stx:arr-exp? id) (type_array type (stx:arr-exp-num id)))
                       (else (cond ((equal? flag 'nomal) type)
                                   ((equal? flag 'pointer) (type_pointer 'pointer type)))))))
      (obj name lev kind type pos)))
  ;;end interdefinition
  
  (let* ((type (stx:int-stmt-type st))
         (declarator-list (stx:int-stmt-declist st))
         
         (obj-list (mapcons 
                    (lambda (x) (make-obj-from-decl x (stx:type-exp-op type) lev))
                    declarator-list)))
    
    (stx:int-stmt type obj-list)))

(define (analy-compstate st lev env func-tag)
  (cond ((and (stx:exp-stmt? st) (null? (stx:exp-stmt-stmt st))) st)
        ((stx:assign-stmt? st) 
         (stx:assign-stmt (analy-compstate (stx:assign-stmt-var st) lev env func-tag)
                            (analy-compstate (stx:assign-stmt-src st) lev env func-tag)
                            (stx:assign-stmt-pos st)))
        ((stx:lop-exp? st) 
         (stx:lop-exp    (stx:lop-exp-op st) 
                           (analy-compstate (stx:lop-exp-left st) lev env func-tag)
                           (analy-compstate (stx:lop-exp-right st) lev env func-tag)
                           (stx:lop-exp-pos st)))
        ((stx:rop-exp? st) 
         (stx:rop-exp   (stx:rop-exp-op st) 
                         (analy-compstate (stx:rop-exp-left st) lev env func-tag)
                         (analy-compstate (stx:rop-exp-right st) lev env func-tag)
                         (stx:rop-exp-pos st)))
        ((stx:aop-exp? st) 
         (stx:aop-exp   (stx:aop-exp-op st) 
                          (analy-compstate (stx:aop-exp-left st) lev env func-tag)
                          (analy-compstate (stx:aop-exp-right st) lev env func-tag)
                          (stx:aop-exp-pos st)))
        ((stx:deref-exp? st)
         (stx:unary-stmt '* (analy-compstate (stx:deref-exp-arg st) lev env func-tag) (stx:deref-exp-pos st)))
         ((stx:addr-exp? st)
         (stx:addr-exp   (analy-compstate (stx:addr-exp-var st) lev env func-tag) (stx:addr-exp-pos st)))
        ((stx:unary-stmt? st) 
          (stx:unary-stmt (stx:unary-stmt-mark st) 
                                              (analy-compstate 
                                               (stx:unary-stmt-stmt st) lev env func-tag)
                                              (stx:unary-stmt-pos st)))
        ((stx:lit-exp? st) st)
        ((stx:exp-stmt? st) 
         (stx:exp-stmt 
          (analy-compstate (stx:exp-stmt-stmt st) lev env func-tag) (stx:exp-stmt-pos st)))
        ((stx:expkakko-stmt? st) 
          (analy-compstate (stx:expkakko-stmt-stmt st) lev env func-tag))
        ((stx:if-stmt? st) 
         (stx:if-stmt 
          (analy-compstate (stx:if-stmt-test st) lev env func-tag)   
          (cond ((stx:cmpd-stmt? 
                  (analy-compstate (stx:if-stmt-tbody st) lev env func-tag))
                 (analy-compstate (stx:if-stmt-tbody st) lev env func-tag))
                (else 
                 (stx:cmpd-stmt
                  'nodecl
                  (analy-compstate (stx:if-stmt-tbody st) lev env func-tag))))
          (cond ((stx:cmpd-stmt?
                  (analy-compstate (stx:if-stmt-ebody st) lev env func-tag))
                 (analy-compstate (stx:if-stmt-ebody st) lev env func-tag))
                (else
                 (stx:cmpd-stmt
                  'nodecl
                  (analy-compstate (stx:if-stmt-ebody st) lev env func-tag))))
          (stx:if-stmt-pos st)))
        ((stx:while-stmt? st) 
         (stx:while-stmt 
          (analy-compstate (stx:while-stmt-test st) lev env func-tag)
          (analy-cmpd (stx:while-stmt-body st) lev env func-tag)
          (stx:while-stmt-pos st)))
        ((stx:ret-stmt? st) 
         (stx:smret-stmt 
          (analy-compstate (stx:ret-stmt-val st) lev env func-tag) 
          (stx:ret-stmt-pos st) 
          func-tag))
        ((stx:cmpd-stmt? st) (analy-cmpd st lev env func-tag))
        ((stx:kakko-exp? st) 
         (stx:kakko-exp (check-func-ref st lev env) 
                      (cond ((null? (stx:kakko-exp-para st)) 'nopara)
                            (else (flatten 
                                   (map (lambda (x) 
                                          (analy-compstate x lev env func-tag))
                                        (flatten (stx:kakko-exp-para st))))))
         (stx:kakko-exp-pos st)))
        ((or (stx:var-exp? st)     
             (stx:varpoint-exp? st))      
         (check-var-ref st lev 
                        (append (cond ((equal? 'nopara para-env) '()) 
                                      (else para-env)) env)))
        ((stx:arrvar-exp? st)
         (cond ((number? (stx:arrvar-exp-num st))
               
                (check-var-ref st lev 
                               (append (cond ((equal? 'nopara para-env) '()) 
                                             (else para-env)) env)))
               (else (let* ((original-name (stx:arrvar-exp-tgt st))
                            (original-pos (stx:arrvar-exp-pos st))
                            (new-num (analy-compstate (stx:arrvar-exp-num st) lev env func-tag)))
                       (check-var-ref (stx:arrvar-exp original-name new-num original-pos) lev 
                               (append (cond ((equal? 'nopara para-env) '()) 
                                             (else para-env)) env))))))
        ((cons? st) (flatten (list (car st) (cdr st))))
        ;debug     
        (else (error "ERROR! UNEXPECTED STRUCTURE!" st))
        ))

(define (sem-analyze-tree t)
  (define (sem-analyze-struct st)
    (cond ((stx:int-stmt? st) (analy-decl st 0))
          ((stx:funpro-stmt? st) (analy-funpro st))
          ((stx:fundef-stmt? st) (analy-fundef st))
          (else 
           (begin (eprintf
                   (format "SYNTAX ERROR! INVALID DEFINITION."))
                  (error (format "SYNTAX ERROR! INVALID DEFINITION."))))))
  (set! env '())
  (let* ((out-tree (mapcons sem-analyze-struct t)))
    (set! env '())
    out-tree))


(define ay (sem-analyze-tree psr:ax))
(define by (sem-analyze-tree psr:bx))
(define cy (sem-analyze-tree psr:cx))
(define dy (sem-analyze-tree psr:dx))

