#lang racket

(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         parser-tools/yacc)
(require "environment.rkt")
(require (prefix-in stx: "syntax.rkt"))
(require (prefix-in psr: "parser.rkt"))
(require (prefix-in kat: "katakensa.rkt"))
(require (prefix-in imi: "imikaiseki.rkt"))
(provide (all-defined-out))
; ; プログラムは var-decl と fun-def のリスト
; 変数宣言
(struct var-decl (var) #:transparent)
; 関数定義
(struct fun-def (var parms body) #:transparent) ; parms は var-decl のリスト
 
; ; 文
; 変数への代入: <var> = <exp>;
(struct assign-stmt (var exp) #:transparent)
; メモリへの書き込み: *<dest> = <src>;
(struct write-stmt (dest src) #:transparent)
; メモリ参照: <dest> = *<src>;
(struct read-stmt (dest src) #:transparent)
; ラベル: <name>:
(struct label-stmt (name) #:transparent)
; 条件分岐: if(<var>){ goto <tlabel>; } else { goto <elabel>; }
(struct if-stmt (var tlabel elabel) #:transparent)
; 繰り返し文 while(<var>){<stmt>}
(struct while-stmt (var stmt) #:transparent)
; 無条件分岐: goto <label>;
(struct goto-stmt (label) #:transparent)
; 関数呼出し: <dest> = <tgt>(<var1>, <var2>, <var3>, ...);
(struct call-stmt (dest tgt vars) #:transparent) ; vars は var のリスト
; リターン: return <var>;
(struct ret-stmt (var) #:transparent)
; 値の出力: print(<var>);
(struct print-stmt (var) #:transparent)
; 複文: {<decls> <stmts>}
(struct cmpd-stmt (decls stmts) #:transparent) ; decls は var-decl のリスト,stmts は文のリスト

(struct null-stmt () #:transparent)
; ; 式
; 変数参照
(struct var-exp (var) #:transparent)
; 整数即値
(struct lit-exp (val) #:transparent)
; 算術演算
(struct aop-exp (op left right) #:transparent)
; 比較演算
(struct rop-exp (op left right) #:transparent)
; アドレス取得: &<var>
(struct addr-exp (var) #:transparent)


(define label-maxid 0)
(define fresh-maxid 0)
(define intermed-code (list '()))
(define temp-decl (list '()))
(define intermed-decllist (list '()))

(define temp-space '())

(define (fresh-symbol)  ; 呼び出すたびに fresh な識別子を返す関数
  (let* ((new-name (string->symbol (string-append "temp" (number->string fresh-maxid))))
         (new-obj (obj new-name 'nolev 'nokin 'notyp 'nopos)))
    (set! fresh-maxid (+ 1 fresh-maxid))
    (set! temp-decl (flatten (append temp-decl (list (var-decl new-obj)))))
    new-obj))

(define (make-base-temp array-obj)　　; 呼び出すたびに fresh な識別子を返す関数(arrayver)
  (let* ((name (obj-name array-obj))
         (lev (obj-lev array-obj))
         (kind (obj-kind array-obj)))
         (obj (array_base name lev kind) 'nolev 'array-base 'notyp 'nopos)))

(define (fresh-label) ; 呼び出すたびに fresh なラベルを返す関数
      (let*
          ([oldid label-maxid])
        (set! label-maxid (+ label-maxid 1)) 
        (string-append "label" (number->string oldid)))) 

(struct array_base (name lev kind) #:transparent)



(define (gen-intermed st) 
    (flatten (list temp-decl (map syn-to-inter st))))

(struct array_base_add (ar) #:transparent)

(define (syn-to-inter st) 
  (cond
    ((stx:int-stmt? st) 
     (let* ((decl-ls (stx:int-stmt-declist st))
            (meaningles (set! temp-space '())))
       (cond ((obj? decl-ls) 
             (var-decl (decl-ls)))
             ((list? decl-ls) 
              (flatten 
               (map (lambda (x) 
                      (var-decl x)) 
                    decl-ls)))
             (error (format "\n error! ~a\n" st)))))
    ((stx:fundef-stmt? st)
     (let* ((fun-dec (stx:fundef-stmt-tgt st))
            (fun-obj (stx:fundec-stmt-val fun-dec))
            (fun-para-list (stx:fundec-stmt-tgt fun-dec))
            (fun-body (stx:fundef-stmt-body st)))
       (fun-def fun-obj 
                (cond ((equal? 'nopara fun-para-list) '())
                      (else (map (lambda (x) (var-decl x)) fun-para-list)))
                (syn-to-inter fun-body))))
    ;not necessary
    ((stx:funpro-stmt? st) '())
    
    ((stx:assign-stmt? st) 
     (let* ((var (stx:assign-stmt-var st))
            (src (stx:assign-stmt-src st))
            (syn-to-intered-src (cond ((stx:lit-exp? src) 
                                       (lit-exp (stx:lit-exp-val src))) 
                                      (else (syn-to-inter src)))))
       
       
       (cond ((stx:unary-stmt? var)
              (cond ((equal? '* (stx:unary-stmt-mark var))
                     (write-stmt (syn-to-inter
                                  (stx:unary-stmt-stmt var)) syn-to-intered-src))
                    (else
                    (assign-stmt (syn-to-inter var) syn-to-intered-src))))
              (else (assign-stmt (syn-to-inter var) syn-to-intered-src)))))
    ((stx:lop-exp? st) 
     (let* ((op (stx:lop-exp-op st))
            (tleft (syn-to-inter (stx:lop-exp-left st)))
            (tright (syn-to-inter (stx:lop-exp-right st)))
            (tall (syn-to-inter (fresh-symbol))))
       (cond ((equal? '|| op)
              (begin
                (set! intermed-code
                 (append 
                  intermed-code 
                  (flatten 
                   (list 
                    (if-stmt tleft 
                                (assign-stmt tall (lit-exp 1)) 
                               (if-stmt tright 
                                           (assign-stmt tall (lit-exp 1)) 
                                           (assign-stmt tall (lit-exp 0))))))))
                tall))
             ((equal? '&& op)
              (begin 
                (set! intermed-code
                      (append intermed-code
                              (flatten (list (if-stmt tleft 
                                             (if-stmt tright 
                                              (assign-stmt tall (lit-exp 1)) 
                                              (assign-stmt tall (lit-exp 0)))
                                               (assign-stmt tall (lit-exp 0)))))))
                tall)))))                              
    ((stx:rop-exp? st) 
     (let* ((op (stx:rop-exp-op st))           
            (left (stx:rop-exp-left st))
            (right (stx:rop-exp-right st))
            (tleft (syn-to-inter left))
            (tright (syn-to-inter right))
            (tall (syn-to-inter (fresh-symbol))))
       (begin
         (set! intermed-code
               (append 
                intermed-code
                (flatten (list
                          (assign-stmt tall (rop-exp op tleft tright))))))
         tall)))
    ((stx:aop-exp? st) 
     (let* ((op (stx:aop-exp-op st))
            (left (stx:aop-exp-left st))
            (right (stx:aop-exp-right st))
            (left (cond ((number? left) (stx:lit-exp left 'nopos))
                       (else left)))
            (right (cond ((number? right) (stx:lit-exp right 'nopos))
                       (else right)))
            (tleft (syn-to-inter left))
            (tright (syn-to-inter right))
            (tall (syn-to-inter (fresh-symbol)))
            (left-type (cond ((obj? left) 
                             (cond ((or (type_pointer? (obj-type left))
                                        (type_array? (obj-type left))) 'int-pointer)
                                   (else 'int)))
                            (else 'int)))
            (right-type (cond ((obj? right) 
                             (cond ((or (type_pointer? (obj-type right))
                                        (type_array? (obj-type right))) 'int-pointer)
                                   (else 'int)))
                            (else 'int))))
       (cond 
         ((and (equal? 'int-pointer left-type) (equal? 'int right-type)) 
          (begin 
            (let* ((tal2 (fresh-symbol))
                   (tal3 (fresh-symbol)))
              (set! intermed-code
                    (append 
                     intermed-code
                     (list
                      (assign-stmt tal2 (lit-exp 4))
                      (assign-stmt tal3 (aop-exp '* tright tal2))
                      (assign-stmt tall (aop-exp '+ tleft tal3))))))
            tall))
         ((and (equal? 'int-pointer right-type) (equal? 'int left-type)) 
          (begin 
            (let* ((tal2 (fresh-symbol))
                   (tal3 (fresh-symbol)))
              (set! intermed-code
                    (append 
                     intermed-code
                     (list
                      (assign-stmt tal2 (lit-exp 4))
                      (assign-stmt tal3 (aop-exp '* tleft tal2))
                      (assign-stmt tall (aop-exp '+ tright tal3))))))
            tall))
         (else
          (begin
            (set! intermed-code
                  (append 
                   intermed-code
                   (flatten (list 
                             (assign-stmt tall (aop-exp op tleft tright))))))
            tall)))))
    ((stx:unary-stmt? st)      
       (cond ((equal? '& (stx:unary-stmt-mark st))
              (addr-exp (syn-to-inter (stx:unary-stmt-stmt st))))
             ((equal? '* (stx:unary-stmt-mark st))
              (let* ((tall (fresh-symbol))
                     (syn-to-intered-left (syn-to-inter (stx:unary-stmt-stmt st))))
                (set! intermed-code
                                  (flatten 
                                   (append 
                                    intermed-code 
                                    (list (read-stmt tall syn-to-intered-left))))) 
                                 tall))))
    ((stx:lit-exp? st) 
     (let* ((temp (fresh-symbol)))
       (begin
         (set! intermed-code
             (append 
              intermed-code
              (flatten (list  
                   (assign-stmt temp (lit-exp (stx:lit-exp-val st)))))))
       temp)))
    ((stx:expkakko-stmt? st) (syn-to-inter (stx:expkakko-stmt-stmt st)))
    ((stx:exp-stmt? st) (syn-to-inter (stx:exp-stmt-stmt st)))
    ((stx:if-stmt? st)
      (if-stmt (syn-to-inter (stx:if-stmt-test st)) 
                  (syn-to-inter (stx:if-stmt-tbody st))
                  (syn-to-inter (stx:if-stmt-ebody st))))
  ((stx:while-stmt? st)
   (while-stmt (syn-to-inter (stx:while-stmt-test st))
               (syn-to-inter (stx:while-stmt-body st)))
     #;(let* ((temp (fresh-symbol))
            (label1 (fresh-label))
            (label2 (fresh-label))
            (label3 (fresh-label))
            (test (stx:while-stmt-test st)))
      (begin (set! intermed-code
       (append intermed-code
              (flatten (list  
                   (label-stmt label1)
                   (if-stmt (syn-to-inter test)
                            (goto-stmt label2)
                            (goto-stmt label3))
                   (label-stmt label2)
                   (syn-to-inter (stx:while-stmt-body st))
                   (goto-stmt label1)
                   (label-stmt label3)
                   )))) '()
             )))
    ((stx:smret-stmt? st) 
       (ret-stmt (syn-to-inter (stx:smret-stmt-val st))))
    ((stx:cmpd-stmt? st) 
     (let* ((decs (stx:cmpd-stmt-decs st))
            (stmts (stx:cmpd-stmt-stmts st))
            (original-intermed intermed-code)
            (original-temp fresh-maxid)
            (original-tdecl temp-decl)
            (meaningless (set! intermed-code '()))
            (meaningless (set! fresh-maxid 0))
            (meaningless (set! temp-decl (list '())))
            (decl (cond ((equal? 'nodecl decs) '())
                        (else (flatten (map syn-to-inter decs)))))
            (stmt (cond
                    ((equal? 'nostmt stmts) '())
                    (else (flatten 
                           (map 
                            (lambda (x) (let* ((meaningless (set! intermed-code '()))
                                               (syn-to-intered-x (syn-to-inter x)))
                                          (flatten (append (list intermed-code)
                                                           (list syn-to-intered-x)))))
                            (flatten (list stmts)))))))
            (stmt-temp-decl temp-decl)
            (meaningless (set! intermed-code original-intermed))
            (meaningless (set! fresh-maxid original-temp))
            (meaningless (set! temp-decl original-tdecl)))
       (cmpd-stmt (flatten (append decl stmt-temp-decl)) 
                     stmt
                     )))          
    ((stx:kakko-exp? st) 
     (cond 
       ((equal? 'print (obj-name (stx:kakko-exp-tgt st))) 
        (let* ((para (flatten (stx:kakko-exp-para st))))
          (print-stmt (syn-to-inter (car para)))))
       (else
        (let* ((para (stx:kakko-exp-para st))
               (tgt (stx:kakko-exp-tgt st))
               (return-type (type_fun-out (obj-type tgt)))
               (tall (syn-to-inter (fresh-symbol)))
               (let-var
                (cond
                  ((equal? 'nopara para)'())
                  (else
                   (map 
                    (lambda (x)
                      (let* ((syn-to-intered-x (syn-to-inter x)))
                        (assign-stmt
                         (fresh-symbol)
                         (cond ((or (lit-exp? syn-to-intered-x)
                                    (aop-exp? syn-to-intered-x)
                                    (rop-exp? syn-to-intered-x)
                                    (addr-exp? syn-to-intered-x))
                                syn-to-intered-x)
                               (else (cond ((type_array? (obj-type syn-to-intered-x))
                                            (addr-exp syn-to-intered-x))
                                           (else 
                                            (var-exp syn-to-intered-x)))))))) 
    　　　　　　　para)))))
          (begin
            (set! intermed-code
            (append 
             intermed-code
             (flatten (list
                       let-var
                       (call-stmt tall 
                                  tgt 
                                  (map (lambda (x) (assign-stmt-var x)) let-var))))))
            (cond ((equal? 'void return-type) '())
                  (else tall)))))))
    ((obj? st)  st)
    (else (error (format "\n check syn-to-inter!! ~a\n" st)))))


(define (optimize-cmpd st outer-tdecls)
  (cond ((cmpd-stmt? st)
         (let* ((decls (cmpd-stmt-decls st))
                (stmts (cmpd-stmt-stmts st))
                (tdecls 
                 (filter (lambda (x) (equal? 'temp (obj-type (var-decl-var x)))) decls))
                (new-var-decls (remove* tdecls decls))
                (new-tdecls 
                 (remove* outer-tdecls tdecls))
                (new-outer-decls
                 (flatten (append (list outer-tdecls) (list new-tdecls)))))
           (cmpd-stmt (flatten (append (list new-var-decls) (list new-tdecls)))
                      (flatten
                       (map (lambda (x) (optimize-cmpd x new-outer-decls)) stmts)))))
        ((if-stmt? st)
         (let* ((var (if-stmt-var st))
                (tlabel (if-stmt-tlabel st))
                (elabel (if-stmt-elabel st)))
           (if-stmt var 
                    (optimize-cmpd tlabel outer-tdecls) 
                    (optimize-cmpd elabel outer-tdecls))))
        (else st)))

(define (optimize-intermed intermed)
  (map (lambda (x) (cond ((fun-def? x) 
                          (let* ((var (fun-def-var x))
                                 (parms (fun-def-parms x))
                                 (body (optimize-cmpd (fun-def-body x) '())))
                            (fun-def var parms body)))
                         (else x)))
       intermed))

(define (gen-optimized-intermed tree)
  (optimize-intermed (gen-intermed tree)))



(define (main fname)
  (begin (kat:katakensa (imi:sem-analyze-tree (psr:parse-file fname)))
         (gen-optimized-intermed (imi:sem-analyze-tree (psr:parse-file fname)))))
  
 ;;test 
(define aw (gen-optimized-intermed imi:ay))
(define bw (gen-optimized-intermed imi:by))
(define cw (gen-optimized-intermed imi:cy))
(define dw (gen-optimized-intermed imi:dy))
