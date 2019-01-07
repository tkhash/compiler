#lang racket
(require (prefix-in chu: "chukan.rkt")
         (prefix-in addr: "address.rkt"))
(require "environment.rkt")
(require rackunit)
(require racket/trace)
(provide (all-defined-out))

;; registers

(define zero '$zero) ; constant 0
(define at '$at) ; reserved for assembler

(define v0 '$v0) ; expression evaluation and results of a function
(define v1 '$v1) ; expression evaluation and results of a function

(define a0 '$a0) ; argument 1
(define a1 '$a1) ; argument 2
(define a2 '$a2) ; argument 3
(define a3 '$a3) ; argument 4

(define t0 '$t0) ; temporary (not preserved accross call)
(define t1 '$t1) ; temporary (not preserved accross call)
(define t2 '$t2) ; temporary (not preserved accross call)
(define t3 '$t3) ; temporary (not preserved accross call)
(define t4 '$t4) ; temporary (not preserved accross call)
(define t5 '$t5) ; temporary (not preserved accross call)
(define t6 '$t6) ; temporary (not preserved accross call)
(define t7 '$t7) ; temporary (not preserved accross call)
(define t8 '$t8) ; temporary (not preserved accross call)
(define t9 '$t9) ; temporary (not preserved accross call)

(define s0 '$s0) ; saved temporary (preserved accross call)
(define s1 '$s1) ; saved temporary (preserved accross call)
(define s2 '$s2) ; saved temporary (preserved accross call)
(define s3 '$s3) ; saved temporary (preserved accross call)
(define s4 '$s4) ; saved temporary (preserved accross call)
(define s5 '$s5) ; saved temporary (preserved accross call)
(define s6 '$s6) ; saved temporary (preserved accross call)
(define s7 '$s7) ; saved temporary (preserved accross call)

(define k0 '$k0) ; reserved for OS kernel
(define k1 '$k1) ; reserved for OS kernel

(define gp '$gp) ; pointer to global area
(define sp '$sp) ; stack pointer
(define fp '$fp) ; frame pointer
(define ra '$ra) ; return address (used by function call)

;; opcode

(define abs     'abs) ; abs rdest, rsrc

(define add     'add)   ; add   rd, rs, rt
(define addi    'addi)  ; addi  rt, rs, imm
(define addiu   'addiu) ; addiu rt, rs, imm
(define addu    'addu)  ; addu  rd, rs, rt

(define op-and  'and)  ; and  rd, rs, rt
(define andi    'andi) ; andi rt, rs, imm

(define b       'b)      ; b      label
(define bclf    'bclf)   ; bclf   cc lael
(define bclt    'bclt)   ; bclt   cc lael
(define beq     'beq)    ; beq    rs, rt, label
(define beqz    'beqz)   ; beqz   rsrc, label
(define bge     'bge)    ; bge    rsrc1, rsrc2, label
(define bgeu    'bgeu)   ; bgeu   rsrc1, rsrc2, label
(define bgez    'bgez)   ; bgez   rs, label
(define bgezal  'bgezal) ; bgezal rs, label
(define bgt     'bgt)    ; bgt    rsrc1, src2, label
(define bgtu    'bgtu)   ; bgtu   rsrc1, src2, label
(define bgtz    'bgtz)   ; bgtz   rs, label
(define ble     'ble)    ; ble    rsrc1, src2, label
(define bleu    'bleu)   ; bleu   rsrc1, src2, label
(define blez    'blez)   ; blez   rs, label
(define blt     'blt)    ; blt    rsrc1, rsrc2, label
(define bltu    'bltu)   ; bltu   rsrc1, rsrc2, label
(define bltz    'bltz)   ; bltz   rs, label
(define bltzal  'bltzal) ; bltzal rs, label
(define bne     'bne)    ; bne    rs, rt, label
(define bnez    'bnez)   ; bnez   rsrc, label


(define clo     'clo) ; clo rd, rs
(define clz     'clz) ; clz rd, rs

(define div     'div)  ; div  rs, rt / div  rdest, rsrc1, src2
(define divu    'divu) ; divu rs, rt / divu rdest, rsrc1, src2

(define j       'j)    ; j    target
(define jal     'jal)  ; jal  target
(define jalr    'jalr) ; jalr rs, rd
(define jr      'jr)   ; jr   rs

(define li      'li)  ; li  rdest, imm
(define lui     'lui) ; lui rt, imm

(define la      'la)   ; la   rdest, address
(define lb      'lb)   ; lb   rt, address
(define lbu     'lbu)  ; lbu  rt, address
(define ld      'ld)   ; ld   rdest, address
(define lh      'lh)   ; lh   rt, address
(define lhu     'lhu)  ; lhu  rt, address
(define ll      'll)   ; ll   rt, address
(define lw      'lw)   ; lw   rt, address
(define lwc1    'lwc1) ; lwc1 ft, address
(define lwl     'lwl)  ; lwl  rt, address
(define lwr     'lwr)  ; lwr  rt, address
(define ulh     'ulh)  ; ulh  rdest, address
(define ulhu    'ulhu) ; ulhu rdest, address
(define ulw     'ulw)  ; ulw  rdest, address

(define move    'move) ; move rdest rsrc
(define movf    'movf) ; movf rd, rs, cc
(define movn    'movn) ; movn rd, rs, rt
(define movt    'movt) ; movt rd, rs, cc
(define movz    'movz) ; movz rd, rs, rt
(define mfc0    'mfc0) ; mfc0 rt, rd
(define mfc1    'mfc1) ; mfc1 rt, fs
(define mfhi    'mfhi) ; mfhi rd
(define mflo    'mflo) ; mflo rd
(define mthi    'mthi) ; mthi rs
(define mtlo    'mtlo) ; mtlo rs
(define mtc0    'mtc0) ; mtc0 rd, rt
(define mtc1    'mtc1) ; mtc1 rd, fs

(define madd    'madd)  ; madd  rs, rt
(define maddu   'maddu) ; maddu rs, rt

(define msub    'msub)  ; msub  rs, rt
(define msubu   'msubu) ; msubu rs, rt

(define mul     'mul)   ; mul   rd, rs, rt
(define mulo    'mulo)  ; mulo  rdest, rsrc1, src2
(define mulou   'mulou) ; mulou rdest, rsrc1, src2

(define mult    'mult)  ; mult  rs, rt
(define multu   'multu) ; multu rs, rt

(define neg     'neg)  ; neg  rdest, rsrc
(define negu    'negu) ; negu rdest, rsrc

(define nop     'nop) ; nop

(define nor     'nor) ; nor rd, rs, rt

(define notMIPS 'not) ; not rdest, rsrc

(define op-or   'or)  ; or  rd, rs, rt
(define ori     'ori) ; ori rt, rs, imm

(define rem     'rem)  ; rem rdest, rsrc1, rsrc2
(define remu    'remu) ; rem rdest, rsrc1, rsrc2

(define rol     'rol)  ; rol rdest, rsrc1, rsrc2
(define ror     'ror)  ; ror rdest, rsrc1, rsrc2

(define sb      'sb)   ; sb   rt, address
(define sc      'sc)   ; sc   rt, address
(define sd      'sd)   ; sd   rsrc, address
(define sh      'sh)   ; sh   rt, address
(define sw      'sw)   ; sw   rt, address
(define swc1    'swc1) ; swc1 ft, address
(define sdc1    'sdc1) ; sdc1 ft, address
(define swl     'swl)  ; swl  rt, address
(define swr     'swr)  ; swr  rt, address
(define ush     'ush)  ; ush  rsrc, address
(define usw     'usw)  ; usw  rsrc, address

(define seq     'seq)   ; seq   rdest, rsrc1, rsrc2
(define sge     'sge)   ; sge   rdest, rsrc1, rsrc2
(define sgeu    'sgeu)  ; sgeu  rdest, rsrc1, rsrc2
(define sgt     'sgt)   ; sgt   rdest, rsrc1, rsrc2
(define sgtu    'sgtu)  ; sgtu  rdest, rsrc1, rsrc2
(define sle     'sle)   ; sle   rdest, rsrc1, rsrc2
(define sleu    'sleu)  ; sleu  rdest, rsrc1, rsrc2
(define slt     'slt)   ; slt   rd, rs, rt
(define slti    'slti)  ; sltu  rt, rs, imm
(define sltiu   'sltiu) ; sltiu rt, rs, imm
(define sltu    'sltu)  ; sltu  rd, rs, rt
(define sne     'sne)   ; sne   rdest, rsrc1, rsrc2

(define sll     'sll)  ; sll  rd, rt, shamt
(define sllv    'sllv) ; sllv rd, rt, rs
(define sra     'sra)  ; sra  rd, rt, shamt
(define srav    'srav) ; srav rd, rt, rs
(define srl     'sra)  ; srl  rd, rt, shamt
(define srlv    'srav) ; srlv rd, rt, rs

(define sub     'sub)  ; sub  rd, rs, rt
(define subu    'subu) ; subu rd, rs, rt

(define syscall 'syscall) ; syscall

(define xor     'xor)  ; xor  rd, rs, rt
(define xori    'xori) ; xori rt, rs, imm

;; directives
(define .align  '.align)
(define .ascii  '.ascii)
(define .asciiz '.asciiz)
(define .byte   '.byte)
(define .data   '.data)
(define .double '.double)
(define .extern '.extern)
(define .float  '.float)
(define .globl  '.globl)
(define .half   '.half)
(define .kdata  '.kdata)
(define .ktext  '.ktext)
(define .set    '.set)
(define .space  '.space)
(define .text   '.text)
(define .word   '.word)



;命令
(struct instr (op args) #:transparent)
;ディレクティブ
(struct dir (label args) #:transparent)
;大域変数
(struct global (name size) #:transparent)
;コメント
(struct comment (arg) #:transparent)
;returnレジスタ
(define retreg '$v0)
;fpレジスタ
(define fpreg '$fp)

(define (addr->sym sym)
  (cond 
    ((addr:obj-off? sym) 
     (cond ((type_array? (addr:obj-off-type sym)) 
            (string->symbol (format "~a($fp)" (addr:obj-off-off sym))))
           (else (string->symbol (format "~a($fp)" (addr:obj-off-off sym))))))
    ((chu:lit-exp? sym) '())
    ((obj? sym) (obj-name sym))
    (else (error (format "debug in addr->sym \n~a\n" sym)))))

(define (makesp of)
  (string->symbol (format "~a($sp)" of)))

(define (gen-global-code gloval-vars)
  (map (lambda (x)
         (cond ((not (type_array? (obj-type (chu:var-decl-var x))))
                (global (obj-name (chu:var-decl-var x)) '()))
               (else (global (obj-name (chu:var-decl-var x)) 
                             (type_array-size (obj-type (chu:var-decl-var x)))))))
       gloval-vars))

(define (intermed-prog->code it-and-st)
  (let* ((itmd (addr:itmd-and-stack-it it-and-st))
         [global-vars (filter (lambda (x) (chu:var-decl? x)) itmd)]
         [global-code (gen-global-code global-vars)]
         (st (addr:itmd-and-stack-st it-and-st))
         [fds (filter (lambda (x) (chu:fun-def? x)) itmd)]
         [localvarsize 
          (* addr:wordsize (length (filter (lambda (x) (chu:var-decl? x)) itmd)))]
         [main 
          (car 
           (cond ((null? 
                   (filter (lambda (x) (equal? 'main (obj-name (chu:fun-def-var x)))) fds))
                  (error (format "ERROR! PLEASE MAKE [main] FUNCTION!")))
                 (else (filter (lambda (x) (equal? 'main (obj-name (chu:fun-def-var x)))) fds))))]
         [fdscode 
          (flatten 
           (map 
            (lambda (x) (intermed-fundef->code x st))
            (remove main fds)))]
         [maincode 
          (intermed-fundef->code main st)])
    (flatten
     (list
      (dir .data '())
      global-code
      (dir .text '())
      fdscode
      maincode
      ))))

(define (intermed-fundef->code fd st)
  (let* ([f (obj-name (chu:fun-def-var fd))]
         [fst 
          (car 
           (filter 
            (lambda (x) (equal? (chu:fun-def-var fd) (addr:fun-stack-fun x))) 
            st))]
         [localandargs (addr:fun-stack-vars fst)]
         [localvar (filter (lambda (x) (> 4 (addr:obj-off-off x))) localandargs)]
         [array-base (filter (lambda (x) (chu:array_base? (addr:obj-off-name x))) localvar)]
         [base-inst (cond ((null? array-base) '())
                          (else (flatten 
                                 (map 
                                  (lambda (x) (let* ((ofs (addr:obj-off-off x)))
                                                (list (instr li `(,t0 ,ofs))
                                                      (instr sw `(,t0 ,(addr->sym x)))))) 
                                  array-base))))]
         [args (filter (lambda (x) (< 0 (addr:obj-off-off x))) localandargs)]
         [localvarsize (cond ((null? localvar) 0)
                             (else (- addr:wordsize 
                                      (addr:obj-off-off (list-ref localvar 
                                                             (- (length localvar) 1))))))]
         [argbyte (* addr:wordsize (length args))]
         [stmts (chu:fun-def-body fd)]
         [code (intermed-stmt->code localvarsize argbyte stmts)])
    (flatten 
     (list
      (chu:label-stmt f)
      (savecode localvarsize argbyte)
      base-inst
      code
      (restorecode localvarsize argbyte)))))

(define (savecode localvarbyte argbyte)
  (let* ([localandfpra (+ localvarbyte (* addr:wordsize 2))]
	 [framesize (+ localandfpra argbyte)])
    (list
     (instr subu `($sp $sp ,framesize))
     (instr sw `($ra |4($sp)|))
     (instr sw `($fp |0($sp)|))
     (instr addiu `($fp $sp ,(- localandfpra 4))))))


(define (restorecode localvarbyte argbyte)
  (let* ([localandfpra (+ localvarbyte 8)]
	 [framesize (+ localandfpra argbyte)])
    (list
     (instr lw `($fp |0($sp)|))
     (instr lw `($ra |4($sp)|))
     (instr addiu `($sp $sp ,framesize))
     (instr jr `($ra)))))

(define (intermed-stmt->code localvarbyte argbyte s)
  (cond
   [(chu:write-stmt? s)
    (let* ([dest (chu:write-stmt-dest s)]
	   [symdest (addr->sym dest)]
	   [destderef (string->symbol (format "0(~a)" (symbol->string t1)))]
	   [src (chu:write-stmt-src s)]
	   [symsrc (addr->sym src)])
      (cond ((chu:lit-exp? src) 
             (list (instr li `(,t0 ,(chu:lit-exp-val src)))
                   (instr lw `(,t1 ,symdest))
                   (instr sw `(,t0 ,destderef))))
            (else 
             (list (instr lw `(,t0 ,symsrc))
                   (instr lw `(,t1 ,symdest))
                   (instr sw `(,t0 ,destderef))))))]
   [(chu:read-stmt? s)
    (let* ([dest (chu:read-stmt-dest s)]
           [symdest (addr->sym dest)]
           [srcderef (string->symbol (format "0(~a)" (symbol->string t0)))]
           [src (chu:read-stmt-src s)]
           [symsrc (addr->sym src)])
      (list (instr lw `(,t0 ,symsrc))
            (instr lw `(,t0 ,srcderef))
            (instr sw `(,t0 ,symdest))))]
   [(chu:assign-stmt? s)
    (let* ([dest (chu:assign-stmt-var s)]
           [exp (chu:assign-stmt-exp s)])
      (intermed-exp->code dest exp))]
   [(chu:if-stmt? s)
    (let* ([sym (addr->sym (chu:if-stmt-var s))]
	   [stmts1 (let* ((stmt1 (chu:if-stmt-tlabel s)))
                     (cond ((chu:cmpd-stmt? stmt1)
                            (chu:cmpd-stmt-stmts stmt1))
                           (else (list stmt1))))]
	   [stmts2 (let* ((stmt2 (chu:if-stmt-elabel s)))
                     (cond ((chu:cmpd-stmt? stmt2)
                            (chu:cmpd-stmt-stmts stmt2))
                           (else (list stmt2))))]
	   [code1 
            (flatten 
             (map 
              (lambda (stmt) (intermed-stmt->code localvarbyte argbyte stmt)) 
              stmts1))]
           [code2 
            (flatten 
             (map 
              (lambda (stmt) (intermed-stmt->code localvarbyte argbyte stmt)) 
              stmts2))]
	   [label1 (chu:fresh-label)]
	   [label2 (chu:fresh-label)])
      (flatten (list 
		(instr lw `(,t0 ,sym))
		(instr beqz `(,t0 ,label1))
		code1
		(instr j `(,label2))
		(chu:label-stmt label1)
		code2
		(chu:label-stmt label2))))]
   [(chu:while-stmt? s)
    (let* ((var (addr->sym (chu:while-stmt-var s)))
           (body (chu:cmpd-stmt-stmts (chu:while-stmt-stmt s)))
           (code
            (flatten
             (map
              (lambda (stmt)
                (intermed-stmt->code localvarbyte argbyte stmt))
              body)))
           (label1 (chu:fresh-label))
           (label2 (chu:fresh-label)))
      (flatten (list (instr lw `(,t0 ,var))
                     (chu:label-stmt label1)
                     (instr beqz `(,t0 ,label2))
                     code
                     (instr j `(,label1))
                     (chu:label-stmt label2))))]
   [(chu:ret-stmt? s)
    (let* ((var (chu:ret-stmt-var s))
           (var (addr->sym var)))
      (list (instr lw `(,t0 ,var))
            (instr move `(,retreg ,t0))
            (restorecode localvarbyte argbyte)
            ))]
   [(chu:call-stmt? s)
    (define (save-args varlist varnum)
      (cond ((equal? '() varlist) '())
            (else (let* ((arg (addr->sym (car varlist)))
                         (n (* 4 varnum))
                         (sp (makesp (- 0 n)))
                         (m (- varnum 1)))
                    (flatten 
                     (append 
                      (list (instr lw `(,t0 ,arg))
                            (instr sw `(,t0 ,sp)))
                      (save-args (cdr varlist) m)))))))
   (let* [(func (obj-name (chu:call-stmt-tgt s)))
          (varnum (length (chu:call-stmt-vars s)))
          (varlist (chu:call-stmt-vars s))
          (dest (chu:call-stmt-dest s))
          (dest (addr->sym dest))]
     (list
      (save-args varlist varnum)
      (instr jal `(,func))
      (instr sw `(,retreg ,dest))))]
   [(chu:print-stmt? s)
    (let* ([src (chu:print-stmt-var s)]
	   [symsrc (addr->sym src)])
      (list (instr li `($v0 1))
            (instr lw `(,t0 ,symsrc))
            (instr move `($a0 ,t0))
            (instr syscall '())
            (instr li `($v0 11))
            (instr li `($a0 10))
            (instr syscall '())))]
   [(chu:cmpd-stmt? s)
    (let* ((stmts (chu:cmpd-stmt-stmts s)))
      (flatten 
       (list (map 
              (lambda (x) (intermed-stmt->code localvarbyte argbyte x)) 
              stmts))))]
   [(chu:label-stmt? s) s]
   [(chu:goto-stmt? s) 	(instr j `(,(chu:goto-stmt-label s)))]
   [else (error (format "check intermed-stmt->code \n~a\n" s))]))

(define (intermed-exp->code dest e)
  (cond
    [(chu:lit-exp? e)
     (let* ([arg (chu:lit-exp-val e)]
            [val arg]
	   [symaddr (addr->sym dest)])
       (list (instr li `(,t0 ,val))
             (instr sw `(,t0 ,symaddr))))]
   [(chu:var-exp? e)
    (let* ([arg (chu:var-exp-var e)]
	   [symsrc (addr->sym arg)]
           [symdest (addr->sym dest)])
      (list (instr lw `(,t0 ,symsrc))
            (instr sw `(,t0 ,symdest))))]
   [(chu:aop-exp? e)
    (let* ([op (string->symbol 
                (format "~a" 
                        (cond ((equal? '+ (chu:aop-exp-op e)) add)
                              ((equal? 'add (chu:aop-exp-op e)) add)
                              ((equal? '- (chu:aop-exp-op e)) sub)
                              ((equal? '* (chu:aop-exp-op e)) mul)
                              ((equal? 'mul (chu:aop-exp-op e)) mul)
                              ((equal? '/ (chu:aop-exp-op e)) div)
                              (else 
                               (error (format "check intermed-exp->code in aop-exp"))))))]
           [left (chu:aop-exp-left e)]
           [right (chu:aop-exp-right e)]
	   [left (addr->sym left)]
	   [right (addr->sym right)]
	   [dest (addr->sym dest)])
      (cond ((obj? (chu:aop-exp-left e))
             (cond ((type_array? (obj-type (chu:aop-exp-left e)))
                    (list (instr la `(,t0 ,left))
                          (instr lw `(,t1 ,right))
                          (instr op `(,t0 ,t0 ,t1))
                          (instr sw `(,t0 ,dest))))
                   (else 
                    (list (instr lw `(,t0 ,left))
                          (instr lw `(,t1 ,right))
                          (instr op `(,t0 ,t0 ,t1))
                          (instr sw `(,t0 ,dest))))))
            ((type_array? (addr:obj-off-type (chu:aop-exp-left e)))
             (list (instr la `(,t0 ,left))
                   (instr lw `(,t1 ,right))
                   (instr op `(,t0 ,t0 ,t1))
                   (instr sw `(,t0 ,dest))))
            (else
             (list (instr lw `(,t0 ,left))
                   (instr lw `(,t1 ,right))
                   (instr op `(,t0 ,t0 ,t1))
                   (instr sw `(,t0 ,dest))))))]
   [(chu:rop-exp? e)
    (let* ((op (string->symbol 
                (format "~a" 
                        (cond ((equal? '< (chu:rop-exp-op e)) slt)
                              ((equal? '== (chu:rop-exp-op e)) seq)
                              ((equal? '>= (chu:rop-exp-op e)) sge)
                              ((equal? '> (chu:rop-exp-op e)) sgt)
                              ((equal? '<= (chu:rop-exp-op e)) sle)
                              ((equal? '!= (chu:rop-exp-op e)) sne)
                              (else 
                               (error (format "check intermed-exp->code in rop-exp")))))))
           (left (chu:rop-exp-left e))
           (right (chu:rop-exp-right e))
           (left (addr->sym left))
           (right (addr->sym right))
           (dest (addr->sym dest)))
      (list (instr lw `(,t0 ,left))
            (instr lw `(,t1 ,right))
            (instr op `(,t0 ,t0 ,t1))
            (instr sw `(,t0 ,dest))))]
   [(chu:addr-exp? e)
    (let* ([arg (chu:addr-exp-var e)]
           [symsrc (addr->sym arg)]
           [symdest (addr->sym dest)])
      (list (instr la `(,t0 ,symsrc))
            (instr sw `(,t0 ,symdest))))]
   [(addr:obj-off? e)
    (let* ([symsrc (addr->sym e)]
           [symdest (addr->sym dest)])
      (list (instr lw `(,t0 ,symsrc))
            (instr sw `(,t0 ,symdest))))]
   (else (error (format "check intermed-exp->code \n~a\n" e)))))


;test
(define (makegen f) (intermed-prog->code (addr:addrmain f)))
(define au (intermed-prog->code addr:av))
(define bu (intermed-prog->code addr:bv))
(define cu (intermed-prog->code addr:cv))
(define du (intermed-prog->code addr:dv))
