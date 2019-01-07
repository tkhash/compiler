#lang racket
;; (require rackunit)
(provide (all-defined-out))

;; declaration/definition
;; (struct parm-decl (name ty pos)                #:transparent)
;; (struct fun-decl  (name ret-ty parm-tys pos)   #:transparent)
;; (struct fun-def   (name ret-ty parms body pos) #:transparent)

;; statement

;; メモリへの書き込み: *<exp> = <exp>;
(struct massign-stmt (dst src pos)              #:transparent)

;; 変数への代入: <var> = <exp>;
(struct assign-stmt  (var src pos)              #:transparent)
;; 条件分岐: if(<exp>) <cmpd-stmt> else <cmpd-stmt>
(struct if-stmt      (test tbody ebody pos)     #:transparent)

;; 繰り返し: while(<exp>) <cmpd-stmt>
(struct while-stmt   (test body pos)            #:transparent)
(struct for-stmt     (test body tbody ebody pos)#:transparent)
   
;; 値の出力: print(<exp>);
(struct print-stmt   (exp pos)                  #:transparent)
;; 複文: {<stmt>;...<stmt>;} 
(struct cmpd-stmt    (decs stmts)               #:transparent)
;;return
(struct ret-stmt     (val pos)                  #:transparent)
(struct smret-stmt   (val pos tag)              #:transparent)

(struct exp-stmt    (stmt pos)                  #:transparent)
(struct expkakko-stmt    (stmt pos)             #:transparent)
(struct unary-stmt    (mark stmt pos)           #:transparent)



(struct funpro-stmt  (val tgt)              #:transparent)
(struct fundec-stmt  (val tgt pos)          #:transparent)
(struct fundecpoint-stmt  (val tgt pos)     #:transparent)
(struct fundef-stmt  (val tgt body)         #:transparent)


(struct decl-stmt    (var)                  #:transparent)
(struct declpoint-stmt   (var)              #:transparent)
(struct declists-stmt (type paralist)       #:transparent)
(struct declist-stmt  (type para)           #:transparent)
(struct paradec-stmt  (type para)           #:transparent)


(struct int-stmt (type declist)             #:transparent)

;; expression
;; 変数: <var>
(struct var-exp    (tgt pos)           #:transparent)
;; 変数pointar: *<var>
(struct varpoint-exp    (tgt pos)      #:transparent)
;; 偽を表す真偽値: true
(struct true-exp   (pos)               #:transparent)
;; 偽を表す真偽値: false
(struct false-exp  (pos)               #:transparent)
;; 整数即値: <num>
(struct lit-exp    (val pos)           #:transparent)
;; 算術演算: <left-exp> <op> <right-exp>
(struct aop-exp    (op left right pos) #:transparent)
;; 比較演算: <left-exp> <op> <right-exp>
(struct rop-exp    (op left right pos) #:transparent)
;; ronri演算: <left-exp> <op> <right-exp>
(struct lop-exp    (op left right pos) #:transparent)
;; hensuu:
(struct type-exp   (op pos)            #:transparent)
;; array: <exp>[<exp>]
(struct arr-exp    (tgt num pos)       #:transparent)
;; array in expression
(struct arrvar-exp (tgt num pos)       #:transparent)
;; kakko kansuu
(struct kakko-exp  (tgt para pos)       #:transparent)
;; 符号反転: -<exp>
(struct neg-exp    (arg pos)           #:transparent)
;; メモリ参照: *<exp>
(struct deref-exp   (arg pos)           #:transparent)
;; アドレスを取る: &<var>
(struct addr-exp  (var pos)           #:transparent)
;; (struct call-exp   (tgt args pos)      #:transparent)
