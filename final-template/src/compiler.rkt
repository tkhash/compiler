#lang racket
(require "imikaiseki.rkt")
(require "parser.rkt")
(require (prefix-in gen: "gen.rkt"))
(require "katakensa.rkt")
(require "address.rkt")
(require "environment.rkt")
(require (prefix-in chu: "chukan.rkt"))
(provide (all-defined-out))

(define (code->strings code) (map instr->string code))

(define (address->string addr)
  (cond
   [(symbol? addr) (symbol->string addr)]
   [(number? addr) (number->string addr)]
   [else addr]))

(define (instr->string i)
  (cond
   [(gen:instr? i)
    (let* ([op (gen:instr-op i)]
	   [args (gen:instr-args i)]
	   [argsstr (string-join (map address->string args) ",")])
      (format "\t~a\t~a" (symbol->string op) argsstr))]
   [(chu:label-stmt? i)
    (let* ([label (chu:label-stmt-name i)])
      (format "~a:" (address->string label)))]
   [(gen:dir? i)
    (let* ([label (gen:dir-label i)]
	   [args (gen:dir-args i)]
	   [argsstr (string-join (map symbol->string args) ",")])
      (format "\t~a\t~a" label argsstr))]
   [(gen:global? i)
    (let* ([name (gen:global-name i)]
           [size (gen:global-size i)])
      (cond ((null? size)
             (format "~a:\t.word 0" name))
            (else 
             (format "~a:\t.word 0:~a" name size))))]
))

(define (compile filename)
  (let* ((file-port (open-input-file filename))
         (meaningless  (port-count-lines! file-port))
         (sem-analyzed-tree (sem-analyze-tree 
                             (parse-port file-port)))
         (meaningless (katakensa sem-analyzed-tree)))
     (display 
      (string-join 
       (code->strings 
        (gen:intermed-prog->code 
         (gen-assigned-itmd 
          (assign-add-intermed 
           (chu:gen-optimized-intermed 
            sem-analyzed-tree))))) "\n"))))

(define at (string-join (code->strings gen:au) "\n"))
(define bt (string-join (code->strings gen:bu) "\n"))
(define ct (string-join (code->strings gen:cu) "\n"))
(define dt (string-join (code->strings gen:du) "\n"))


