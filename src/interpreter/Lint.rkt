#lang racket

;;; AST
(struct Int (value))
(struct Prim (op args))
(struct Program (info body))

;;; Primitives
(define rd          (Prim 'read '()))
(define (negate a)  (Prim '- (list a)))
(define (minus a b) (Prim '- (list a b)))
(define (plus a b)  (Prim '+ (list a b)))

;;; Operations
(define (leaf? ast)
  (match ast
    [(Int _) #t]
    [(Prim 'read '()) #t]
    [_ #f]))

(define (exp? ast)
  (match ast
    [(Int _) #t]
    [(Prim 'read '()) #t]
    [(Prim _ args) (andmap exp? args)]
    [_ #f]))

(define (Lint? ast)
  (match ast
    [(Program '() e) (exp? e)]
    [_ #f]))

;;; Interpreter
(define (interp_exp e)
  (match e
    [(Int n) n]
    [(Prim 'read '())
     (let ([r (read)])
       (cond [(fixnum? r) r]
             [else (error 'interp_exp "read expected an integer: ~v" r)]))]
    [(Prim '- (list a))   (- 0 (interp_exp a))]
    [(Prim '- (list a b)) (- (interp_exp a) (interp_exp b))]
    [(Prim '+ (list a b)) (+ (interp_exp a) (interp_exp b))]))

(define (interp_Lint p)
  (match p
    [(Program '() e) (interp_exp e)]))
