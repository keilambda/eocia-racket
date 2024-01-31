#lang racket

;;; AST
(struct Int (value))
(struct Prim (op args))
(struct Var (name))
(struct Let (name value body))
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
(define (interp-exp e)
  (match e
    [(Int n) n]
    [(Prim 'read '())
     (let ([r (read)])
       (cond [(fixnum? r) r]
             [else (error 'interp-exp "read expected an integer: ~v" r)]))]
    [(Prim '- (list a))   (- 0 (interp-exp a))]
    [(Prim '- (list a b)) (- (interp-exp a) (interp-exp b))]
    [(Prim '+ (list a b)) (+ (interp-exp a) (interp-exp b))]))

(define (interp-Lint p)
  (match p
    [(Program '() e) (interp-exp e)]))

;;; Partial evaluation
(define (pe-negate a)
  (match a
    [(Int n) (Int (- 0 n))]
    [_ (negate a)]))

(define (pe-minus a b)
  (match* (a b)
    [((Int na) (Int nb)) (Int (- na nb))]
    [(_ _) (minus a b)]))

(define (pe-plus a b)
  (match* (a b)
    [((Int na) (Int nb)) (Int (+ na nb))]
    [(_ _) (plus a b)]))

(define (pe-exp e)
  (match e
    [(Int n) (Int n)]
    [(Prim 'read '()) rd]
    [(Prim '- (list a))   (pe-negate (pe-exp a))]
    [(Prim '- (list a b)) (pe-minus (pe-exp a) (pe-exp b))]
    [(Prim '+ (list a b)) (pe-plus (pe-exp a) (pe-exp b))]))

(define (pe-Lint p)
  (match p
    [(Program '() e) (Program '() (pe-exp e))]))

(define interp-Lint%
  (class object%
    (super-new)

    (define/public (interp-exp env e)
      (match e
        [(Int n) n]
        [(Prim 'read '())
         (let ([r (read)])
           (cond [(fixnum? r) r]
                 [else (error 'interp-exp "read expected an integer: ~v" r)]))]
        [(Prim '- (list e)) (- 0 (interp-exp env e))]
        [(Prim '- (list a b)) (- (interp-exp env a) (interp-exp env b))]
        [(Prim '+ (list a b)) (+ (interp-exp env a) (interp-exp env b))]))

    (define/public (interp-program p)
      (match p
        [(Program '() e) (interp-exp '() e)]))))

(define interp-Lvar%
  (class interp-Lint%
    (super-new)

    (define/override (interp-exp env e)
      (match e
        [(Var name) (dict-ref env name)]
        [(Let var rhs body) (interp-exp (dict-set env var (interp-exp env rhs)) body)]
        [_ (super interp-exp env e)]))))

(define (interp-Lvar p)
  (send (new interp-Lvar%) interp-program p))
