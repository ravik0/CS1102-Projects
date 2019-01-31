#lang racket
(require test-engine/racket-tests)

;; #:transparent makes structures display a bit nicer
(define-struct graph (name nodes) #:transparent #:mutable)

(define-struct node (name edges) #:transparent #:mutable)

;; *******************************************************
;; you don't need to understand this part
;; just be sure you use my-eval instead of eval
(define-namespace-anchor a)
(define ns (namespace-anchor->namespace a))


(define-syntax my-eval
  (syntax-rules ()
    [(my-eval arg)
     (eval arg ns)]))
;; *******************************************************

;; some examples of Racket weirdness you will need to work around
(define x 3)
(define y 5)
(set! y 6)

;; in the interpreter, try:
;;     (set! x 10)
;;     (set! y 10)
;; first one fails, second one works
;; --> if you want a variable you can change later, will need both
;;     define and set! in your macro
;; (create z 99) could be a handy macro to define and set a variable

(define z (list y))
;          (list 6)
(set! y 8)
;; in the interpreter, try
;;        y
;;        z
;; it looks like values don't update properly.  how can we make it update properly?

(define z2 (list (quote y)))
(set! y 11)
;; what is z2?
;; it's '(y)
;; how to get back the 11?

;; how about (my-eval (first z2)) ?
;;    -> that's how we'll store lists of nodes

;PART 1

;node graph -> void
;adds a node to a graph, returns void if the node is already in the graph
(define (add-unique node graph)
  (cond [(member (node-name node) (graph-nodes graph)) void]
        [else
         (set-graph-nodes! graph (cons (node-name (my-eval node)) (graph-nodes graph)))]))

;creates a new graph
(define-syntax new
  (syntax-rules (graph)
    [(new graph g)
     (begin
       (define g (make-graph (quote g) empty))
       (set! g (make-graph (quote g) empty)))]))

;creates a node and inserts node into the graph
(define-syntax vertex
  (syntax-rules (in)
    [(vertex n in g)
     (begin
       (define n (make-node (quote n) empty))
       (set! n (make-node (quote n) empty))
       (add-unique n g))]))

;PART 2

;creates a connection (an edge) between two nodes
(define-syntax edge
  (syntax-rules ()
    [(edge n1 n2)
     (cond [(->? n1 n2) void]
           [else
            (set-node-edges! n1 (cons (node-name n2) (node-edges n1)))])]))

;creates edges between multiple nodes
(define-syntax edges
  (syntax-rules (<- <-> ->)
    [(edges n1 -> n2)
     (edge n1 n2)]
    [(edges n1 <- n2)
     (edge n2 n1)]
    [(edges n1 <-> n2)
     (begin
       (edge n1 n2)
       (edge n2 n1))]
    [(edges n1 proc n2 proc2 ...)
     (begin
       (edges n1 proc n2)
       (edges n2 proc2 ...))]))

;PART 3

(check-expect (->? n0 n1) true)
(check-expect (->? n0 n2) false)
(check-expect (->? n1 n0) false)
;checks if two nodes have a directed node
(define-syntax ->?
  (syntax-rules ()
    [(->? n0 n1)
     (not (false? (member (node-name n1) (node-edges n0))))]))

(check-expect (<->? n0 n1) false)
(check-expect (<->? n1 n2) true)
(check-expect (<->? n2 n1) true)
;checks if two nodes have an undirected edge
(define-syntax <->?
  (syntax-rules ()
    [(<->? n0 n1)
     (and (->? n0 n1) (->? n1 n0))]))

;PART 4

(check-expect (-->? n0 n2) true)
(check-expect (-->? n0 n0) true)
(check-expect (-->? n0 n3) true)
(check-expect (-->? n3 n0) false)
(check-expect (-->? n3 n4) true)
(check-expect (-->? n4 n1) false)
;node node -> boolean
;determines if you can get to one node from another node
(define (-->? n0 n1)
  (local [(define (fn-for-node n todo visited)
          (cond [(member (node-name (my-eval n1)) (node-edges (my-eval n))) true]
                [(member (node-name (my-eval n)) visited) (fn-for-lon todo visited)]
                [else (fn-for-lon (append todo (node-edges (my-eval n))) (cons n visited))]))
          (define (fn-for-lon todo visited)
            (cond [(empty? todo) false]
                  [(fn-for-node (first todo) (rest todo) visited) true]
                  [else (fn-for-lon (rest todo) visited)]))]
    (fn-for-node n0 empty empty)))

(check-expect (all-nodes g0) (map node-name (list n4 n3 n0 n1 n2)))
(check-expect (all-nodes g1) (map node-name (list n3 n4)))
;graph -> (listof symbols)
;takes a graph and returns all nodes (as symbols) that you can get to from traversing through its nodes 
(define (all-nodes g)
  (local [(define (fn-for-node n todo visited)
            (if (member n visited)
                (fn-for-lon todo visited)
                (fn-for-lon (append todo (node-edges (my-eval n))) (cons n visited))))
          (define (fn-for-lon todo visited)
            (cond [(empty? todo) visited]
                  [else
                   (fn-for-node (first todo) (rest todo) visited)]))]
    (fn-for-lon (graph-nodes g) empty)))

(new graph g0)
(new graph g1)
(vertex n0 in g0)
(vertex n1 in g0)
(vertex n2 in g0)
(vertex n3 in g1)
(vertex n4 in g1)
(edges n0 -> n1 <-> n2 -> n0 -> n3 <-> n4)
(test)