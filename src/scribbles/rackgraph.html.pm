#lang pollen

◊h1{Basic graph theory}
◊h2{Computing with Racket and graphviz}
◊m-article{
  ◊p{◊em{writing in progress, check back later.}}
  ◊m-code-racket{
#lang racket

(require graph)
(require rackunit)

; In graph theory, G is a non-empty finite set of vertices V
; and edges E. We can denote the number of vertices by n (order)
; and number of edges by m (size)

(define trivial-and-empty-g (unweighted-graph/undirected '(a)))
(check-equal? (get-edges trivial-and-empty-g) '())
(check-equal? (get-vertices trivial-and-empty-g) '(a))

(define nontrivial-g (unweighted-graph/undirected '((a b))))
(check-equal? (has-edge? nontrivial-g 'a 'b) #t)

(define denser-g (unweighted-graph/undirected '((a b) (c d) (b c) (a d))))

; A graph G with order n and size m,
; the sum of the degrees of vertices
; is equal twice the number of edges
(check-equal?
  (apply + (list
            (length (get-neighbors denser-g 'a))
            (length (get-neighbors denser-g 'b))
            (length (get-neighbors denser-g 'c))
            (length (get-neighbors denser-g 'd))
             ))
  (length (get-edges denser-g)))

; We can produce a .dot file from a graph, for example
; (graphviz denser-g #:colors (coloring/brelaz denser-g))

; A complement G' of a graph G is one such that
; two vertices in G' are adjacent only if they
; are not adjacent in G
(define (remove-edges es g)
  (for ([e es])
    (remove-edge! g (car e) (cadr e)))
  g)
(define (restore-vertices vs g)
  (for ([v vs])
    (add-vertex! g v))
  g)
(define (make-g vs)
  (unweighted-graph/undirected (filter
   (lambda (p)
     (not (equal? (car p) (cadr p))))
   (cartesian-product vs vs))))
(define (get-complement g)
  (let*
    ([vs (get-vertices g)]
     [es (get-edges g)]
     [full-g (make-g vs)]
     [comp-g (restore-vertices vs (remove-edges es full-g))])
    comp-g))

(check-equal? (get-complement denser-g)
              (unweighted-graph/undirected '((d b) (c a) (b d) (a c))))
(check-equal? (get-complement trivial-and-empty-g)
              (unweighted-graph/undirected '(a)))
  }
  ◊p{At this point I wanted to display the graphs visually by invoking ◊code{graphviz}. Defining another graph at the same time,}
  ◊m-code-racket{
; Let's check another graph P
(define p (unweighted-graph/undirected '((a b) (b c) (a c) d)))
(check-equal? (get-vertices p) '(d c b a))
(check-equal? (get-edges p) '((c a) (c b) (b a) (b c) (a c) (a b)))
  }
  ◊div[#:class "columns is-centered"]{
    ◊figure[#:style "padding: 2rem;"]{
      ◊img[#:src "/assets/images/graph-p.png"]{}
    }
  }
  ◊m-code-racket{
(define p-comp (unweighted-graph/undirected '((a d) (b d) (c d))))
(check-equal? (get-vertices p-comp) '(d c b a))
(check-equal? (get-complement p) p-comp)
  }
  ◊div[#:class "columns is-centered"]{
    ◊figure[#:style "padding: 2rem;"]{
      ◊img[#:src "/assets/images/graph-p-comp.png"]{}
    }
  }
  ◊p{From here we can consider the idea of ◊em{multigraphs}, which are graphs with multiple parallel edges joining same pairs of vertices. As before, we ignore the notion of direction.}
  ◊m-code-racket{
(define w1-g (weighted-graph/undirected '((1 a b) (2 b c) (3 a c))))
(check-equal? (edge-weight w1-g 'a 'c) 3)
  }
  ◊div[#:class "columns is-centered"]{
    ◊figure[#:style "padding: 2rem;"]{
      ◊img[#:src "/assets/images/graph-weighted.png"]{}
    }
  }
  ◊p{The degree of a vertex in a weighted graph is simply the sum of the weights of the edges incident to it. Let's implement a function to check that,}
  ◊m-code-racket{
(define (vertex-in-pair v p)
  (or
    (equal? (car p) v)
    (equal? (cadr p) v)))
(define (edge-weight-from-pair g p)
  (edge-weight g (car p) (cadr p)))
(define (vertex-degree g v)
  (/ (apply +
    (map
      ((curry edge-weight-from-pair) g)
      (filter
        ((curry vertex-in-pair) v)
        (get-edges g)))) 2))

(check-equal? (vertex-degree w1-g 'a) 4)
(check-equal? (vertex-degree w1-g 'b) 3)
(check-equal? (vertex-degree w1-g 'c) 5)
  }
  ◊p{In addition, a weighted graph is said to be ◊em{irregular} if the vertices have distinct degrees.}
  ◊m-code-racket{
; A counter-example
(define w2-g (weighted-graph/undirected '((1 a b) (1 b c) (3 a c))))
(define (irregular-graph? g)
  (let*
    ([ds (map
           ((curry vertex-degree) g)
           (get-vertices g))])
    (or
      (= (length ds) (set-count (list->set ds)))
      (> 1 (set-count (list->set ds))))))

(check-equal? (irregular-graph? w1-g) #t)
(check-equal? (irregular-graph? w2-g) #f)
(check-equal? (irregular-graph? denser-g) #f)
  }
  ◊p{In contrast, if every vertex of a graph has the same degree, then it's said to be regular. Also, if we have a complete graph, then the complement would have 0 edges.}
  ◊m-code-racket{
(define r5-g (make-g '(a b c d e)))
(check-equal? (irregular-graph? r5-g) #f)
(check-equal? (get-edges (get-complement r5-g)) '())
  }
  ◊div[#:class "columns is-centered"]{
    ◊figure[#:style "padding: 2rem;"]{
      ◊img[#:src "/assets/images/graph-r5.png"]{}
    }
  }
  ◊m-code-racket{
; Subgraphs
; A graph H is a subgraph of graph G
(define (subgraph? h g)
  (and
    (subset?
      (list->set (get-edges h))
      (list->set (get-edges g)))
    (subset?
      (list->set (get-vertices h))
      (list->set (get-vertices g)))))

(define denser-h (unweighted-graph/undirected '((a b) (b c))))
(check-equal? (subgraph? denser-h denser-g) #t)
(check-equal? (subgraph? denser-g denser-g) #t)

(define (spanning-subgraph? h g)
  (and
    (subgraph? h g)
    (equal?
      (get-vertices h)
      (get-vertices g))))

(define spanning-h (unweighted-graph/undirected '((a b) (b c) d)))
(check-equal? (spanning-subgraph? denser-h denser-g) #f)
(check-equal? (spanning-subgraph? spanning-h denser-g) #t)

; Connectedness in graphs
; The graph package provides `cc`
(define (connected g x y)
  (define (in-g x y g-lst)
    (and
      (not (equal? #f (member x g-lst)))
      (not (equal? #f (member y g-lst)))))
  (ormap
    (((curry in-g) x) y) (cc g)))

(define (disconnected g x y)
  (not (connected g x y)))

(check-equal? (connected denser-g 'a 'd) #t)
(check-equal? (connected spanning-h 'a 'd) #f)
(check-equal? (disconnected spanning-h 'a 'd) #t)
  }
  ◊p{A ◊em{bridge} is an edge of a graph if and only if it does not belong to a cycle in the graph. I highlighted these red in the generated graph image:}
  ◊m-code-racket{
(define (is-bridge? g u v)
  (let*
    ([new-g (graph-copy g)])
    (remove-edge! new-g u v)
    (and
      (disconnected new-g u v)
      (<
        (length (cc g))
        (length (cc new-g))))))

(define bridged-g (unweighted-graph/undirected '((a b) (b c) (a c) (c d) (d e))))
(check-equal? (is-bridge? bridged-g 'a 'b) #f)
(check-equal? (is-bridge? bridged-g 'b 'c) #f)
(check-equal? (is-bridge? bridged-g 'a 'c) #f)
(check-equal? (is-bridge? bridged-g 'c 'd) #t)
(check-equal? (is-bridge? bridged-g 'd 'e) #t)
  }
  ◊div[#:class "columns is-centered"]{
    ◊figure[#:style "padding: 2rem;"]{
      ◊img[#:src "/assets/images/graph-bridge.png"]{}
    }
  }
  ◊p{Lastly, we want to consider the notion of direction in graphs. A very common construction is that of a binary tree (or decision tree,}
  ◊m-code-racket{
(define b-tree
  (unweighted-graph/directed '((a b) (a c) (c d) (c e))))
(check-equal? (has-edge? b-tree 'a 'b) #t)
(check-equal? (has-edge? b-tree 'b 'a) #f)
  }
  ◊div[#:class "columns is-centered"]{
    ◊figure[#:style "padding: 2rem;"]{
      ◊img[#:src "/assets/images/graph-btree.png"]{}
    }
  }
  ◊p{◊em{To be continued...}}
}
◊m-back
