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
  ◊p{◊em{To be continued...}}
}
◊m-back
