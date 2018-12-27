#lang pollen

◊h1{Typed Racket}
◊h2{Exploring gradual typing in Racket}
◊m-article{
  ◊p{◊em{learning/writing in progress}}
  ◊p{Racketland continues to hold many nice things and remains a decent pastime for me over the holiday season. While I'm having fun jumping between the mountain of material I've planned out for myself, most of which includes ancient Lisp textbooks and high level programming refreshers in Racket/Scheme, I wondered where the types were.}
  ◊p{Taking a simple Project Euler problem that I wrote out in Racket, let's quickly convert it into a typed version}
  ◊m-code-racket{
#lang racket

(define (is-multiple-of n k)
  (zero? (modulo n k)))

(define (sumList xs)
  (foldl (lambda (v i) (+ v i)) 0 xs))

(define (filter-multiples-of-three-or-five xs)
  (filter
    (lambda (i) (or (is-multiple-of i 3) (is-multiple-of i 5))) xs))

; Solve
(sumList (filter-multiples-of-three-or-five (range 1 1000)))

;; > (is-multiple-of 12 -1)
;; #t
  }
  ◊m-code-racket{
#lang typed/racket

(: is-multiple-of (-> Natural Natural Boolean))
(define (is-multiple-of n k)
  (zero? (modulo n k)))

(: sumList (-> (Listof Natural) Natural))
(define (sumList xs)
  (foldl
    (lambda ([v : Natural] [i : Natural]) (+ v i)) 0 xs))

(: filter-multiples-of-three-or-five (-> (Listof Natural) (Listof Natural)))
(define (filter-multiples-of-three-or-five xs)
  (filter
    (lambda ([i : Natural])
      (or (is-multiple-of i 3) (is-multiple-of i 5))) xs))

; Solve
(sumList (filter-multiples-of-three-or-five (range 1 1000)))

;; > (is-multiple-of 12 -1)
;; . Type Checker: type mismatch
;;   expected: Nonnegative-Integer
;;   given: Negative-Fixnum in: -1
  }
  ◊p{All types in Racket are basically subtypes of ◊code{Any}, and numbers have a really layered hierarchy in the language. We can introduce any custom types of our choosing via ◊code{struct}, define type synonyms with ◊code{define-type} and more. Here's a sampling:}
  ◊m-code-racket{
#lang typed/racket

(require typed/rackunit)

(define-type Age Natural)

(struct person(
    [name : String]
    [age : Age]
    [member : Boolean]))

(define: foo : person (person "foo" 42 #t))
(define: bar : person (person "bar" 72 #f))

(assert foo person?) ; #<person>

(: show-privilege (-> person (Option String)))
(define (show-privilege p)
  (cond
    [(person-member p) "Members get free drinks!"]
    [else #f]))

(check-equal? (show-privilege bar) #f)
(check-equal? (show-privilege foo) "Members get free drinks!")
  }
  ◊p{And we could also gradually type regions of our code using the ◊code{with-type} form. For example, we could have chose to add typing to just one of the functions in the Project Euler example earlier,}
  ◊m-code-racket{
#lang racket

(require typed/racket)

(define (is-multiple-of n k)
  (zero? (modulo n k)))

(define (sumList xs)
  (foldl (lambda (v i) (+ v i)) 0 xs))

(define (filter-multiples-of-three-or-five xs)
  (filter
  ; a localized Typed Racket region
   (with-type
       #:result (Natural -> Boolean)
       #:freevars ([is-multiple-of (-> Natural Natural Boolean)])
    (lambda ([i : Natural])
      (or (is-multiple-of i 3)
          (is-multiple-of i 5))
      )) xs))

;; > (filter-multiples-of-three-or-five (range 1.0 4.0))
;; .../contract/region.rkt:700:62: contract violation
;;   expected: Natural
;;   given: 1.0
;;   in: the 1st argument of
;;       (-> Natural any)
;;   contract from: (region typed-region)
;;   blaming: anonymous-module
;;    (assuming the contract is correct)
  }
}
◊m-back
