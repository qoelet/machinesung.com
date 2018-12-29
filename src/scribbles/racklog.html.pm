#lang pollen

◊h1{Racklog}
◊h2{Racket extended with logic programming}
◊m-article{
 ◊p{
	TIL today that there's ◊a[#:href "https://docs.racket-lang.org/racklog/index.htm://docs.racket-lang.org/racklog/index.html"]{Racklog}, an embedding of Prolog-style within Racket. I had fun following through the short introduction just to see how it measured up.
  }
  ◊m-code-racket{
#lang racket

(require racklog)
(require rackunit)

; Let's define some helpers for our queries
(define (test expr)
  (check-equal? (%which () expr) '()))
(define (fails expr)
  (check-equal? (%which () expr) #f))
(define (query expr)
  (cond
    [(equal? (%which () expr) '()) 'yes]
    [else 'no]))

; Predicates
(test (%=:= 1 1))
(test (%< 1 2))
(fails (%=:= 1 2))

; Facts
(define %drinks
  (%rel ()
        [('Kenny 'coffee)]
        [('Kenny 'tea)]
        [('Isaac 'milk)]))

(query (%drinks 'Kenny 'coffee)) ; 'yes
(query (%drinks 'Kenny 'milk)) ; 'no

(define %identity
  (%rel ()
        [('Kenny 'an-adult)]
        [('Audrey 'an-adult)]
        [('Isaac 'a-child)]))

; Adding rules
(define %an-adult?
  (%rel (person)
        [(person)
         (%identity person 'an-adult)]
        [(person)
         (%drinks person 'coffee)]))

(query (%an-adult? 'Isaac)) ; no
(query (%an-adult? 'Audrey)) ; yes

; Solving goals
(%which (who) (%drinks who 'tea)) ; '((who . Kenny))
(%more) ; '((who . Audrey))
(%more) ; #f - no more solutions

; Add an extra clause
(%assert! %drinks () [('Audrey 'water)])
(%which (who) (%drinks who 'water)) ; '((who . Audrey))

; Conjunction
(%which (who)
        (%and (%identity who 'an-adult)
              (%and (%drinks who 'tea)
                    (%drinks who 'coffee)))) ; '((who . Kenny))
  }
}
◊m-back
