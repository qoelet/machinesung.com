#lang pollen/mode racket/base
(require pollen/tag)
(require rackunit)
(provide (all-defined-out))

; tags
(define a (default-tag-function 'a))
(define article (default-tag-function 'article))
(define code (default-tag-function 'code))
(define div (default-tag-function 'div))
(define p (default-tag-function 'p))
(define pre (default-tag-function 'pre))
(define script (default-tag-function 'script))

; shorthands
(define (m-row . contents)
  `(div ((class "columns")) (div ((class "column")) ,@contents)))

(define (m-article . contents)
  `(div ((class "columns")) (div ((class "column")) (article ,@contents))))

(define (m-code . contents)
  `(pre (code ,@contents)))

(define m-back
  `(a ((class "arrow") (href "/")) "<< back"))

(define m-mathjax
  `(script ((src "https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.5/MathJax.js?config=TeX-MML-AM_CHTML"))))

(define (m-code-haskell . xs)
	`(pre (code ((class ,(format "~a" "haskell"))) ,@xs)))

(define (m-code-lisp . xs)
	`(pre (code ((class ,(format "~a" "lisp"))) ,@xs)))

(define (m-code-lua . xs)
	`(pre (code ((class ,(format "~a" "lua"))) ,@xs)))

(define (m-code-racket . xs)
	`(pre (code ((class ,(format "~a" "racket"))) ,@xs)))

(define (m-code-mercury . xs)
	`(pre (code ((class ,(format "~a" "mercury"))) ,@xs)))

(define (m-code-prolog . xs)
	`(pre (code ((class ,(format "~a" "prolog"))) ,@xs)))

(define (m-code-shell . xs)
	`(pre (code ((class ,(format "~a" "shell"))) ,@xs)))

(define (m-code-yaml . xs)
	`(pre (code ((class ,(format "~a" "yaml"))) ,@xs)))

; tests
(check-equal? ◊m-row{"foo"}
  ◊div[#:class "columns"]{◊div[#:class "column"]{"foo"}})

(check-equal? ◊m-article{"foo"}
  ◊div[#:class "columns"]{◊div[#:class "column"]{◊article{"foo"}}})

(check-equal? ◊m-code{"foo"}
  ◊pre{◊code{"foo"}})

(check-equal? ◊m-back
  ◊a[#:href "/" #:class "arrow"]{<< back})

(check-equal? ◊m-mathjax
  ◊script[#:src "https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.5/MathJax.js?config=TeX-MML-AM_CHTML"])

(check-equal? ◊m-code-shell{"foo"}
  ◊pre{◊code[#:class "shell"]{"foo"}})

(check-equal? ◊m-code-yaml{"foo"}
  ◊pre{◊code[#:class "yaml"]{"foo"}})
