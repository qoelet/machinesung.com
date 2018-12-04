#lang pollen

◊h1{Lisp and AI}
◊h2{'(Fun with symbolic manipulation)}
◊m-article{
  ◊p{◊em{writing in progress}}
  ◊m-code-lisp{
(car (cdr '(
  Here is a list of symbols
  constructed from atoms and lists in Lisp.
)))
;; evaluates to IS
  }
  ◊p{The ◊a[#:href "https://en.wikipedia.org/wiki/Person-centered_therapy"]{Rogerian 'psychiatrist'}. The following snippet of code from a book I was reading is hilarious - but is truly the roots of what we call AI chatbots these days. It is fun to witness how this works: a collection of facts, pattern matching and some helper functions to switch contexts around can make for an almost bearable conversation - if you're feeling truly bored! Most chatbot implementations don't actually veer far from this - once you start asking very generic questions where there are no matches to their knowledge base then you get a chain of fallback statements like "I do not understand your question - can you rephrase..." and the like.}
  ◊m-code-lisp{
(defun shrink ()
  (setq wword-count 0)
  (setq punt-count 0)
  (format t "Welcome to my sofa!~%")
  (format t "Please enclose your input in parentheses.~%")
  (loop
    (setq s (you-me-map (read)))
    (cond
      ((match '(bye) s)
       (return 'goodbye))
      ((match '(you are (* x)) s)
       (printl (append '(please tell me)
                       (list (wword))
                       '(you are)
                       x)))
      ((match '(you have (* x)) s)
       (printl (append '(how long have you had) x)))
      ((match '(you feel (* x)) s)
       (format t "I sometimes feel the same way.~%"))
  ...
  }
  ◊m-code-shell{
[2]> (shrink)
Welcome to my sofa!
Please enclose your input in parentheses.
(hi)
TELL ME MORE
(about?)
I SEE
(lol)
WHAT DOES THAT INDICATE
  }
  ◊p{This actually reminded me of ◊a[#:href "https://github.com/qoelet/kopilog/blob/master/kopilog.pl"]{a toy project} that I did years ago in Prolog, where I wanted to represent a set of facts revolving around the Singaporean culture of ordering drinks at the local coffeeshop and then use backtracking to get the exact verbal order to use given some instantiation of the facts.}
  ◊m-code-shell{
?- order(coffee, sugar(none), milk(condensed), temperature(hot), dilution_factor(low)).
kopi gao kosong sio
true.
?- order(tea, sugar(less), milk(condensed), temperature(cold), dilution_factor(low)).
teh gao siudai peng
true.
  }
}
◊m-back
