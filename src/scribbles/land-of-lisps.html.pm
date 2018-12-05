#lang pollen

◊h1{Beginning AI}
◊h2{First explorations via Lisp}
◊m-article{
  ◊p{◊em{learning/writing in progress}}
  ◊p{Taking a more concrete look at the entire field of AI has been on my list of things to do this year for a while now; I got my hands on a few really beat up AI textbooks from a thrift store recently and decided to take some time to chow down the material. A lot of them involved examples in Lisp, so it made for a fun "dual" learning of sorts. Having written some Racket and Pollen, the syntax isn't entirely unfamiliar.}
  ◊m-code-lisp{
(car (cdr '(
  Here is a list of symbols
  constructed from atoms and lists in Lisp.
)))
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
  ◊p{In AI literature this would be similar to a ◊em{simple reflex agent}, or just simply, a lookup table that takes some given input and returns some action/state. For example, if we want some agent to tell us what to do with some given fruit:}
  ◊m-code-lisp{
(defun fruitbot (p)
  ;; a lookup table
  (setq tbl (make-hash-table))
  (setf (gethash 'apple tbl) 'eat)
  (setf (gethash 'orange tbl) 'peel-squeeze-and-drink)
  (setf (gethash 'durian tbl) 'pry-open-and-eat)

  ;; map precept to action
  (cond
    ((gethash p tbl) (gethash p tbl))
    (t 'i-cannot-help-you)))
  }
  ◊p{A common first step to making such an agent more reactive or may I say, less helpless, would be to add randomization. With our fruit bot, we could instead make a guess from known actions when it encounters a fruit not in the lookup table,}
}
  ◊m-code-lisp{
(defun fruitbot (p)
  ;; ...
  (maphash
    #'(lambda (key val)
       (setq actions (append (list val) actions))) tbl)

  ;; map precept to action
  (cond
    ((gethash p tbl) (gethash p tbl))
    (t (nth (random 3) actions))))
  }
◊m-back
