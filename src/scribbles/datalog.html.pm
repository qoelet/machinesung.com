#lang pollen

◊h1{Datalog}
◊h2{Logical queries in Racket}
◊m-article{
  ◊m-code-racket{
#lang datalog

% Let's add some facts to the relation `parent`
parent(kenny, isaac).
parent(audrey, isaac).
parent(harry, tom).
parent(foo, bar).

% We can query who are parents of Isaac
parent(A, isaac)?

% No answer for the following query
parent(A, foobar)?

% We can derive new facts
child(B, A) :- parent(A, B).
hasTwoParents(A, B, C) :-
  parent(A, C), parent(B, C), A != B.

child(A, kenny)?
hasTwoParents(A, B, isaac)?

% We can retract facts
parent(harry, tom)~
parent(A, tom)?

% Let's define another relation `personalInfo`
personalInfo(kenny, 91231231).
personalInfo(foo, 99111199).

% Some other operations
joinOnParent(X, Y, Z) :-
  parent(X, Y), personalInfo(X, Z).

projectOnJoin(X, P) :-
  joinOnParent(A, X, P).

projectOnJoin(X, 91231231)?
  }
  ◊m-code-racket{
parent(kenny, isaac).
parent(audrey, isaac).

child(isaac, kenny).
hasTwoParents(kenny, audrey, isaac).
hasTwoParents(audrey, kenny, isaac).

projectOnJoin(isaac, 91231231).
  }
}
◊m-back
