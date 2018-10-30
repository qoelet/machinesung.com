#lang pollen

◊h1{GHC extension - TypeApplications}
◊h2{Providing type arguments explicitly}
◊m-article{
  ◊p{Learning more about type level programming has been something on my list of to dos for a while now, so I decided to start by looking at one of the many GHC extensions required for the job - ◊code{TypeApplications}. Let's look at the classic polymorphic function example: ◊code{id}, the identity function (I'm loading ◊code{ghci} with some additional command line options to display ◊a[#:href "https://ghc.haskell.org/trac/ghc/wiki/Commentary/Compiler/CoreSynType"]{Core})}
  ◊m-code-shell{
$ ghci

◊string->symbol{lambda}> id x = x

==================== Tidy Core ====================
Result size of Tidy Core
  = {terms: 18, types: 12, coercions: 0, joins: 0/0}

-- RHS size: {terms: 3, types: 3, coercions: 0, joins: 0/0}
id :: forall p. p -> p
id = \ (@ p) (x :: p) -> x
...
  }
  ◊p{Here we can see that ◊code{id} receives an implicit type variable ◊code{@p}, which represents the polymorphic type. With polymorphic function application, GHC deals with most of the unification process without the programmer needing to get involved. There are however, many examples of ambiguity; usually these are resolved with type annotations, but they can grow gnarly. In many Haskell libraries, you may have seen the following workaround: prior to GHC 8, controlling type variable instantiation meant using the ◊code{Proxy} type, which is a type that holds no data but is used to provide type information:}
  ◊m-code-haskell{
module NoTypeApp where

import Data.Proxy

data Foo = Foo
  deriving (Read, Show)

data Bar = Bar
  deriving (Read, Show)

foo :: String -> String
foo x = show ((read :: String -> Foo) x)

polyReadThenShow
  :: forall a . (Show a, Read a)
  => Proxy a
  -> String -> String
polyReadThenShow _ x = show ((read :: String -> a) x)

foo' :: String -> String
foo' = polyReadThenShow (Proxy :: Proxy Foo)

bar :: String -> String
bar = polyReadThenShow (Proxy :: Proxy Bar)
  }
  ◊p{An obvious application of the ◊code{TypeApplications} extension is that we can rewrite the above without the need for phantom typing:}
  ◊m-code-haskell{
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module HasTypeApp where

data Foo = Foo
  deriving (Read, Show)

data Bar = Bar
  deriving (Read, Show)

polyReadThenShow
  :: forall a . (Show a, Read a)
  => String -> String
polyReadThenShow = show . read @a

foo :: String -> String
foo = polyReadThenShow @Foo

bar :: String -> String
bar = polyReadThenShow @Bar
  }
  ◊p{This is of course, a rather trivial/dumb use; this would be more useful in scenarios like resolving ambiguity in type classes or type families. But while we're at it, here's another dumb example that demonstrates a trivial case of type ambiguity:}
  ◊m-code-shell{
-- adding on to the earlier example
-- class Qux a b where
--   qux :: String

-- instance Qux Foo Bar where
--   qux = "foo"

-- in ghci
◊string->symbol{lambda}> qux
<interactive>:28:1: error:
    * Ambiguous type variables `a0', `b0' arising from a use of `qux'
      prevents the constraint `(Qux a0 b0)' from being solved.
      Probable fix: use a type annotation to specify what `a0',
                                                          `b0' should be.
      These potential instance exist:
        instance [safe] Qux Foo Bar -- Defined at src/HasTypeApp.hs:29:10
    * In the expression: qux
      In an equation for `it': it = qux

◊string->symbol{lambda}> qux @Foo @Bar
"foo"
  }
  ◊p{If this interests you, you should check out the paper ◊a[#:href "https://www.seas.upenn.edu/~sweirich/papers/type-app-extended.pdf"]{Visible Type Applications (Extended version)}.}
}
◊m-back
◊m-mathjax
