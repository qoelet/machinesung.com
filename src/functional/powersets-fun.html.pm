#lang pollen

◊h1{Power sets}
◊h2{Deconstruction of a nifty expression}
◊m-article{
  ◊p{I was browsing through the IRC logs on ◊a[#:href "https://wiki.haskell.org/IRC_channel"]{#haskell} when I came upon this expression for defining a power-set: ◊span[#:class "tilda"]{powerset = filterM (const [True, False])}. This piqued my interest, and soon I had a little puzzle on hand. Let's take a look at how this works.}
  ◊m-code-shell{
◊string->symbol{lambda}> powerset = filterM (const [True, False])
◊string->symbol{lambda}> powerset []
[[]]
◊string->symbol{lambda}> powerset [1,2,3]
[[1,2,3],[1,2],[1,3],[1],[2,3],[2],[3],[]]
  }
  ◊p{For those of you who don't know what a power set is, it's basically a set that comprises of all the possible subsets of given set. For a given set S, the power set is defined as}
  ◊p{\(P(S) \colon= \text{ T | Set(T), T $\subset$ S} \)}
  ◊p{including the empty set \(\emptyset\) and S itself. I started with ◊span[#:class "tilda"]{filterM} and drilled downwards from there:}
  ◊m-code-haskell{
filterM :: (Applicative m) => (a -> m Bool) -> [a] -> m [a]
filterM p
  = foldr (\ x -> liftA2 (\ flg -> if flg then (x:) else id) (p x)) (pure [])

liftA2 :: (a -> b -> c) -> f a -> f b -> f c
liftA2 f x = (<*>) (fmap f x)
  }
  ◊p{◊span[#:class "tilda"]{liftA2} lifts the lambda function ◊span[#:class "tilda"]{\flg -> if flg then (x:) else id} into ◊span[#:class "tilda"]{[True, False]}, which produces a list of functions. If we take a collection of integers as an example, then each call produces the following}
  ◊m-code-shell{
-- As an example, given [1, 2 ,3], calling liftA2 with the first value 1
-- liftA2 = (<*>) (fmap f x)
--        = (<*>) [(1:), id] :: Num a => [[a]] -> [[a]]
-- gives us a list of 2 actions that has the shape
-- [append x, do nothing]

-- Let's name each of these
◊string->symbol{lambda}> let g1 = (<*>) [(1:), id]
◊string->symbol{lambda}> let g2 = (<*>) [(2:), id]
◊string->symbol{lambda}> let g3 = (<*>) [(3:), id]

-- `pure` [] simply evaluates to [[]] here
-- If we fold the above from right to left
◊string->symbol{lambda}> g1 (g2 (g3 [[]]))
[[1,2,3],[1,2],[1,3],[1],[2,3],[2],[3],[]]
◊string->symbol{lambda}> g1 (g2 (g3 [[]])) == powerset [1,2,3]
True
  }
  ◊p{And that's how this magical expression really works underneath!}
}
◊m-back
◊m-mathjax
