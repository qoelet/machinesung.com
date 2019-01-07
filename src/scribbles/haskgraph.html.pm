#lang pollen

◊h1{More graph theory}
◊h2{Computing with Alga}
◊m-article{
  ◊p{◊em{writing in progress, check back later.}}
  ◊p{Mirroring the code examples from ◊a[#:href "/scribbles/rackgraph.html"]{Basic graph theory} in Haskell...}
  ◊m-code-haskell{
import           Algebra.Graph

trivialAndEmpty :: Graph Char
trivialAndEmpty = vertex 'a'

nonTrivial :: Graph Char
nonTrivial = edge 'a' 'b'

denserGraph :: Graph Char
denserGraph
  = edges [ ('a', 'b'), ('b', 'c'), ('a', 'c') ]
  `overlay` vertex 'd'

test :: IO ()
test = do
  print $ edgeCount trivialAndEmpty == 0
  print $ vertexCount trivialAndEmpty == 1
  print $ hasEdge 'a' 'b' nonTrivial
  print $ vertexList denserGraph == ['a', 'b', 'c', 'd']
  print $ edgeList denserGraph == [('a','b'),('a','c'),('b','c')]
  print $ (path "abc" `overlay` path "ac") `overlay` vertex 'd' == denserGraph
  }
  ◊p{◊em{To be continued...}}
}
◊m-back
