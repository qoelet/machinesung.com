#lang pollen

◊h1{Terser import declarations}
◊h2{Generalizing via set operations}
◊m-article{
  ◊p{◊em{writing in progress, check back later.}}
  ◊p{I want to see if I can use it to generalize some operations with my prior explorations with the ◊a[#:href "/scribbles/ghc-api.html"]{GHC API}; expressing imported names as sets}
  ◊p{The task at hand starts with identifying imports declarations we can "compress" ◊string->symbol{mdash} a list of exports from module ◊code{A}, and some adjacent import declaration ◊code{A.B (x)} and determine if ◊code{A (x)} is feasible. We can start by thinking of the possible outcomes in terms of sets:}
  ◊figure[#:class "image" #:style "width: 75%; padding: 1rem;"]{
    ◊img[#:src "/assets/images/imports-as-venn-diagrams.png"]{}
  }
  ◊p{We could start by modeling declarations as a standalone sets, and locate a handful of operations for our aims:}
  ◊m-code-shell{
◊string->symbol{lambda}> :m +Data.Set
◊string->symbol{lambda}> let mExports = fromList ["foo", "bar", "baz"]
◊string->symbol{lambda}> let i1 = fromList ["foo", "qux"]
◊string->symbol{lambda}> let i2 = fromList ["qux"]
◊string->symbol{lambda}> let i3 = fromList ["foo", "baz"]
◊string->symbol{lambda}> union (intersection i1 mExports) (difference i1 mExports) == i1
True
◊string->symbol{lambda}> disjoint i2 mExports
True
◊string->symbol{lambda}> isProperSubsetOf i3 mExports
True
  }
  ◊p{Let's translate that to a datatype we can pattern match on:}
  ◊m-code-haskell{
  data CanMerge a
    = No
    | Partial (a, a)
    | Full a

  canMerge :: Ord a => Set a -> Set a -> CanMerge (Set a)
  canMerge xs ys
    | disjoint xs ys = No
    | isProperSubsetOf xs ys = Full xs
    | otherwise = Partial (difference xs ys, intersection xs ys)
  }
  ◊p{To tie this back to GHC API, for any 2 adjacent import declarations, we ask for the former's exports, and the latter, and determine if we can merge any of the imports to to former. We can then take the list of ◊code{CanMerge (Set Name)} to rewrite the import declarations.}
  ◊m-code-haskell{
  mergeableImports
    :: [LImportDecl GhcRn]
    -> [(Module, [ImportedBy])]
    -> [CanMerge (Set Name)]
  mergeableImports [i, j] exports
    = canMerge (getImportNames j) (getExportNames i j)
    : mergeableImports [] all_imports
  ...

  rewriteImport
    :: (Anns, Located (HsModule GhcPs))
    -> CanMerge (Set.Set Name)
    -> IO (Anns, Located (HsModule GhcPs))
  rewriteImport (anns, ast) = \case
    No -> return (anns, ast)
    Full rewrites -> do
      ...
    Partial (keep, rewrites) -> do
      ...
  }
  ◊p{Within source plugins, we can play out the entire story from the given ◊code{TcGblEnv}:}
  ◊m-code-haskell{
  testMergeImportDecls _ _ tcEnv = do
    runParser modulePath fileContents >>= \case
      Left () -> pure ()
      Right (anns, ast) -> do
      let canMergeList = mergeableImports user_imports all_imports
      (anns', ast') <- foldM rewriteImport (anns, ast) canMergeList
      print (exactPrint ast', anns')
  }
  ◊m-code-shell{
[1 of 1] Compiling Main             ( Main.hs, Main.o ) [Plugin forced recompilation]

Original source:

  module Main where

  import System.Environment (setEnv)
  import System.Environment.Blank (getArgs, getEnvDefault)

Modified source:

  module Main where

  import System.Environment (setEnv,getArgs)
  import System.Environment.Blank (getEnvDefault)

Linking Main ...
  }
  ◊p{◊em{Note: Still rewriting the actual transforms, but getting close. To be continued... }}
}
◊m-back
