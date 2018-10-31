#lang pollen

◊h1{Source modifications}
◊h2{via GHC API and ghc-exactprint}
◊m-article{
  ◊p{◊em{writing in progress, this is part of my curiosity towards the GHC API, check back later.}}
  ◊p{The general idea of modifying a given source file can be demonstrated below}
  ◊m-code-haskell{
compressImports :: FilePath -> ByteString -> TcGblEnv -> IO ()
compressImports modulePath fileContents tcEnv =
  ...
          rewriteImports = do
            -- generate a SrcSpan (we need this for the import element we're adding)
            loc1 <- uniqueSrcSpanT
            let [L locImp imp, imp2] = hsmodImports hsMod
                Just (_, L locHiding ns) = ideclHiding imp
                -- constructing an import from a `Name`
                n1 = L loc1 (mkVarUnqual ((occNameFS . occName) addN))
                n1' = L loc1 (IEVar noExt (L loc1 (IEName n1)))
                impModified = imp { ideclHiding = Just (True, L locHiding (ns ++ [n1'])) }
                hsMod' = hsMod { hsmodImports = [L locImp impModified] }
            -- transformations from `ghc-exactprint`
            addSimpleAnnT n1
              (DP (0, 0))
              [ (G AnnVal, DP (0, 0)) ]
            addTrailingCommaT (last ns)
            return (L astLoc hsMod')
          (ast', (anns', _), _) = runTransform anns rewriteImports
      putStrLn "Original source:"
      print (exactPrint ast anns)
      putStrLn "Modified source:"
      print (exactPrint ast' anns')
  }
  ◊p{Running the above as a source plugin... (modified the actual output for readability)}
  ◊m-code-shell{
[1 of 1] Compiling Main             ( Main.hs, Main.o ) [Plugin forced recompilation]
Original source:
  module Main where

  import System.FilePath (isValid)
  import System.FilePath.Posix (isAbsolute)

Modified source:
  module Main where

  import System.FilePath (isValid,isAbsolute)
  }
  ◊p{Another example, where we remove any trailing commas from import declarations. This was motivated by another issue from ◊a[#:href "https://github.com/kowainik/smuggler"]{smuggler}:}
  ◊m-code-haskell{
removeTrailingCommas :: Anns -> Anns
removeTrailingCommas
  = Map.fromList . concatMap removeIfImportDecl
  . groupBy withinSrcSpan . Map.toList
  where
    removeIfImportDecl :: [(AnnKey, Annotation)] -> [(AnnKey, Annotation)]
    removeIfImportDecl gAnns
      | any isImportDecl gAnns = removeTrailingComma gAnns
      | otherwise = gAnns

    removeTrailingComma :: [(AnnKey, Annotation)] -> [(AnnKey, Annotation)]
    removeTrailingComma [] = []
    removeTrailingComma [x, (annKey, ann), z]
      = [x, (annKey, ann { annsDP = filter (not . isTrailingComma) (annsDP ann) }), z]
    removeTrailingComma (x : xs) = x : removeTrailingComma xs

    isImportDecl :: (AnnKey, Annotation) -> Bool
    isImportDecl (AnnKey _ (CN "ImportDecl"), _) = True
    isImportDecl _ = False

    isTrailingComma :: (KeywordId, DeltaPos) -> Bool
    isTrailingComma (G GHC.AnnComma, _) = True
    isTrailingComma _ = False

    withinSrcSpan :: (AnnKey, Annotation) -> (AnnKey, Annotation) -> Bool
    withinSrcSpan (AnnKey (RealSrcSpan x) _, _) (AnnKey (RealSrcSpan y) _, _)
      = srcSpanStartLine x == srcSpanStartLine y
      || srcSpanEndLine x == srcSpanEndLine y
    withinSrcSpan _ _ = True
  }
  ◊p{This results in the following:}
  ◊m-code-shell{
Original source:

  import Data.Bool (Bool (False,))

Modified source:

  import Data.Bool (Bool (False))
  }
}
◊m-back
