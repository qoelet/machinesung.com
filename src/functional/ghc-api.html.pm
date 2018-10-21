#lang pollen

◊h1{Source modifications}
◊h2{via GHC API and ghc-exactprint}
◊m-article{
  ◊p{◊em{writing in progress, this is part of my curiosity towards the GHC API, check back later.}}
  ◊p{The general idea of modifying a given source file}
  ◊m-code{
modifyImports :: FilePath -> ByteString -> TcGblEnv -> IO ()
modifyImports modulePath fileContents tcEnv =
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
  ◊m-code{
[1 of 1] Compiling Main             ( Main.hs, Main.o ) [Plugin forced recompilation]
Original source:
  module Main where

  import System.FilePath (isValid)
  import System.FilePath.Posix (isAbsolute)

Modified source:
  module Main where

  import System.FilePath (isValid,isAbsolute)
  }
}
◊m-back
