#lang pollen

◊h1{Exception handling}
◊h2{Handling exceptions neatly via monad transformers}
◊m-article{
  ◊p{Taking a simple app that returns a webpage as an example, it's fairly common to add requirements and have some function soon look like the following:}
  ◊m-code-haskell{
myApp :: Request -> IO Response
myApp req = do
  foo <- action1
  case foo of
    Just bar -> do
      baz <- action2
      case baz of
        Right qux -> do
          -- ... etc
        Left err -> ...
    _ -> ...
  }
  ◊p{When I first started writing Haskell, I never really "felt" it was an issue - there's clarity in simple pattern matching and it appealed to my imperative habits to read something like the above from top to bottom aloud, reasoning about the expected behavior of the program. If it type checks and compile, I guess I'm good.}
  ◊p{But as I started to write longer programs in the IO monad, this started to get out of hand quickly as the nesting started a nice descend to the right. I had heard monad transformers would be a nice solution, and I had used ◊span[#:class= "tilda"]{MaybeT} once or twice to get some nice terse expressions going. So where do I begin?}
  ◊p{Most of my pattern matching were on values expressed in the `Maybe` or `Either`, and represented exception signaling when they were `Nothing` or `Left`, for example:}
  ◊m-code-haskell{
myApp req = do
  foo <- checkThatWeHaveADatabaseResult :: IO (Either String Result)
  case foo of
    Right r -> do
      ... -- do something with Result
      bar <- fetchSomethingElseFromDatabase :: IO (Maybe [Baz])
      case bar of
        Just xs -> ...
        _ -> ...
    Left err -> ... -- do something with message (log it, return some error page)
  }
  ◊p{The ◊a[#:href "http://hackage.haskell.org/package/transformers"]{transformers} package defines ◊code{ExceptT}, which we can use to add error handling to the IO (or any other) monad. I often run application handlers within the Reader monad, so this plays nicely towards the term "stack" that one might have come across in certain discussions. So we can now redefine the example as follows:}
  ◊m-code-haskell{
data MyException
  = UnableToGetResult
  | NothingOfThatSort

getResultOrFail :: ExceptT MyException IO Result
getResultOrFail = do
  res <- lift checkThatWeHaveADatabaseResult
  case res of
    Right r -> return r
    Left _ -> throwE UnableToGetResult

getSomethingOrFail :: ExceptT MyException IO [Baz]
getSomethingOrFail = do
  res <- lift fetchSomethingElseFromDatabase
  case res of
    Just xs -> return xs
    Nothing -> throwE NothingOfThatSort

-- pattern match on exceptions, return appropriate response to user
databaseFailureHandler UnableToGetResult = return $ errorRes status502 "Oops!"
databaseFailureHandler NothingOfThatSort = return $ errorRes status404 "Nope"

myApp req = do
  foo <- getResultOrFail
  ... -- do something with Result
  bar <- fetchSomethingElseFromDatabase :: IO (Maybe [Baz])
  ... -- do something with [Baz]
  }
  ◊p{That's definitely a lot more readable! While we're also looking at exceptions, I definitely recommend reading ◊a[#:href "https://wiki.haskell.org/Error_vs._Exception"]{Error vs Exception (Haskell Wiki)}.}
}
◊m-back
