#lang pollen

◊h1{Sums of products (and carbs)}
◊h2{Fun with datatypes and representations}
◊m-article{
  ◊p{If we take any data, there's many ways we can represent that information - bullet points on a notebook, in a PostgreSQL table, a JSON object  and so on. There's going to be all sorts of syntactic wrappings that make things look different from one representation to another, but underneath it's pretty much, well, data. So if there is a path to convert between representations, then we can say they are the same. (◊em{ie. up to isomorphism})}
  ◊p{In Haskell, lists are ◊em{homogeneous}, that is, they can only be instantiated to some given type. ◊span[#:class "tilda"]{["foo", "bar"] :: [String]} is ok, but ◊span[#:class "tilda"]{["foo", 3]} is not. We can however represent the latter as a datatype,}
  ◊m-code-haskell{
datatype Foo = MkFoo String Int

foo :: Foo
foo = MkFoo "foo" 3
  }
  ◊p{But both encode the same information! (or at least that was what preoccupied my thoughts over dinner) In other words, there's a path from one to the other. Some searching on Hackage brought up the ◊a[#:href "http://hackage.haskell.org/package/generics-sop"]{generics-sop} library, which describes an interesting way of representing datatypes: ◊strong{"the choice between constructors is represented using an n-ary sum, and the arguments of each constructor are represented using an n-ary product"}}
  ◊p{That sounded like a nice after dinner scribble, so I decided to explore it. Some preliminaries,}
  ◊m-code-haskell{
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}

import qualified GHC.Generics as GHC
import           Generics.SOP
  }
  ◊p{Next, let's think about some trivial data to work with. If we want to encode some information about our meals today, we could do it as ◊span[#:class "tilda"]{[food, drink]}, so today I might feel like ◊span[#:class "tilda"]{["Rice", "Water"]}. Putting that into some trivial datatype definitions,}
  ◊m-code-haskell{
-- Sum
data Drink
  = Soda
  | Water
  deriving (Read, Show, GHC.Generic)

-- Sum
data Chow
  = Rice
  | Bread
  | Noodles
  deriving (Read, Show, GHC.Generic)

-- Sum of products
data Meal
  = Upsize Chow Drink
  | Normal Chow
  | Diet
  deriving (Show, GHC.Generic)

instance Generic Drink
instance Generic Meal
  }
  ◊p{So we can think of the choice between constructors as sums, and they are represented via ◊em{Peano numbers}, and the arguments of each constructor via a list like structure. We can then write down the representation of this in the type signature: ◊span[#:class "tilda"]{Rep Meal ~ SOP I '[ '[ Chow, Drink ], '[Chow], '[] ]},}
  ◊m-code-haskell{
chooseMeal :: [String] -> Rep Meal
chooseMeal [x, y] = SOP (Z (I (read x) :* I (read y) :* Nil))
chooseMeal [x] = SOP (S (Z (I (read x) :* Nil)))
chooseMeal [] = SOP (S (S (Z Nil)))
chooseMeal _ = error "Fail to parse input"

-- Making use of that fancy TypeApplications extension
mealFromOpt :: [String] -> Meal
mealFromOpt s = to @Meal (chooseMeal s)
  }
  ◊p{And we convert back to a list of strings. Here, ◊span[#:class "tilda"]{SOP I (Code Meal) ◊string->symbol{cong} datatype Meal} and we can switch this into a homogeneous structure (a list of strings) with the constant functor `K` (◊span[#:class "tilda"]{SOP (K String) (Code Meal) ◊string->symbol{cong} [String]})}
  ◊m-code-haskell{
hmeal :: Rep Meal
hmeal = SOP (Z (I Bread :* I Soda :* Nil))

printMeal :: Rep Meal -> [String]
printMeal = hcollapse . hcmap (Proxy :: Proxy Show) (mapIK show)
  }
  ◊p{In ◊span[#:class "tilda"]{ghci},}
  ◊m-code-shell{
◊string->symbol{lambda}> mealFromList ["Rice", "Soda"]
Upsize Rice Soda
◊string->symbol{lambda}> mealFromList ["Bread"]
Normal Bread
◊string->symbol{lambda}> mealFromList []
Diet
◊string->symbol{lambda}> printMeal hmeal
["Bread","Soda"]

-- back and forth
◊string->symbol{lambda}> printMeal . chooseMeal $ ["Bread"]
["Bread"]
◊string->symbol{lambda}> mealFromList . printMeal . from $ Normal Bread
Normal Bread
  }
  ◊p{That was fun! The library is pretty huge - there are some examples there but I suspect I'll need to take some time to mow down the concepts.}
}
◊m-back
◊m-mathjax
