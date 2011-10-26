module MaybeCake where

import Data.Maybe
import Control.Applicative
import Control.Monad

data Coup = Coup {
  inspectCoup :: Maybe Chook
}

data Chook = Chook {
  layEgg :: Maybe Egg
}

data Egg = Egg
  deriving Show

data Chocolate = Chocolate
  deriving Show

data Cocoa = Cocoa
  deriving Show

data Flour = Flour
  deriving Show

data Fridge = Fridge {
  fridgeChocolate :: Maybe Chocolate,
  fridgeEgg :: Maybe Egg
}

data Pantry = Pantry {
  pantryChocolate :: Maybe Chocolate,
  pantryCocoa :: Maybe Cocoa,
  pantryFlour :: Maybe Flour
}

data Bakery = Bakery {
  bakeryCake :: Maybe Cake
}

data Cake =
    MudCake Egg Chocolate Flour
  | FlourlessCake Egg Chocolate Cocoa
  | BakeryCake
  deriving Show


-- define this
bakeMeACake :: Coup -> Fridge -> Pantry -> Bakery -> Maybe Cake
bakeMeACake coup fridge pantry bakery =
  undefined
