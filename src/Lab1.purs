module Lab1
  ( length
  , null
  , singleton
  , snoc
  , test
  )
  where

import Data.Array as Array
import Data.Foldable (foldr)
import Effect (Effect)
import Effect.Console (log)
import Prelude

-- Task 1: Singleton function
singleton :: forall a. a -> Array a
singleton x = [x]

-- Task 2: Null function
null :: forall a. Array a -> Boolean
null arr = length arr == 0

-- Task 3: Snoc function
snoc :: forall a. Array a -> a -> Array a
snoc arr x = Array.snoc arr x

-- Task 4: Length function
length :: forall a. Array a -> Int
length arr = foldr (\_ acc -> acc + 1) 0 arr

test :: Effect Unit
test = do 
  log $ "Singleton"
  log $ show (singleton "item")
  log $ "Null empty"
  log $ show (null [])
  log $ "Null nonEmpty"
  log $ show (null ["item"])
  log $ "Snoc" 
  log $ show (snoc ["item"] "item2")
  log $ "Length"
  log $ show (length ["item", "item2", "item3"])
