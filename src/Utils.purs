module Utils where

import Prelude

import Data.Array (elemIndex, foldl, intersect)
import Data.Array.NonEmpty (NonEmptyArray, findIndex, head, index, toArray, updateAt)
import Data.Array.NonEmpty as NEArr
import Data.Maybe (Maybe(..), fromMaybe, isJust)

toMaybe :: forall a. Boolean -> a -> Maybe a
toMaybe true a  = Just a
toMaybe false _  = Nothing

contains :: forall a. Eq a => a -> Array a -> Boolean
contains a arr = isJust $ elemIndex a arr

intersectAll :: forall a. Eq a => NonEmptyArray (Array a) -> Array a
intersectAll arrs = foldl intersect (head arrs) (toArray arrs)

-- | Update the first occurace where pred is true
updatePred :: forall a. (a -> Boolean) -> a -> NonEmptyArray a -> NonEmptyArray a
updatePred pred new arr = case (findIndex pred arr) of
  Nothing -> arr
  Just i -> fromMaybe arr (updateAt i new arr)

-- | Update the first occurance of a
updateEq :: forall a. Eq a => a -> a -> NonEmptyArray a -> NonEmptyArray a
updateEq old new arr = case (NEArr.elemIndex old arr) of
  Nothing -> arr
  Just i  -> fromMaybe arr (updateAt i new arr)

-- | Find the first instance where pred is true
findEq :: forall a. (a -> Boolean) -> NonEmptyArray a -> Maybe a
findEq f arr = do
  i <- findIndex f arr
  index arr i
