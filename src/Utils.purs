module Utils where

import Prelude

import Data.Array (elemIndex, foldl, intersect)
import Data.Array.NonEmpty (NonEmptyArray, head, toArray, updateAt)
import Data.Array.NonEmpty as NEArr
import Data.Maybe (Maybe(..), fromMaybe, isJust)

toMaybe :: forall a. Boolean -> a -> Maybe a
toMaybe true a  = Just a
toMaybe false _  = Nothing

contains :: forall a. Eq a => a -> Array a -> Boolean
contains a arr = isJust $ elemIndex a arr

intersectAll :: forall a. Eq a => NonEmptyArray (Array a) -> Array a
intersectAll arrs = foldl intersect (head arrs) (toArray arrs)

-- | Update the first occurance of a
updateEq :: forall a. Eq a => a -> a -> NonEmptyArray a -> NonEmptyArray a
updateEq old new arr = case (NEArr.elemIndex old arr) of
  Nothing -> arr
  Just i  -> fromMaybe arr (updateAt i new arr)
