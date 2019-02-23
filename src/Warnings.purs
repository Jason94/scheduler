module Warnings where

import App.Data
import Prelude

import Data.Array (concatMap, mapMaybe)
import Data.Array.NonEmpty (NonEmptyArray, singleton)
import Data.Maybe (Maybe(..))
import Data.String (joinWith)
import Utils (toMaybe)

data Severity = Low | Medium | High
derive instance eqSeverity :: Eq Severity
derive instance ordSeverity :: Ord Severity
instance showSeverity :: Show Severity where
  show Low    = "Low"
  show Medium = "Medium"
  show High   = "High"

type Warning =
  { message :: String
  , severity :: Severity
  }

type WarningMachine =
  NonEmptyArray Team
  -> Array Employee
  -> Array Role
  -> Array Warning

compileWarnings :: Array WarningMachine -> NonEmptyArray Team -> Array Employee -> Array Role -> Array Warning
compileWarnings fs teams employees roles = concatMap (\f -> f teams employees roles)  fs

--- Warnings Data ---
notAssigned :: WarningMachine
notAssigned teams employees roles = mapMaybe forEmployee employees
  where
    forEmployee :: Employee -> Maybe Warning
    forEmployee employee = case daysUnassignedAll teams employee of
      []   -> Nothing
      days -> Just
        { message: employee.name <> " is not assigned on " <> (joinWith ", " $ map show days)
        , severity: Medium
        }

compileAllWarnings :: NonEmptyArray Team -> Array Employee -> Array Role -> Array Warning
compileAllWarnings = compileWarnings $ [notAssigned]
