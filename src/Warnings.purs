module Warnings where

import App.Data
import Prelude

import Data.Array (catMaybes, concatMap, mapMaybe)
import Data.Array.NonEmpty (NonEmptyArray, singleton, toArray)
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
        , severity: High
        }

underStaffed :: WarningMachine
underStaffed teams _ roles = catMaybes do
  team <- toArray teams
  role <- roles
  pure $ forTeamRole team role
    where
      makeWarning :: Team -> Role -> Array Day -> Maybe Warning
      makeWarning team role = case _ of
        [] -> Nothing
        theDays -> Just $
          { message: (show role) <> "s on "
            <> team.name
            <> " are understaffed on "
            <> (joinWith ", " $ map show theDays)
          , severity: Medium
          }

      forTeamRole :: Team -> Role -> Maybe Warning
      forTeamRole team role = makeWarning team role $ daysUnderStaffed team role

compileAllWarnings :: NonEmptyArray Team -> Array Employee -> Array Role -> Array Warning
compileAllWarnings = compileWarnings $ [notAssigned, underStaffed]
