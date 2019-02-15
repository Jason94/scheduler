module App.Data where

import Prelude

import Data.Array (filter)
import Data.Array.NonEmpty (NonEmptyArray, elemIndex, singleton, toArray, (:))
import Data.Foldable (foldl)
import Data.Map (Map, empty, insert)
import Data.Maybe (isJust)
import Data.Newtype (class Newtype)

data Role = Manager | Programmer | Analyst
derive instance eqRole :: Eq Role
derive instance ordRole :: Ord Role
instance showRole :: Show Role where
  show Manager = "Manager"
  show Programmer = "Programmer"
  show Analyst = "Analyst"

allRoles :: NonEmptyArray Role
allRoles = Manager : Programmer : singleton Analyst

test :: Map Int Int
test = insert 0 0 (empty)

newtype EmployeeName = EmployeeName String
derive instance newtypeEmployeeName :: Newtype EmployeeName _

type Employee =
  { name :: String
  , roles :: NonEmptyArray Role
  }

-- | Check if an employee has a given role
hasRole :: Role -> Employee -> Boolean
hasRole role { roles } = isJust $ elemIndex role roles

-- | Given a list of employees, grab each employee that is every role
groupByRole :: NonEmptyArray Employee -> Map Role (Array Employee)
groupByRole employees = foldl buildForRole empty
  where
    employeesFor :: Role -> Array Employee
    employeesFor role = filter (hasRole role) (toArray employees)

    buildForRole :: Map Role (Array Employee) -> Role -> Map Role (Array Employee)
    buildForRole m role = insert role-- [] m--(employeesFor role) m

type Team =
  { name :: String
  , roleSlots :: Map Role Int
  , roleAssignments :: Map Role (Array Employee)
  }
