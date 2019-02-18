module App.Data where

import Prelude

import Data.Array (filter)
import Data.Array.NonEmpty (NonEmptyArray, elemIndex, singleton, toArray, (:))
import Data.Foldable (foldl)
import Data.Map (Map, empty, insert, lookup)
import Data.Maybe (fromMaybe, isJust)
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

newtype EmployeeName = EmployeeName String
derive instance newtypeEmployeeName :: Newtype EmployeeName _

type Employee =
  { name :: String
  , roles :: NonEmptyArray Role
  }

allEmployees :: NonEmptyArray Employee
allEmployees =
  { name: "Amanda", roles: singleton Manager }
  : { name: "Frank", roles: Analyst : singleton Manager }
  : { name: "Donna", roles: singleton Analyst }
  : { name: "Ellen", roles: singleton Analyst }
  : { name: "Judy", roles: singleton Analyst }
  : { name: "Travis", roles: Manager : singleton Programmer }
  : { name: "Doug", roles: singleton Programmer }
  : { name: "Adam", roles: singleton Programmer }
  : { name: "Ivan", roles: singleton Programmer }
  : singleton { name: "Jason", roles: singleton Programmer }

-- | Check if an employee has a given role
hasRole :: Role -> Employee -> Boolean
hasRole role { roles } = isJust $ elemIndex role roles

-- | Given a list of employees, grab each employee that is every role
groupByRole :: NonEmptyArray Employee -> Map Role (Array Employee)
groupByRole employees = foldl buildForRole empty allRoles
  where
    employeesFor :: Role -> Array Employee
    employeesFor role = filter (hasRole role) (toArray employees)

    buildForRole :: Map Role (Array Employee) -> Role -> Map Role (Array Employee)
    buildForRole m role = insert role (employeesFor role) m

employeesByRole :: Map Role (Array Employee)
employeesByRole = groupByRole allEmployees

employeesForRole :: Role -> Array Employee
employeesForRole role = fromMaybe [] $ lookup role employeesByRole

type Team =
  { name :: String
  , roleSlots :: Map Role Int
  , roleAssignments :: Map Role (Array Employee)
  }

emptyTeam :: String -> Map Role Int -> Team
emptyTeam name roleSlots =
  { name
  , roleSlots
  , roleAssignments: empty
  }

allTeams :: NonEmptyArray Team
allTeams = singleton (emptyTeam "scca efiling" empty)
