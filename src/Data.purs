module App.Data where

import Prelude

import Data.Array (delete, filter, nubEq)
import Data.Array.NonEmpty (NonEmptyArray, elemIndex, singleton, toArray, (:))
import Data.Foldable (foldl)
import Data.Map (Map, empty, insert, lookup)
import Data.Maybe (fromMaybe, isJust)

data Day = Monday | Tuesday | Wednesday | Thursday | Friday
derive instance eqDay :: Eq Day
derive instance ordDay :: Ord Day
instance showDay :: Show Day where
  show Monday = "Monday"
  show Tuesday = "Tuesday"
  show Wednesday = "Wednesday"
  show Thursday = "Thursday"
  show Friday = "Friday"

days :: Array Day
days = [ Monday, Tuesday, Wednesday, Thursday, Friday ]

data Role = Manager | Programmer | Analyst
derive instance eqRole :: Eq Role
derive instance ordRole :: Ord Role
instance showRole :: Show Role where
  show Manager = "Manager"
  show Programmer = "Programmer"
  show Analyst = "Analyst"

allRoles :: NonEmptyArray Role
allRoles = Manager : Programmer : singleton Analyst

type Employee =
  { name :: String
  , roles :: NonEmptyArray Role
  }

-- | Dummy employee data
adam :: Employee
adam = { name: "Adam", roles: singleton Programmer }

ivan :: Employee
ivan = { name: "Ivan", roles: singleton Programmer }

allEmployees :: NonEmptyArray Employee
allEmployees =
  { name: "Amanda", roles: singleton Manager }
  : { name: "Frank", roles: Analyst : singleton Manager }
  : { name: "Donna", roles: singleton Analyst }
  : { name: "Ellen", roles: singleton Analyst }
  : { name: "Judy", roles: singleton Analyst }
  : { name: "Travis", roles: Manager : singleton Programmer }
  : { name: "Doug", roles: singleton Programmer }
  : adam
  : ivan
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
  , roleAssignments :: Map Day (Map Role (Array Employee))
  }

emptyTeam :: String -> Map Role Int -> Team
emptyTeam name roleSlots =
  { name
  , roleSlots
  , roleAssignments: empty
  }

-- | Get the employees assigned to a particular team of a particular role on a given day.
getAssigments :: Team -> Day -> Role -> Array Employee
getAssigments { roleAssignments } day role = fromMaybe [] $ do
  roles <- lookup day roleAssignments
  lookup role roles

-- | Assign an employee to a team on a given day as a given role. Will not double assign.
assignEmployee :: Team -> Employee -> Day -> Role -> Team
assignEmployee team employee day role = team { roleAssignments = newAssignments }
  where
    newEmployees :: Array Employee
    newEmployees = nubEq $ [employee] <> (getAssigments team day role)

    newAssignments :: Map Day (Map Role (Array Employee))
    newAssignments =
      let roleMap = insert role newEmployees $ fromMaybe empty (lookup day team.roleAssignments)
      in insert day roleMap team.roleAssignments

-- | Remove an employee from the schedule. No-op if not assigned as such.
unassignEmployee :: Team -> Employee -> Day -> Role -> Team
unassignEmployee team employee day role = team { roleAssignments = newAssignments }
  where
    newEmployees :: Array Employee
    newEmployees = delete employee (getAssigments team day role)

    newAssignments :: Map Day (Map Role (Array Employee))
    newAssignments =
      let roleMap = insert role newEmployees $ fromMaybe empty (lookup day team.roleAssignments)
      in insert day roleMap team.roleAssignments

-- | Dummy team data
reviewEfiling :: Team
reviewEfiling =
  (\t -> (assignEmployee t ivan Monday Programmer)) <<<
  (\t -> (assignEmployee t ivan Tuesday Programmer)) $
  (assignEmployee (emptyTeam "efiling review" empty) adam Monday Programmer )

allTeams :: NonEmptyArray Team
allTeams =
  reviewEfiling
  : singleton (emptyTeam "scca efiling" empty)
