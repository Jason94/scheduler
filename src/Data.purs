module App.Data where

import Effect.Console
import Effect.Unsafe
import Prelude
import Utils

import Data.Array (concatMap, delete, filter, index, length, mapMaybe, nubEq, snoc)
import Data.Array as Arr
import Data.Array.NonEmpty (NonEmptyArray, catMaybes, elemIndex, singleton, sortWith, toArray, (:))
import Data.Foldable (fold, foldMap, foldl, sum)
import Data.Map (Map, empty, insert, lookup, values)
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

frank :: Employee
frank = { name: "Frank", roles: Analyst : singleton Manager }

amanda :: Employee
amanda = { name: "Amanda", roles: singleton Manager }

donna :: Employee
donna = { name: "Donna", roles: singleton Analyst }

ellen :: Employee
ellen = { name: "Ellen", roles: singleton Analyst }

judy :: Employee
judy = { name: "Judy", roles: singleton Analyst }

travis :: Employee
travis = { name: "Travis", roles: Manager : singleton Programmer }

doug :: Employee
doug = { name: "Doug", roles: singleton Programmer }

jason :: Employee
jason = { name: "Jason", roles: singleton Programmer }

allEmployees :: NonEmptyArray Employee
allEmployees =
  amanda
  : frank
  : donna
  : ellen
  : judy
  : travis
  : doug
  : adam
  : ivan
  : singleton jason

-- | Check if x is in the array
contains :: forall a. Eq a => a -> Array a -> Boolean
contains x arr = isJust $ Arr.elemIndex x arr

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

-- | Two teams are 'the same' if their names are the same
sameTeam :: Team -> Team -> Boolean
sameTeam t1 t2 = t1.name == t2.name

sortTeams :: NonEmptyArray Team -> NonEmptyArray Team
sortTeams = sortWith (_.name)

emptyTeam :: String -> Map Role Int -> Team
emptyTeam name roleSlots =
  { name
  , roleSlots
  , roleAssignments: empty
  }

-- | Set the number of slots for a role in a team
setRoleSlots :: Role -> Int -> Team -> Team
setRoleSlots role n t@{ roleSlots } = t { roleSlots = newSlots }
  where
    newSlots :: Map Role Int
    newSlots = insert role n roleSlots

-- | Get the slots on a team for a role.
getRoleSlots :: Role -> Team -> Int
getRoleSlots role { roleSlots } = fromMaybe 0 $ lookup role roleSlots

-- | Get the employees assigned to a particular team of a particular role on a given day.
getAssigments :: Team -> Day -> Role -> Array Employee
getAssigments { roleAssignments } day role = fromMaybe [] $ do
  roles <- lookup day roleAssignments
  lookup role roles

-- | Get the days this employee is assigned to this role on this team
daysAssigned :: Team -> Role -> Employee -> Array Day
daysAssigned team role employee = filter
  (\d -> isJust do
      roleMap <- lookup d team.roleAssignments
      assignments <- lookup role roleMap
      Arr.elemIndex employee assignments)
  days

-- | Get the number of employees assigned to a particular day for a particular role.
numAssigned :: Day -> Role -> Team -> Int
numAssigned day role team = length $ getAssigments team day role

-- | Get the total number of employees that still need to be assigned for all roles on a day.
totalUnassigned :: Day -> Team -> Int
totalUnassigned day team =
  let totalAssigned = sum $ map (\r -> numAssigned day r team) allRoles
      totalNeeded   = sum $ values team.roleSlots
  in totalNeeded - totalAssigned

-- | Assign an employee to a team on a given day as a given role. Will not double assign.
assignEmployee :: Team -> Employee -> Day -> Role -> Team
assignEmployee team employee day role = team { roleAssignments = newAssignments }
  where
    newEmployees :: Array Employee
    newEmployees = nubEq $ snoc (getAssigments team day role) employee

    newAssignments :: Map Day (Map Role (Array Employee))
    newAssignments =
      let roleMap = insert role newEmployees $ fromMaybe empty (lookup day team.roleAssignments)
      in insert day roleMap team.roleAssignments

-- | Assign an employee to a team as a given role for all days. Will not double assign.
assignEmployeeAllDays :: Team -> Employee -> Role -> Team
assignEmployeeAllDays team employee role = foldl (\newT d -> assignEmployee newT employee d role) team days

-- | An employee can be assigned if they have the right role and aren't on the team.
canAssign :: Team -> Employee -> Day -> Role -> Boolean
canAssign team employee day role =
  hasRole role employee
    && not (contains employee (getAssigments team day role))

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

-- | Get the days an employee is unassigned on a team.
daysUnassigned :: Employee -> Team -> Array Day
daysUnassigned employee team = filter (not <<< isAssigned) days
  where
    isAssigned :: Day -> Boolean
    isAssigned day = contains employee $ concatMap (getAssigments team day) (toArray allRoles)

-- | Get the days an employee is unassigned on all teams.
daysUnassignedAll :: NonEmptyArray Team -> Employee -> Array Day
daysUnassignedAll teams employee = intersectAll $ map (daysUnassigned employee) teams

-- | Check if a team is understaffed on a given day for a particular role.
isUnderStaffed :: Team -> Role -> Day -> Boolean
isUnderStaffed team role day =
  let target = fromMaybe 0 $ lookup role team.roleSlots
      actual = numAssigned day role team
  in actual < target

-- | Get the days a team is understaffed for a particular role.
daysUnderStaffed :: Team -> Role -> Array Day
daysUnderStaffed team role = filter (isUnderStaffed team role) days

-- | Remove all employees assigned.
unassignAll :: Team -> Team
unassignAll = _ { roleAssignments = empty }

-- | Dummy team data
reviewEfiling :: Team
reviewEfiling =
  (setRoleSlots Analyst 2) <<<
  (setRoleSlots Programmer 3) <<<
  (setRoleSlots Manager 1) $
  emptyTeam "EFiling-Review" empty

support :: Team
support =
  (setRoleSlots Analyst 3) <<<
  (setRoleSlots Manager 1) $
  emptyTeam "Support" empty

sccaEfiling :: Team
sccaEfiling =
  (setRoleSlots Analyst 1) <<<
  (setRoleSlots Programmer 2) <<<
  (setRoleSlots Manager 1) $
  (emptyTeam "Scca-EFiling" empty)

allTeams :: NonEmptyArray Team
allTeams =
  reviewEfiling
  : support
  : singleton sccaEfiling
