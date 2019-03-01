module Component where

import App.Data
import Prelude
import Utils
import Warnings

import Data.Array (concatMap, cons, delete, foldl)
import Data.Array.NonEmpty (NonEmptyArray, head, reverse, singleton, toArray, (:))
import Data.Array.NonEmpty as NEArr
import Data.Map (keys, lookup)
import Data.Maybe (Maybe(..))
import Data.Set (toUnfoldable)
import Data.String (toLower)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

-- | I get annoyed writing `class_ $ ClassName "..."` over and over again. This small utility saves
-- | a few characters all over our HTML.
css :: forall r i. String -> HH.IProp ( class :: String | r ) i
css = HP.class_ <<< HH.ClassName

----       Types       ----
data Query a =
  Select Employee a
  | Assign Team Employee Day Role a
  | AssignAllDays Team Employee Role a
  | Unassign Team Employee Day Role a
  | UnassignAll a

type State =
  { selected :: Employee
  , teams :: NonEmptyArray Team
  }

-- | Select an employee to be inserted into the schedule
selectEmployee :: Employee -> State -> State
selectEmployee employee = _ { selected = employee }

unassignEmployeeFromState :: Team -> Employee -> Day -> Role -> State -> State
unassignEmployeeFromState t e d r s@{ teams } = s { teams = newTeams }
  where
    newTeams :: NonEmptyArray Team
    newTeams = updateEq t (unassignEmployee t e d r) teams

assignEmployeeFromState:: Team -> Employee -> Day -> Role -> State -> State
assignEmployeeFromState t e d r s@{ teams } =
  if canAssign t e d r
  then s { teams = newTeams }
  else s
    where
      newTeams :: NonEmptyArray Team
      newTeams = updateEq t (assignEmployee t e d r) teams

assignEmployeeAllDaysFromState :: Team -> Employee -> Role -> State -> State
assignEmployeeAllDaysFromState t e r s@{ teams } = s { teams = newTeams }
  where
    newTeams :: NonEmptyArray Team
    newTeams = updateEq t (assignEmployeeAllDays t e r) teams

unassignAllFromState :: State -> State
unassignAllFromState s@{ teams } = s { teams = map unassignAll teams }

---- Employees Display ----

employeesDisplay :: State -> H.ComponentHTML Query
employeesDisplay state =
  HH.div_
    [ HH.div
        [ css "selector" ]
        (map roleDisplay (toArray allRoles))
    ]
  where
    employeeButtonClass :: Employee -> State -> String
    employeeButtonClass employee { selected } =
      if employee == selected
      then "selector__button selector__button--selected"
      else "selector__button"

    employeeButton e@{ name } =
      HH.button
        [ css $ employeeButtonClass e state
        , HE.onClick (HE.input_ $ Select e)
        ]
        [ HH.text name ]

    roleDisplay :: Role -> H.ComponentHTML Query
    roleDisplay role =
      HH.fieldset
        [ css "selector__panel" ]
        ([ HH.legend_ [ HH.text $ show role ] ] <>
         map employeeButton (employeesForRole role))

---- Schedule Display  ----
dayDisplays :: forall p i. Array (HH.HTML p i)
dayDisplays = [ HH.span_ [] ] <> (map dayDisplay days)
  where
    dayDisplay :: Day -> HH.HTML p i
    dayDisplay day = HH.span
      [ css "schedule__day" ]
      [ HH.text $ show day ]

-- | Buttons that add the selected employee to all five days.
addFiveButtons :: State -> Team -> H.ComponentHTML Query
addFiveButtons state team =
  HH.div
    [ css "flex-column" ]
    (map addFiveButton (toUnfoldable $ keys team.roleSlots))
  where
    className :: Role -> String
    className role = "schedule__add margin-bottom-5px" <> if hasRole role state.selected
                                                          then ""
                                                          else " schedule__add--disabled"

    addFiveButton :: Role -> H.ComponentHTML Query
    addFiveButton role =
      HH.button
        [ css $ className role
        , HE.onClick (HE.input_ $ AssignAllDays team state.selected role)
        ]
        [ HH.text $ show role ]

teamDisplays :: State -> Array (H.ComponentHTML Query)
teamDisplays state = concatMap teamDisplay (toArray $ sortTeams state.teams)
  where
    teamHeader :: Team -> H.ComponentHTML Query
    teamHeader team@{ name } = HH.div_
      [ HH.span
          [ css "schedule__team-label" ]
          [ HH.text name ]
      , addFiveButtons state team
      ]

    teamCell :: Team -> Day -> H.ComponentHTML Query
    teamCell team day =
      HH.span
        [ css className ]
        (toArray $ map roleRow allRoles)
      where
        className :: String
        className = if totalUnassigned day team > 0
                    then "schedule__team-cell schedule__team-cell--unassigned"
                    else "schedule__team-cell"

        employeeButton :: Role -> Employee -> H.ComponentHTML Query
        employeeButton role e@{ name } =
          HH.button
            [ css "schedule__button"
            , HE.onClick (HE.input_ $ Unassign team e day role)
            ]
            [ HH.text name ]

        addButton :: Role -> H.ComponentHTML Query
        addButton role =
          let classes = if canAssign team state.selected day role
                        then "schedule__add"
                        else "schedule__add schedule__add--disabled"
          in HH.button
               [ css classes
               , HE.onClick (HE.input_ $ Assign team state.selected day role)
               ]
               [ HH.text "+" ]

        roleLabel :: Role -> H.ComponentHTML Query
        roleLabel role =
          HH.span
            [ css "schedule__row-label" ]
            [ HH.text $ (show role) <> ": " ]

        roleRow :: Role -> H.ComponentHTML Query
        roleRow role =
          HH.div
            [ css "schedule__role-row" ] $
            [ roleLabel role ]
              <> (map (employeeButton role) $ getAssigments team day role)
              <> [ addButton role ]

    teamDisplay :: Team -> Array (H.ComponentHTML Query)
    teamDisplay team = [ teamHeader team ] <> (map (teamCell team) days)

scheduleDisplay :: State -> H.ComponentHTML Query
scheduleDisplay state =
  HH.fieldset_
    [ HH.legend_ [ HH.text "Schedule" ]
    , HH.div
        [ css "schedule" ]
        $ dayDisplays <> (teamDisplays state)
    ]
----       Sidebar     ----
warningClassName :: Warning -> String
warningClassName { severity: Low } = "sidebar__warning sidebar__warning--low"
warningClassName { severity: Medium } = "sidebar__warning sidebar__warning--medium"
warningClassName { severity: High } = "sidebar__warning sidebar__warning--high"

warnings :: State -> H.ComponentHTML Query
warnings state =
  HH.ul_
    (map warningDisplay $ compileAllWarnings state.teams (toArray allEmployees) (toArray allRoles))
  where
    warningDisplay :: Warning -> H.ComponentHTML Query
    warningDisplay w@{ message } =
      HH.li
        [ css $ warningClassName w ]
        [ HH.text message ]

sidebar :: State -> H.ComponentHTML Query
sidebar state =
  HH.div
    [ css "sidebar" ]
    [ HH.button
        [ HE.onClick $ HE.input_ UnassignAll
        , css "selector__button"
        ]
        [ HH.text "Clear All" ]
    , HH.br_
    , warnings state
    ]

----   Main Component  ----
component :: forall m. NonEmptyArray Employee -> H.Component HH.HTML Query Unit Void m
component employees =
  H.component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where
    initialState :: State
    initialState =
      { selected: head employees
      , teams: allTeams
      }

    render :: State -> H.ComponentHTML Query
    render state =
      HH.div
        [ css "full flex-row" ]
        [ HH.div
            [ css "content" ]
            [ employeesDisplay state
            , scheduleDisplay state
            ]
        , sidebar state
        ]

    eval :: Query ~> H.ComponentDSL State Query Void m
    eval = case _ of
      Select employee next -> do
        _ <- H.modify_ $ selectEmployee employee
        pure next
      AssignAllDays team employee role next -> do
        _ <- H.modify_ $ assignEmployeeAllDaysFromState team employee role
        pure next
      Assign team employee day role next -> do
        _ <- H.modify_ $ assignEmployeeFromState team employee day role
        pure next
      Unassign team employee day role next -> do
        _ <- H.modify_ $ unassignEmployeeFromState team employee day role
        pure next
      UnassignAll next -> do
        _ <- H.modify_ unassignAllFromState
        pure next
