module Component where

import App.Data
import Prelude

import Data.Array (concatMap, cons, delete)
import Data.Array.NonEmpty (NonEmptyArray, head, reverse, singleton, toArray, (:))
import Data.Map (lookup)
import Data.Maybe (Maybe(..))
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
data Query a = Select Employee a | Assign Team Employee Day Role a | Unassign Team Employee Day Role a

type State =
  { selected :: Employee
  , teams :: Array Team
  }

-- | Select an employee to be inserted into the schedule
selectEmployee :: Employee -> State -> State
selectEmployee employee = _ { selected = employee }

unassignEmployeeFromState :: Team -> Employee -> Day -> Role -> State -> State
unassignEmployeeFromState t e d r s@{ teams } = s { teams = newTeams }
  where
    newTeams :: Array Team
    newTeams = cons (unassignEmployee t e d r) (delete t teams)

assignEmployeeFromState:: Team -> Employee -> Day -> Role -> State -> State
assignEmployeeFromState t e d r s@{ teams } =
  if canAssign t e d r
  then s { teams = newTeams }
  else s
    where
      newTeams :: Array Team
      newTeams = cons (assignEmployee t e d r) (delete t teams)

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

teamDisplays :: State -> Array (H.ComponentHTML Query)
teamDisplays state = concatMap teamDisplay (sortTeams state.teams)
  where
    teamHeader :: Team -> H.ComponentHTML Query
    teamHeader { name } = HH.span
                            [ css "schedule__team-label" ]
                            [ HH.text name ]

    teamCell :: Team -> Day -> H.ComponentHTML Query
    teamCell team day = HH.span
                          [ css "schedule__team-cell" ]
                          (toArray $ map (roleRow day) allRoles)
      where
        employeeButton :: Role -> Employee -> H.ComponentHTML Query
        employeeButton role e@{ name } =
          HH.button
            [ css "schedule__button"
            , HE.onClick (HE.input_ $ Unassign team e day role)
            ]
            [ HH.text name ]

        addButton :: Day -> Role -> H.ComponentHTML Query
        addButton day role =
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

        roleRow :: Day -> Role -> H.ComponentHTML Query
        roleRow day role =
          HH.div
            [ css "schedule__role-row" ] $
            [ roleLabel role ]
              <> (map (employeeButton role) $ getAssigments team day role)
              <> [ addButton day role ]

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
      , teams: toArray allTeams
      }

    render :: State -> H.ComponentHTML Query
    render state =
      HH.div
        [ css "full" ]
        [ employeesDisplay state
        , scheduleDisplay state
        ]

    eval :: Query ~> H.ComponentDSL State Query Void m
    eval = case _ of
      Select employee next -> do
        _ <- H.modify_ $ selectEmployee employee
        pure next
      Assign team employee day role next -> do
        _ <- H.modify_ $ assignEmployeeFromState team employee day role
        pure next
      Unassign team employee day role next -> do
        _ <- H.modify_ $ unassignEmployeeFromState team employee day role
        pure next
