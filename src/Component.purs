module Component where

import App.Data
import Prelude

import Data.Array (concatMap)
import Data.Array.NonEmpty (NonEmptyArray, head, reverse, singleton, toArray, (:))
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
data Query a = Select Employee a

type State =
  { selected :: Employee
  }

-- | Select an employee to be inserted into the schedule
selectEmployee :: Employee -> State -> State
selectEmployee employee = _ { selected = employee }

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

teamDisplays :: forall p i. State -> Array (HH.HTML p i)
teamDisplays state = concatMap teamDisplay (toArray allTeams)
  where
    teamHeader :: Team -> HH.HTML p i
    teamHeader { name } = HH.span
                            [ css "schedule__team-label" ]
                            [ HH.text name ]

    teamCell :: Team -> String -> HH.HTML p i
    teamCell team day = HH.span
                          [ css "schedule__team-cell" ]
                          [ HH.text team.name ]

    teamDisplay :: Team -> Array (HH.HTML p i)
    teamDisplay team = [ teamHeader team ] <> (map (teamCell team) days)

scheduleDisplay :: forall p i. State -> HH.HTML p i
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
