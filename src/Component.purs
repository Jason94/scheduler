module Component where

import App.Data
import Prelude

import Data.Array.NonEmpty (NonEmptyArray, head, toArray)
import Data.Maybe (Maybe(..))
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
    [ HH.h1_ [ HH.text "Employees" ]
    , HH.div_ $ map roleDisplay (toArray allRoles)
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
      HH.fieldset_ $
        [ HH.legend_ [ HH.text $ show role ] ] <>
        map employeeButton (employeesForRole role)

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
    HH.div_
      [ employeesDisplay state
      ]

  eval :: Query ~> H.ComponentDSL State Query Void m
  eval = case _ of
    Select employee next -> do
      _ <- H.modify_ $ selectEmployee employee
      pure next
