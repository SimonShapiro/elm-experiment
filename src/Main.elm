module Main exposing (..)
import Browser
import Html exposing (Html, button, div, text, input)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Html.Events exposing (onInput)
import String exposing (toFloat)


type alias Model = 
  { 
    parameters: RecipeForm,
    recipe: Recipe
  }
type alias RecipeParameters = 
  { totalFlour: Int
  , hydration: Float
  , starterPercent: Float
  , saltPercent: Float
  }

type alias RecipeForm =
  { totalFlour: String
  , hydration: String
  , starterPercent: String
  , saltPercent: String
  }

type alias Recipe = 
  { recipeFlour: Int
  , recipeWater: Int
  , starterAmount: Int
  , saltAmount: Int
  , totalDoughWeight: Int
  }

calculateRecipe: RecipeParameters -> Recipe
calculateRecipe parms =
  let 
    starterAmount = round (Basics.toFloat parms.totalFlour * parms.starterPercent)
    starterWater = starterAmount // 2
    starterFlour = starterWater
    totalWater = round (Basics.toFloat parms.totalFlour * parms.hydration)
    recipeFlour = parms.totalFlour - starterFlour
    recipeWater = totalWater - starterWater
    saltAmount = round (Basics.toFloat parms.totalFlour * parms.saltPercent)
    totalDoughWeight = (recipeFlour + recipeWater + starterAmount + saltAmount)
  in
    { recipeFlour = recipeFlour
    , recipeWater = recipeWater
    , starterAmount = starterAmount 
    , saltAmount = saltAmount
    , totalDoughWeight = totalDoughWeight
    }

initParms: RecipeForm
initParms =  { totalFlour = String.fromInt 500
        , hydration = String.fromFloat 0.7
        , starterPercent = String.fromFloat 0.2
        , saltPercent = String.fromFloat 0.02
        }

convertRecipeForm: RecipeForm -> RecipeParameters
convertRecipeForm form =
  { totalFlour = Maybe.withDefault 0 (String.toInt form.totalFlour)
  , hydration = Maybe.withDefault 0.0 (String.toFloat form.hydration)
  , starterPercent = Maybe.withDefault 0.0 (String.toFloat form.starterPercent)
  , saltPercent = Maybe.withDefault 0.0 (String.toFloat form.saltPercent)
  }

init:  Model
init =  { parameters = initParms
        , recipe = convertRecipeForm initParms
          |> calculateRecipe 
        }

main: Program () Model Msg
main =
  Browser.sandbox { init = init, update = update, view = view }

type Msg = 
      ChangeTotalFlour String
      | ChangeHydration String
      | ChangeStarter String
    
update: Msg->Model->Model
update msg model =
  case msg of
    ChangeTotalFlour newFlour -> 
      let
        parameters = model.parameters |> \p -> {p | totalFlour = newFlour}
      in
        {  parameters = parameters
        , recipe = convertRecipeForm parameters
                          |> calculateRecipe 
        }

    ChangeHydration newHydration ->
      let 
        parms = model.parameters
        parameters = {parms | hydration = newHydration}
        recipe = calculateRecipe (convertRecipeForm parameters)
      in
        { parameters = parameters , recipe = recipe} 

    ChangeStarter newStarter ->
      let 
        parms = model.parameters
        parameters = {parms | starterPercent = newStarter}
        recipe = calculateRecipe (convertRecipeForm parameters)
      in
        { parameters = parameters , recipe = recipe} 

-- class="w3-panel w3-white w3-card w3-display-container"
view: Model->Html Msg
view model =
  div [class "w3-container w3-content"]
      [ 
        div [class "w3-panel w3-white w3-card w3-display-container", id "parameters"][ 
          div [][
          text "Total Flour "
          , input [placeholder "Total Flour", value model.parameters.totalFlour, onInput ChangeTotalFlour][]
        ]
        ,  div [][
          text "Hydration "
          , input [placeholder "Hydration", value model.parameters.hydration, onInput ChangeHydration][]
        ]
        ,  div [][
          text "Percent of starter "
          , input [placeholder "Percent of starter", value model.parameters.starterPercent, onInput ChangeStarter][]
        ]
        ]
        , div [class "w3-panel w3-white w3-card w3-display-container", id "parameters"][ 
          div [][
            text "Recipe Flour "
            , text (String.fromInt model.recipe.recipeFlour)
          ]
          ,div [][
              text "Recipe Water "
            , text (String.fromInt model.recipe.recipeWater)
          ]
          ,div [][
              text "Starter "
            ,           text (String.fromInt model.recipe.starterAmount)
          ]
          ,div [][
              text "Salt "
            , text (String.fromInt model.recipe.saltAmount)
          ]
          ,div [][
              text "Total Dough Weight "
            , text (String.fromInt model.recipe.totalDoughWeight)
          ]
        ]
      ]
