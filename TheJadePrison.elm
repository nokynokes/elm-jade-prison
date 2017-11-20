import Dict
import String
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
 -- Model
type alias PlayerInfo = Dict.Dict String (Maybe String)
type alias PlayerAttrs = Dict.Dict String Int
type alias Model = {
  playerInfo : PlayerInfo
  , playerAttrs : PlayerAttrs
}

emptyPlayerInfo : PlayerInfo
emptyPlayerInfo =
  Dict.fromList
    [("Name", Nothing)
    ,("Player", Nothing)
    ,("Caste", Nothing)
    ,("Concept", Nothing)
    ,("Anima", Nothing)
    ,("Ability", Nothing)
    ]

emptyPlayerAttrs : PlayerAttrs
emptyPlayerAttrs = List.map (\str -> (str, 1)) abilities2 |> Dict.fromList
-- Update

type Msg
   = EditPlayerInfo String String
   | EditPlayerAttrs Operation String

type Operation
   = Incr
   | Decr


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =  case msg of
  EditPlayerInfo attr val ->
    let newPlayerInfo =
      Dict.insert attr (if String.length val > 0 then (Just val) else Nothing) model.playerInfo
    in
      {model | playerInfo = newPlayerInfo} ! []
  EditPlayerAttrs operation attr ->
    {model | playerAttrs = extend operation attr model.playerAttrs} ! []
    -- (!) : model -> List (Cmd msg) -> (model, Cmd msg)

extend : Operation -> String -> PlayerAttrs -> PlayerAttrs
extend op attr playerAttrs =
  let oldAttr =
    Dict.get attr playerAttrs
    |> Maybe.withDefault 0
  in
    case op of
      Incr ->
        if oldAttr == 5 then
          playerAttrs
        else
          Dict.insert attr (oldAttr + 1) playerAttrs
      Decr ->
        if oldAttr == 1 then
          playerAttrs
        else
          Dict.insert attr (oldAttr - 1) playerAttrs
-- View
view : Model -> Html Msg
view model =
  div []
    -- input updates state as you type
    [ playerInfoView model
    , ability2View model ]

ability2View : Model -> Html Msg
ability2View model =
  div[]
    (Dict.toList model.playerAttrs
      |> List.map ability2Selection)

playerInfoView : Model -> Html Msg
playerInfoView model =
  div []
    -- input updates state as you type
    [ input [ placeholder "Name", onInput (EditPlayerInfo "Name")] []
    , input [ placeholder "Player", onInput (EditPlayerInfo "Player")] []
    , input [ placeholder "Concept", onInput (EditPlayerInfo "Concept")] []
    , input [ placeholder "Anima", onInput (EditPlayerInfo "Anima")] []
    , casteSelection
    , ability1Selection
    ]

casteSelection : Html Msg
casteSelection =
  -- Drop down menu
  select
     [onInput (EditPlayerInfo "Caste")]
     (List.map simpleSelect castes)

ability1Selection : Html Msg
ability1Selection =
  select
    [onInput (EditPlayerInfo "Ability")]
    (List.map simpleSelect abilities1)

ability2Selection : (String, Int) -> Html Msg
ability2Selection (attr, val) =
  div []
    [ text (attr ++ " ")
    , text (toString val)
    , button
        [onClick (EditPlayerAttrs Incr attr)]
        [text "+"]
    , button
        [onClick (EditPlayerAttrs Decr attr)]
        [text "-"]
    ]

simpleSelect : String -> Html msg
simpleSelect str =
  option [value str] [text str]

castes : List String
castes =
  ["Dawn"
  ,"Zenith"
  ,"Twilight"
  ,"Night"
  ,"Eclipse"]

abilities1 : List String
abilities1 =
  ["Archery"
  ,"Athletics"
  ,"Brawl"
  ,"Bureaucracy"
  ,"Craft"
  ,"Dodge"
  ,"Integrity"
  ,"Investigation"
  ,"Larceny"
  ,"Linguistics"
  ,"Lore"
  ,"Martial Arts"
  ,"Medicine"
  ,"Melee"
  ,"Occult"
  ,"Performance"
  ,"Presence"
  ,"Resistance"
  ,"Ride"
  ,"Sail"
  ,"Socialize"
  ,"Stealth"
  ,"Thrown"
  ,"Survival"
  ,"War"
  ]

abilities2 : List String
abilities2 =
  ["Strength"
  ,"Dexerity"
  ,"Stamina"
  ,"Charisma"
  ,"Manipulation"
  ,"Appearance"
  ,"Perception"
  ,"Intelligence"
  ,"Wit"]
 -- Init



init : (Model, Cmd Msg)
init = {playerInfo = emptyPlayerInfo, playerAttrs = emptyPlayerAttrs} ! []


 -- Entry Point
main : Program Never Model Msg
main = program {
    init = init
    , view = view
    , update = update
    , subscriptions = (\_ -> Sub.none)
  }
