import Dict
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
 -- Model
type alias PlayerInfo = Dict.Dict String (Maybe String)
type alias Model = {
  playerInfo : PlayerInfo
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
-- Update

type Msg
   = EditPlayerInfo String String


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =  case msg of
  EditPlayerInfo attr val ->
    let newPlayerInfo =
      Dict.insert attr (if String.length val > 0 then (Just val) else Nothing) model.playerInfo
    in
      {model | playerInfo = newPlayerInfo} ! []
    -- (!) : model -> List (Cmd msg) -> (model, Cmd msg)

-- View
view : Model -> Html Msg
view model =
  div []
    -- input updates state as you type
    [ input [ placeholder "Name", onInput (EditPlayerInfo "Name")] []
    , input [ placeholder "Player", onInput (EditPlayerInfo "Player")] []
    , input [ placeholder "Concept", onInput (EditPlayerInfo "Concept")] []
    , br [] []
    , input [ placeholder "Anima", onInput (EditPlayerInfo "Anima")] []
    , casteSelection
    , abilitySelection
    ]

casteSelection : Html Msg
casteSelection =
  -- Drop down menu
  select
     [onInput (EditPlayerInfo "Caste")]
     (List.map simpleSelect castes)

abilitySelection : Html Msg
abilitySelection =
  select
    [onInput (EditPlayerInfo "Ability")]
    (List.map simpleSelect abilities)

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

abilities : List String
abilities =
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

 -- Init
init : (Model, Cmd Msg)
init = {playerInfo = emptyPlayerInfo} ! []


 -- Entry Point
main : Program Never Model Msg
main = program {
    init = init
    , view = view
    , update = update
    , subscriptions = (\_ -> Sub.none)
  }
