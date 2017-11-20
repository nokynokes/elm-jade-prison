import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
 -- Model

type alias Model = {
  name : String
  , player : String
  , caste : String
  , concept : String
  , anima : String
  , ability : String
}

-- Update

type Msg
   = EditName String
   | EditPlayer String
   | EditCaste String
   | EditConcept String
   | EditAnima String
   | EditAbility String


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =  case msg of
  EditName str -> {model | name = str } ! []
  EditPlayer str -> {model | player = str} ! []
  EditCaste str -> {model | caste = str} ! []
  EditConcept str -> {model | concept = str} ! []
  EditAnima str -> {model | anima = str} ! []
  EditAbility str -> {model | ability = str} ! []
    -- (!) : model -> List (Cmd msg) -> (model, Cmd msg)

-- View
view : Model -> Html Msg
view model =
  div []
    -- input updates state as you type
    [ input [ placeholder "Name", onInput EditName ] []
    , input [ placeholder "Player", onInput EditPlayer ] []
    , input [ placeholder "Concept", onInput EditConcept ] []
    , br [] []
    , input [ placeholder "Anima", onInput EditAnima ] []
    , casteSelection
    , abilitySelection
    ]

casteSelection : Html Msg
casteSelection =
  -- Drop down menu
  select
     [onInput EditCaste]
     (List.map simpleSelect castes)

abilitySelection : Html Msg
abilitySelection =
  select
    [onInput EditAbility]
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
init = (Model "" "" "Dawn" "" "" "Archery") ! []


 -- Entry Point
main : Program Never Model Msg
main = program {
    init = init
    , view = view
    , update = update
    , subscriptions = (\_ -> Sub.none)
  }
