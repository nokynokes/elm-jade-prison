import Dict
import String
import Html exposing (..)
import Svg
import Svg.Attributes as SvgAttrs
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
   | EditPlayerAttrs String Int



update : Msg -> Model -> (Model, Cmd Msg)
update msg model =  case msg of
  EditPlayerInfo attr val ->
    let newPlayerInfo =
      Dict.insert attr (if String.length val > 0 then (Just val) else Nothing) model.playerInfo
    in
      {model | playerInfo = newPlayerInfo} ! []
  EditPlayerAttrs attr val ->
    {model | playerAttrs = extend attr val model.playerAttrs} ! []
    -- (!) : model -> List (Cmd msg) -> (model, Cmd msg)

extend : String -> Int -> PlayerAttrs -> PlayerAttrs
extend attr val playerAttrs =
  Dict.insert attr val playerAttrs

-- View
view : Model -> Html Msg
view model =
  div []
    -- input updates state as you type
    [ playerInfoView model
    , ability2View model
    ]


pointCircle : String -> Int -> Bool -> Html Msg
pointCircle attr val filled =
  Svg.svg
      [ SvgAttrs.width "20"
      , SvgAttrs.height "20"
      , onClick (EditPlayerAttrs attr val)
      ]
      [ Svg.circle
          [ SvgAttrs.cx "10"
          , SvgAttrs.cy "10"
          , SvgAttrs.r "8"
          , SvgAttrs.stroke "black"
          , SvgAttrs.strokeWidth "2"
          , if filled then
                SvgAttrs.fill "black"
            else
                SvgAttrs.fill "white"
          ]
          []
      ]


info : String -> List (Attribute Msg)
info str = [placeholder str, onInput (EditPlayerInfo str)]

ability2View : Model -> Html Msg
ability2View model =
  div[]
    (Dict.toList model.playerAttrs
      |> List.map ability2Selection)

playerInfoView : Model -> Html Msg
playerInfoView model =
  div []
    -- input updates state as you type
    [ input (info "Name") []
    , input (info "Player") []
    , input (info "Concept") []
    , input (info "Anima") []
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
  let
    bools =
      List.map2 (\ref val -> ref >= val)
        (List.repeat 5 val)
        (List.range 1 5)
  in
    div []
      [ text (attr ++ " ")
      , div
          []
          (List.map2 (pointCircle attr)
            (List.range 1 5)
            bools
          )
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
