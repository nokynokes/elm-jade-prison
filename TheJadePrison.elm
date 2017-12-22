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
type alias PlayerAbils = Dict.Dict String (Bool, Int)
type alias Model = {
  playerInfo : PlayerInfo
  , playerAttrs : PlayerAttrs
  , playerAbils : PlayerAbils
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
emptyPlayerAttrs = List.map (\str -> (str, 1)) exAttrs |> Dict.fromList

emptyPlayerAbils : PlayerAbils
emptyPlayerAbils = List.map (\str -> (str, (False, 0))) exAbils |> Dict.fromList
-- Update

type Msg
   = EditPlayerInfo String String
   | EditPlayerAttrs String Int
   | EditPlayerAbils String Bool Int



update : Msg -> Model -> (Model, Cmd Msg)
update msg model =  case msg of
  EditPlayerInfo attr val ->
    let newPlayerInfo =

    in
      {model | playerInfo = newPlayerInfo} ! []
  EditPlayerAttrs attr val ->
    {model | playerAttrs = extendAttrs attr val model.playerAttrs} ! []
  EditPlayerAbils abil bool int ->
    {model | playerAbils = extendAbils abil (bool,int) model.playerAbils} ! []

extendAttrs : String -> Int -> PlayerAttrs -> PlayerAttrs
extendAttrs attr val playerAttrs =
  Dict.insert attr val playerAttrs

extendAbils : String -> (Bool,Int) -> PlayerAbils -> PlayerAbils
extendAbils abil val abils =
  Dict.insert abil val abils

extendInfo : String -> String -> PlayerInfo -> PlayerInfo
extendInfo str1 str2 playerinfo =
  Dict.insert str1 (if String.length str2 > 0 then (Just str2) else Nothing) playerinfo

-- View
view : Model -> Html Msg
view model =
  div []
    -- input updates state as you type
    [ playerInfoView model
    , attrsView model
    , allAbilitiesView model.playerAbils
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

pointCircleAbils : String -> Int -> Bool -> Html Msg
pointCircleAbils abil val fill =
  Svg.svg
      [ SvgAttrs.width "20"
      , SvgAttrs.height "20"
      , onClick (EditPlayerAbils abil False val)
      ]
      [ Svg.circle
          [ SvgAttrs.cx "10"
          , SvgAttrs.cy "10"
          , SvgAttrs.r "8"
          , SvgAttrs.stroke "black"
          , SvgAttrs.strokeWidth "2"
          , if fill then
                SvgAttrs.fill "black"
            else
                SvgAttrs.fill "white"
          ]
          []
      ]


info : String -> List (Attribute Msg)
info str = [placeholder str, onInput (EditPlayerInfo str)]

attrsView : Model -> Html Msg
attrsView model =
  let attrs =
    model.playerAttrs
  in
  div[]
    [ h2 [] [ text "Attributes"]
    , physicalAttrs attrs
    , socialAttrs attrs
    , mentalAttrs attrs
    ]

allAbilitiesView : PlayerAbils -> Html Msg
allAbilitiesView abils =
  div []
      ([ h2 [] [ text "Abilities" ] ]
      ++ List.map (abilSelection abils) exAbils
      )



physicalAttrs : PlayerAttrs -> Html Msg
physicalAttrs attrs =
  div []
      [ h3 [] [ text "Physical"]
      , attrSelection attrs "Strength"
      , attrSelection attrs "Dexerity"
      , attrSelection attrs "Stamina"
      ]

socialAttrs : PlayerAttrs -> Html Msg
socialAttrs attrs =
  div []
      [ h3 [] [ text "Social"]
      , attrSelection attrs "Charisma"
      , attrSelection attrs "Manipulation"
      , attrSelection attrs "Appearance"
      ]

mentalAttrs : PlayerAttrs -> Html Msg
mentalAttrs attrs =
  div []
      [ h3 [] [ text "Mental"]
      , attrSelection attrs "Perception"
      , attrSelection attrs "Intelligence"
      , attrSelection attrs "Wits"
      ]


playerInfoView : Model -> Html Msg
playerInfoView model =
  div []
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

attrSelection : PlayerAttrs -> String -> Html Msg
attrSelection attrs attr =
  let
    val =
      Dict.get attr attrs
      |> Maybe.withDefault 1
    bools =
      List.map2 (\r v -> r >= v)
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


abilSelection : PlayerAbils -> String -> Html Msg
abilSelection abils abil =
  let
    (favoured, val) =
      Dict.get abil abils
          |> Maybe.withDefault (False, 0)
    bools =
      List.map2 (\r v -> r >= v)
        (List.repeat 5 val)
        (List.range 1 5)
  in
    div []
      [ text abil
      , div
          []
          (List.map2 (pointCircleAbils abil)
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

exAttrs : List String
exAttrs =
  ["Strength"
  ,"Dexerity"
  ,"Stamina"
  ,"Charisma"
  ,"Manipulation"
  ,"Appearance"
  ,"Perception"
  ,"Intelligence"
  ,"Wits"
  ]

exAbils : List String
exAbils =
  ["Archery"
  , "Athletics"
  , "Awareness"
  , "Brawl"
  , "Bureaucracy"
  , "Craft"
  , "Dodge"
  , "Integrity"
  , "Investigation"
  , "Larceny"
  , "Linguistics"
  , "Lore"
  , "Martial Arts"
  , "Medicine"
  , "Melee"
  , "Occult"
  , "Performance"
  , "Presence"
  , "Resistance"
  , "Ride"
  , "Sail"
  , "Socialize"
  , "Stealth"
  , "Survival"
  , "Thrown"
  , "War"
  ]

 -- Init



init : (Model, Cmd Msg)
init = { playerInfo = emptyPlayerInfo
        , playerAttrs = emptyPlayerAttrs
        , playerAbils = emptyPlayerAbils } ! []


 -- Entry Point
main : Program Never Model Msg
main = program {
    init = init
    , view = view
    , update = update
    , subscriptions = (\_ -> Sub.none)
  }
