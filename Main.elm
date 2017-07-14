module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Random exposing (..)
import List.Extra as LE
import Task


-- MAIN


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }



-- MODEL


type alias Item =
    { id : Int, value : String }


type alias Model =
    { randomItems : List Item
    , itemToFind : String
    , foundItems : Int
    , score : Int
    , startedGame : Bool
    , levelComplete : Bool
    , levelInfo : LevelInfo
    }


init : ( Model, Cmd Msg )
init =
    ( { randomItems = []
      , itemToFind = ""
      , foundItems = 0
      , score = 0
      , startedGame = False
      , levelComplete = False
      , levelInfo = { level = Level1, size = 1 }
      }
    , Cmd.none
    )



-- MESSAGES


type Msg
    = NoOp
    | StartGame
    | RandomShapes (List Int)
    | SelectItem Item


type Level
    = Level1
    | Level2
    | Level3
    | Level4
    | Level5
    | Level6
    | Level7
    | Level8
    | Level9


type alias LevelInfo =
    { level : Level, size : Int }


simpleOutline : List String
simpleOutline =
    [ "outline_square", "outline_star", "outline_circle", "outline_rectangle", "outline_triangle" ]


simpleFilled : List String
simpleFilled =
    [ "red_square"
    , "red_star"
    , "red_circle"
    , "red_rectangle"
    , "red_triangle"
    , "blue_square"
    , "blue_star"
    , "blue_circle"
    , "blue_rectangle"
    , "blue_triangle"
    , "green_square"
    , "green_star"
    , "green_circle"
    , "green_rectangle"
    , "green_triangle"
    , "purple_square"
    , "purple_star"
    , "purple_circle"
    , "purple_rectangle"
    , "purple_triangle"
    , "orange_square"
    , "orange_star"
    , "orange_circle"
    , "orange_rectangle"
    , "orange_triangle"
    , "yellow_square"
    , "yellow_star"
    , "yellow_circle"
    , "yellow_rectangle"
    , "yellow_triangle"
    , "brown_square"
    , "brown_star"
    , "brown_circle"
    , "brown_rectangle"
    , "brown_triangle"
    ]



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        StartGame ->
            let
                ( _, maxRand ) =
                    getIconSetForLevel model.levelInfo

                { level, size } =
                    model.levelInfo
            in
                ( { model | startedGame = True, foundItems = 0, levelComplete = False }, Random.generate RandomShapes (Random.list size (int 0 maxRand)) )

        RandomShapes list ->
            let
                randomItems =
                    getRandomItemFile model.levelInfo list

                itemToFind =
                    Maybe.withDefault (Item 0 "outline_square") (List.head randomItems)
            in
                ( { model | randomItems = randomItems, itemToFind = itemToFind.value }, Cmd.none )

        SelectItem item ->
            let
                updatedModel =
                    if item.value == model.itemToFind then
                        let
                            updatedRandomItems =
                                LE.replaceIf (\i -> i.id == item.id) (Item item.id "tick") model.randomItems
                        in
                            { model | randomItems = updatedRandomItems, foundItems = model.foundItems + 1, score = model.score + 1 }
                    else
                        { model | score = (roundToZero (model.score - 1)) }

                expectedItems =
                    List.filter (\i -> i.value == updatedModel.itemToFind) updatedModel.randomItems

                updatedLevelInfo =
                    calculateLevelInfo updatedModel.score
            in
                ( { updatedModel | levelComplete = List.isEmpty expectedItems, levelInfo = updatedLevelInfo }, Cmd.none )


fire : msg -> Cmd msg
fire msg =
    Task.perform identity (Task.succeed msg)


roundToZero : Int -> Int
roundToZero i =
    if i >= 0 then
        i
    else
        0


calculateLevelInfo : Int -> LevelInfo
calculateLevelInfo score =
    if score == 0 then
        { level = Level1, size = 1 }
    else if score == 1 then
        { level = Level2, size = 2 }
    else if score == 2 then
        { level = Level2, size = 2 }
    else if score == 3 then
        { level = Level3, size = 3 }
    else if score == 4 then
        { level = Level4, size = 4 }
    else if score == 5 then
        { level = Level5, size = 5 }
    else if score == 6 then
        { level = Level6, size = 6 }
    else if score == 7 then
        { level = Level7, size = 7 }
    else if score == 8 then
        { level = Level8, size = 8 }
    else
        { level = Level9, size = 8 }


getIconSetForLevel : LevelInfo -> ( List String, Int )
getIconSetForLevel { level, size } =
    case level of
        Level1 ->
            ( simpleOutline, getLength simpleOutline )

        Level2 ->
            ( simpleOutline, getLength simpleOutline )

        Level3 ->
            ( simpleOutline, getLength simpleOutline )

        Level4 ->
            ( simpleOutline, getLength simpleOutline )

        _ ->
            let
                list =
                    simpleOutline
                        ++ simpleFilled
            in
                ( list, getLength list )


getLength : List String -> Int
getLength list =
    (List.length list) - 1



-- VIEW


getRandomItemFile : LevelInfo -> List Int -> List Item
getRandomItemFile levelInfo list =
    let
        ( iconList, _ ) =
            getIconSetForLevel levelInfo
    in
        List.indexedMap (getStringAtIndex iconList) list


getStringAtIndex : List String -> Int -> Int -> Item
getStringAtIndex list index i =
    case (LE.getAt i list) of
        Just s ->
            Item index s

        _ ->
            Item 0 "outline_square"


view : Model -> Html Msg
view model =
    div []
        [ if model.startedGame then
            gameView model
          else
            img [ onClick StartGame, src "assets/play.png" ] []
        ]


gameView : Model -> Html Msg
gameView model =
    div []
        [ div [] [ text ("Score: " ++ (toString model.score)) ]
        , table [] (List.map row model.randomItems)
        , nextOrItemToFind model
        ]


nextOrItemToFind : Model -> Html Msg
nextOrItemToFind model =
    if model.levelComplete then
        img [ onClick StartGame, src "assets/next.png" ] []
    else
        div []
            [ h2 [] [ text "Click the shapes above that match this shape:" ]
            , img [ src ("assets/" ++ model.itemToFind ++ ".png") ] []
            ]


row : Item -> Html Msg
row item =
    td [] [ img [ onClick (SelectItem item), src ("assets/" ++ item.value ++ ".png") ] [] ]
