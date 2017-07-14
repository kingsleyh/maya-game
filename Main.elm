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
    }


init : ( Model, Cmd Msg )
init =
    ( { randomItems = []
      , itemToFind = ""
      , foundItems = 0
      , score = 0
      , startedGame = False
      , levelComplete = False
      }
    , Cmd.none
    )



-- MESSAGES


type Msg
    = NoOp
    | StartGame
    | RandomShapes (List Int)
    | SelectItem Item



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        StartGame ->
            let
                _ =
                    Debug.log "starting: " ""
            in
                ( { model | startedGame = True, foundItems = 0, levelComplete = False }, Random.generate RandomShapes (Random.list 4 (int 0 3)) )

        RandomShapes list ->
            let
                randomItems =
                    getRandomItemFile list

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

                --                cmd =
                --                    if List.isEmpty expectedItems then
                --                        fire StartGame
                --                    else
                --                        Cmd.none
            in
                ( { updatedModel | levelComplete = List.isEmpty expectedItems }, Cmd.none )


fire : msg -> Cmd msg
fire msg =
    Task.perform identity (Task.succeed msg)


roundToZero : Int -> Int
roundToZero i =
    if i >= 0 then
        i
    else
        0



-- VIEW


outlineSet : List String
outlineSet =
    [ "outline_square", "outline_star", "outline_circle", "outline_rectangle" ]


getRandomItemFile : List Int -> List Item
getRandomItemFile list =
    let
        randItems =
            List.indexedMap (getStringAtIndex outlineSet) list
    in
        randItems


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
            img [ onClick StartGame, src "assets/play.png" ] [ ]
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
        img [ onClick StartGame, src "assets/next.png" ] [ ]
    else
        div []
            [ h2 [] [ text "Click the shapes above that match this shape:" ]
            , img [ src ("assets/" ++ model.itemToFind ++ ".png") ] []
            ]


row : Item -> Html Msg
row item =
    td [] [ img [ onClick (SelectItem item), src ("assets/" ++ item.value ++ ".png") ] [] ]
