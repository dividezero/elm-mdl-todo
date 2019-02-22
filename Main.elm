{- This file re-implements the Elm Counter example (1 counter) with elm-mdl
   buttons. Use this as a starting point for using elm-mdl components in your own
   app.
-}


module Main exposing (..)

import Html exposing (..)
import Debug exposing (log)
import Html.Events exposing (keyCode)
import Html.Lazy exposing (lazy, lazy2)
import Html.Attributes exposing (href, class, style)
import Json.Decode as JD
import Material
import Material.Scheme
import Material.Layout as Layout
import Material.Card as Card
import Material.Elevation as Elevation
import Material.List as Lists
import Material.Textfield as Textfield
import Material.Toggles as Toggles
import Material.Options as Options exposing (css)

-- MODEL


type alias Model =
    { count : Int
    , uid: Int
    , field: String
    , raiseCard: Int
    , entries : List Entry
    , mdl :
        Material.Model
        -- Boilerplate: model store for any and all Mdl components you use.
    }

type alias Entry =
    { description : String
    , completed : Bool
    , editing : Bool
    , id : Int
    }

model : Model
model =
    { count = 0
    , raiseCard = 0
    , uid = 4
    , field = ""
    , entries = [ { description = "lolol", completed = False, editing = False, id = 1}
        , { description = "lolol", completed = False, editing = False, id = 2}
        , { description = "lolol", completed = False, editing = False, id = 3}
        ]
    , mdl =
        Material.model
        -- Boilerplate: Always use this initial Mdl model store.
    }



-- ACTION, UPDATE


type Msg
    = Increase
    | Reset
    | Raise Int
    | OnEditField String
    | AddTodo String
    | ToggleDone Int Bool
    | Mdl (Material.Msg Msg)



-- Boilerplate: Msg clause for internal Mdl messages.


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Increase ->
            ( { model | count = model.count + 1 }
            , Cmd.none
            )

        Reset ->
            ( { model | count = 0 }
            , Cmd.none
            )

        Raise value ->
            ( { model | raiseCard = value }
            , Cmd.none
            )

        OnEditField value ->
            (
            { model | field = value }
            , Cmd.none
            )

        AddTodo todo ->
            (
            { model
                | uid = model.uid + 1
                , field = ""
                , entries =
                    if String.isEmpty model.field then
                        log model.field
                        model.entries
                    else
                        model.entries ++ [ newEntry model.field model.uid ]
            }
            , Cmd.none
            )

        ToggleDone id isCompleted ->
            let
                updateEntry t =
                    if t.id == id then
                        { t | completed = isCompleted }
                    else
                        t
            in
            log (Basics.toString isCompleted)
            ( { model | entries = List.map updateEntry model.entries }
            , Cmd.none
            )

        -- Boilerplate: Mdl action handler.
        Mdl msg_ ->
            Material.update Mdl msg_ model

newEntry : String -> Int -> Entry
newEntry desc id =
    { description = desc
    , completed = False
    , editing = False
    , id = id
    }

isEnter : number -> JD.Decoder Msg
isEnter code =
   if code == 13 then
      JD.succeed (AddTodo model.field)
   else
      JD.fail "not Enter"

-- VIEW


type alias Mdl =
    Material.Model


view : Model -> Html Msg
view model =
    viewBody model
        |> Material.Scheme.top


-- This used to be the `view`
viewBody : Model -> Html Msg
viewBody model = div [ style [ ( "display", "flex" ), ("justify-content", "center" ), ("padding", "32px") ]  ]
    [ Card.view
          [  css "width" "500px"
          -- Elevation
          , if model.raiseCard == 1 then Elevation.e8 else Elevation.e2
          , Elevation.transition 250
          , Options.onMouseEnter (Raise 1)
          , Options.onMouseLeave (Raise -1)
          ]
          [ Card.title []
                [ Card.head [ ]
                    [ Textfield.render Mdl [1] model.mdl
                        [ Textfield.label "What do you need to do?"
                        , Textfield.value model.field
                        , Options.onInput (OnEditField)
                        , Options.on "keydown" (JD.andThen isEnter keyCode)] []
                    ]
                , Lists.ul [ css "width" "100%" ]
                   <| List.map viewKeyedEntry (model.entries)
                ]
          ]
    ]

viewKeyedEntry : Entry -> ( Html Msg )
viewKeyedEntry todo =
    ( lazy viewEntry todo )

viewEntry : Entry -> Html Msg
viewEntry entry = div [] [ Lists.li [Options.onClick (ToggleDone entry.id (not entry.completed))]
    [ Lists.content []
        [ p [ style [ (getListStyle entry.completed) ] ] [ text entry.description ]]
    ] ]

getListStyle : Bool -> ( String, String )
getListStyle isComplete =
    if isComplete then
        ( "text-decoration", "line-through" )
    else
        ( "text-decoration", "none" )

-- Load Google Mdl CSS. You'll likely want to do that not in code as we
-- do here, but rather in your master .html file. See the documentation
-- for the `Material` module for details.


main : Program Never Model Msg
main =
    Html.program
        { init = ( model, Cmd.none )
        , view = view
        , subscriptions = always Sub.none
        , update = update
        }
