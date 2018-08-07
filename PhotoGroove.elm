module PhotoGroove exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Array exposing (Array)
import Random
import Http

urlPrefix : String
urlPrefix =
    "http://elm-in-action.com/"

type ThumbnailSize
    = Small
    | Medium
    | Large

view : Model -> Html Msg
view model =
    div [class "content"]
        [ h1 [] [ text "Photo Groove"]
        , button [ onClick SurpriseMe ] [ text "Surprise me"]
        , h3 [] [ text "Thumbnail Size:" ]
        , div [ id "choose-size" ]
            (List.map viewSizeChooser [ Small, Medium, Large ] )
        , div [ id "thumbnails", class (sizeToClass model.chosenSize) ] 
            (List.map (viewThumbnail model.selectedUrl) model.photos )
            , viewLarge model.selectedUrl
    ]

viewLarge : Maybe String -> Html Msg
viewLarge maybeUrl =
    case maybeUrl of
        Nothing ->
            text ""
        Just url ->
            img [ class "large", src (urlPrefix ++ "large/" ++ url)] []    

viewThumbnail : Maybe String -> Photo -> Html Msg
viewThumbnail selectedUrl thumbnail = 
        img [ src (urlPrefix ++ thumbnail.url)
        -- Html.classList builds a class attribute using a list of touples
        -- first comes the desired class and second a bolean for whether to include
        , classList [ ( "selected", selectedUrl == Just thumbnail.url) ], onClick (SelectByUrl thumbnail.url ) ] []

-- TODO: 
-- Start radio checked by model
-- Detect whenever the radio state changes using onInput
viewSizeChooser : ThumbnailSize -> Html Msg
viewSizeChooser size =
    label []
        [ input [ type_ "radio", name "size", onClick (SetSize size) ] []
        , text (sizeToString size)
        ]

sizeToString : ThumbnailSize -> String
sizeToString size =
    case size of
        Small ->
            "Small"
        Medium ->
            "Medium"
        Large ->
            "Large"

sizeToClass : ThumbnailSize -> String
sizeToClass size =
    case size of
        Small -> 
            "small"
        Medium ->
            "med"
        Large ->
            "large"

type alias Photo =
    { url: String}

type alias Model =
    { photos : List Photo
    , selectedUrl : Maybe String
    , loadingError : Maybe String
    , chosenSize : ThumbnailSize }

initialModel : Model
initialModel =
    { photos = []
    , selectedUrl = Nothing
    , loadingError = Nothing
    , chosenSize = Large
    }

type Msg 
    = SelectByUrl String
    | SelectByIndex Int
    | SurpriseMe
    | SetSize ThumbnailSize
    | LoadPhotos (Result Http.Error String)

update : Msg -> Model -> ( Model, Cmd Msg)
update message model =
    case message of
        SelectByUrl url ->
            ( { model | selectedUrl = Just url }, Cmd.none )
        SelectByIndex index ->
            let
                newSelectedUrl : Maybe String
                newSelectedUrl = 
                    model.photos
                        |> Array.fromList
                        |> Array.get index
                        |> Maybe.map .url
            in
               ( { model | selectedUrl = newSelectedUrl}, Cmd.none ) 
        SurpriseMe ->
            let
                randomPhotoPicker =
                    Random.int 0 (List.length model.photos - 1)    
            in
                ( model, Random.generate SelectByIndex randomPhotoPicker )
        SetSize size ->
            ( { model | chosenSize = size}, Cmd.none )
        LoadPhotos (Ok responseStr) ->
            let
                urls = 
                    String.split "," responseStr
                photos =
                    List.map Photo urls
            in
                    ( { model | photos = photos, selectedUrl = List.head urls }, Cmd.none )
        LoadPhotos (Err _) ->
            ( { model | loadingError = Just "Error! (Try turning it off and on again?)"}, Cmd.none )

initialCmd : Cmd Msg
initialCmd =
    "http://elm-in-action.com/photos/list"
        |> Http.getString
        |> Http.send LoadPhotos

viewOrError : Model -> Html Msg
viewOrError model =
    case model.loadingError of
        Nothing ->
            view model
        Just errorMessage ->
            div [ class "error-message" ]
                [ h1 [] [ text "Photo Groove" ]
                , p [] [ text errorMessage]
                ]

main: Program Never Model Msg
main =
    Html.program
        { init = ( initialModel, initialCmd )
        , view = viewOrError
        , update = update
        , subscriptions = (\_ -> Sub.none)
        }