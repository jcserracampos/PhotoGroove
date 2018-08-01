module PhotoGroove exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Array exposing (Array)
import Random

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
            , img [ class "large", src (urlPrefix ++ "large/" ++ model.selectedUrl) ] []
    ]

viewThumbnail : String -> Photo -> Html Msg
viewThumbnail selectedUrl thumbnail = 
        img [ src (urlPrefix ++ thumbnail.url)
        -- Html.classList builds a class attribute using a list of touples
        -- first comes the desired class and second a bolean for whether to include
        , classList [ ( "selected", selectedUrl == thumbnail.url) ], onClick (SelectByUrl thumbnail.url ) ] []

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
    { photos: List Photo
    , selectedUrl: String
    , chosenSize: ThumbnailSize }

initialModel : Model
initialModel =
    { photos = 
        [ { url = "1.jpeg" } 
        , { url = "2.jpeg" } 
        , { url = "3.jpeg" } 
        ]
    , selectedUrl = "1.jpeg"
    , chosenSize = Large
    }

photoArray : Array Photo
photoArray =
    Array.fromList initialModel.photos

getPhotoUrl : Int -> String
getPhotoUrl index =
    case Array.get index photoArray of
        Just photo ->
            photo.url
        Nothing ->
            ""

randomPhotoPicker : Random.Generator Int
randomPhotoPicker =
    Random.int 0 (Array.length photoArray - 1)

type Msg 
    = SelectByUrl String
    | SelectByIndex Int
    | SurpriseMe
    | SetSize ThumbnailSize

update : Msg -> Model -> ( Model, Cmd Msg)
update message model =
    case message of
        SelectByUrl url ->
            ( { model | selectedUrl = url }, Cmd.none )
        SelectByIndex index ->
            ( { model | selectedUrl = getPhotoUrl index}, Cmd.none )
        SurpriseMe ->
            ( model, Random.generate SelectByIndex randomPhotoPicker )
        SetSize size ->
            ( { model | chosenSize = size}, Cmd.none )

main: Program Never Model Msg
main =
    Html.program
        { init = ( initialModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = (\model -> Sub.none)
        }