module PhotoGroove exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)

urlPrefix : String
urlPrefix =
    "http://elm-in-action.com/"

view model =
    div [class "content"]
        [ h1 [] [ text "Photo Groove"]
        , div [ id "thumbmails"] 
            (List.map (viewThumbnail model.selectedUrl) model.photos )
            , img [ class "large", src (urlPrefix ++ "large/" ++ model.selectedUrl) ] []
    ]

viewThumbnail selectedUrl thumbnail = 
        img [ src (urlPrefix ++ thumbnail.url)
        -- Html.classList builds a class attribute using a list of touples
        -- first comes the desired class and second a bolean for whether to include
        , classList [ ( "selected", selectedUrl == thumbnail.url) ] ] []

initialModel =
    { photos = 
        [ { url = "1.jpeg" } 
        , { url = "2.jpeg" } 
        , { url = "3.jpeg" } 
        ]
    , selectedUrl = "1.jpeg"
    }

main =
    view initialModel