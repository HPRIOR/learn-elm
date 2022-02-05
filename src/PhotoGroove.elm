module PhotoGroove exposing (main)

import Array
import Browser exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Array exposing (Array)


type alias PhotoUrl =
    { url : String }


type alias Msg =
    { description : String, data : String }


type ThumnailSize
    = Small
    | Medium
    | Large


type alias Model =
    { photos : List PhotoUrl, selectedUrl : String, size : ThumnailSize }


urlPrefix : String
urlPrefix =
    "http://elm-in-action.com/"


viewSizeChooser : ThumnailSize -> Html Msg
viewSizeChooser size =
    label []
        [ input [ type_ "radio", name "size" ] [] -- avoid keyword type
        , text (sizeToString size)
        ]


sizeToString : ThumnailSize -> String
sizeToString size =
    case size of
        Small ->
            "small"

        Medium ->
            "med"

        Large ->
            "large"


getThumbnailUrls : String -> PhotoUrl -> Html Msg
getThumbnailUrls selectedUrl thumb =
    img
        [ src (urlPrefix ++ thumb.url)
        , classList [ ( "selected", selectedUrl == thumb.url ) ]
        , onClick { description = "ClickedPhoto", data = thumb.url }
        ]
        []


view : Model -> Html Msg
view model =
    div [ class "content" ]
        [ h1 [] [ text "PhotoGroove" ]
        , button [ onClick { description = "ClickedSupriseMe", data = "" } ] [ text "Suprise Me!" ]
        , h3 [] [ text "Thumnail Size:" ]
        , div [ id "choose-size" ] ([ Small, Medium, Large ] |> List.map viewSizeChooser)
        , div [ id "thumbnails", class (sizeToString model.size) ]
            (model.photos |> List.map (getThumbnailUrls model.selectedUrl))
        , img
            [ class "large"
            , src (urlPrefix ++ "large/" ++ model.selectedUrl)
            ]
            []
        ]


initModel : Model
initModel =
    { photos = [ { url = "1.jpeg" }, { url = "2.jpeg" }, { url = "3.jpeg" }, { url = "4.jpeg" } ] -- used to display images
    , selectedUrl = "1.jpeg" -- used to display larger image
    , size = Medium
    }


getPhotoUrl : Int -> Array PhotoUrl -> String
getPhotoUrl index photoArray =
    case Array.get index photoArray of
        Just photo ->
            photo.url

        Nothing ->
            ""


update : Msg -> Model -> Model
update msg model =
    case msg.description of
        "ClickedPhoto" ->
            { model | selectedUrl = msg.data }

        "ClickedSupriseMe" ->
            { model | selectedUrl = "2.jpeg" }

        _ ->
            model


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initModel
        , view = view
        , update = update
        }
