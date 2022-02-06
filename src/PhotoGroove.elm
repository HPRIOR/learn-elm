module PhotoGroove exposing (main)

import Array exposing (Array)
import Browser exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Random


type alias PhotoUrl =
    { url : String }


type Msg
    = ClickedPhoto String
    | ClickedSize ThumbnailSize
    | ClickedSurpriseMe
    | GotSelectedIndex Int


type ThumbnailSize
    = Small
    | Medium
    | Large


type alias Model =
    { photos : List PhotoUrl, selectedUrl : String, size : ThumbnailSize }


urlPrefix : String
urlPrefix =
    "http://elm-in-action.com/"


viewSizeChooser : ThumbnailSize -> Html Msg
viewSizeChooser size =
    label []
        [ input
            [ type_ "radio"
            , name "size"
            , onClick (ClickedSize size)
            ]
            []
        , text (sizeToString size)
        ]


sizeToString : ThumbnailSize -> String
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
        , onClick (ClickedPhoto thumb.url)
        ]
        []


view : Model -> Html Msg
view model =
    div [ class "content" ]
        [ h1 [] [ text "PhotoGroove" ]
        , button [ onClick ClickedSurpriseMe ] [ text "Suprise Me!" ]
        , h3 [] [ text "Thumnail Size:" ]
        , div [ id "choose-size" ] <| [ Small, Medium, Large ] |> List.map viewSizeChooser
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


photoArray : Array PhotoUrl
photoArray =
    Array.fromList initModel.photos


randomPhotoPicker : Random.Generator Int
randomPhotoPicker =
    Random.int 0 (Array.length photoArray - 1)


getPhotoUrl : Int -> String
getPhotoUrl index =
    case Array.get index photoArray of
        Just photo ->
            photo.url

        Nothing ->
            ""


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        -- Union types parsed here
        ClickedPhoto url ->
            ( { model | selectedUrl = url }, Cmd.none )

        ClickedSurpriseMe ->
            ( model, Random.generate GotSelectedIndex randomPhotoPicker )

        ClickedSize thumbnailSize ->
            ( { model | size = thumbnailSize }, Cmd.none )

        GotSelectedIndex index ->
            ( { model | selectedUrl = getPhotoUrl index }, Cmd.none )


main : Program () Model Msg
main =
    Browser.element
        { init = \flags -> ( initModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = \model -> Sub.none
        }
