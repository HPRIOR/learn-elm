module PhotoGroove exposing (main)

import Array exposing (Array)
import Browser exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Random



-- Types


type alias PhotoUrl =
    { url : String }


type Msg
    = ClickedPhoto String
    | ClickedSize ThumbnailSize
    | ClickedSurpriseMe
    | GotRandomPhoto PhotoUrl


type ThumbnailSize
    = Small
    | Medium
    | Large


type alias Model =
    { status : Status, size : ThumbnailSize }


type Status
    = Loading
    | Loaded (List PhotoUrl) String
    | Errored String



-- View


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


viewLoaded : List PhotoUrl -> String -> ThumbnailSize -> List (Html Msg)
viewLoaded photos selectedUrl chosenSize =
    [ h1 [] [ text "PhotoGroove" ]
    , button [ onClick ClickedSurpriseMe ] [ text "Suprise Me!" ]
    , h3 [] [ text "Thumnail Size:" ]
    , div [ id "choose-size" ] <| ([ Small, Medium, Large ] |> List.map viewSizeChooser)
    , div [ id "thumbnails", class (sizeToString chosenSize) ]
        (photos |> List.map (getThumbnailUrls selectedUrl))
    , img
        [ class (sizeToString chosenSize)
        , src (urlPrefix ++ "large/" ++ selectedUrl)
        ]
        []
    ]


view : Model -> Html Msg
view model =
    div [ class "content" ] <|
        case model.status of
            Loaded photos selectedUrl ->
                viewLoaded photos selectedUrl model.size

            Loading ->
                []

            Errored errorMessage ->
                [ text ("Error: " ++ errorMessage) ]



-- Model


initModel : Model
initModel =
    { status = Loading
    , size = Medium
    }



-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedSurpriseMe ->
            case model.status of
                Loaded (firstPhoto :: otherPhotos) _ ->
                    Random.uniform firstPhoto otherPhotos
                        |> Random.generate GotRandomPhoto
                        |> Tuple.pair model

                Loaded [] _ ->
                    ( model, Cmd.none )

                Loading ->
                    ( model, Cmd.none )

                Errored _ ->
                    ( model, Cmd.none )

        ClickedSize thumbnailSize ->
            ( { model | size = thumbnailSize }, Cmd.none )

        ClickedPhoto url ->
            ( { model | status = selectUrl url model.status }, Cmd.none )

        GotRandomPhoto photo ->
            ( { model | status = selectUrl photo.url model.status }, Cmd.none )


selectUrl : String -> Status -> Status
selectUrl url status =
    case status of
        Loaded photos _ ->
            Loaded photos url

        _ ->
            status



-- Main


main : Program () Model Msg
main =
    Browser.element
        { init = \flags -> ( initModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = \model -> Sub.none
        }
