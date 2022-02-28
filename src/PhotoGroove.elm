module PhotoGroove exposing (main)

import Browser exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http
import Random



-- Types


type alias PhotoUrl =
    { url : String }


type Msg
    = ClickedPhoto String
    | ClickedSize ThumbnailSize
    | ClickedSurpriseMe
    | GotRandomPhoto PhotoUrl
    | GotPhotos (Result Http.Error String)


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
    , button [ onClick ClickedSurpriseMe ] [ text "Surprise Me!" ]
    , h3 [] [ text "Thumbnail Size:" ]
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
                [ text "Loading" ]

            Errored errorMessage ->
                [ text ("Error: " ++ errorMessage) ]



-- Model


initialCmd : Cmd Msg
initialCmd =
    Http.get
        { url = "http://elm-in-action.com/photos/list"
        , expect = Http.expectString  GotPhotos
        }


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

        GotPhotos (Ok responseStr) ->
            case String.split "," responseStr of
                -- name urls given to array, while first element is also named
                (firstUrl :: _) as urls ->
                    let
                        -- type aliases are also constructor functions
                        photos =
                            List.map PhotoUrl urls
                    in
                    ( { model | status = Loaded photos firstUrl }, Cmd.none )

                [] ->
                    ( { model | status = Errored "Could not find any photos" }, Cmd.none )

        GotPhotos (Err _) ->
            ( { model | status = Errored "Server error" }, Cmd.none )


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
        { init = \flags -> ( initModel, initialCmd )
        , view = view
        , update = update
        , subscriptions = \model -> Sub.none
        }
