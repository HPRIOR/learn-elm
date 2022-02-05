module PhotoGroove exposing (main)

-- import Array exposing (Array)

import Browser exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)


type alias PhotoUrl =
    { url : String }


type alias Msg =
    { description : String, data : String }


type alias Model =
    { photos : List PhotoUrl, selectedUrl : String }


urlPrefix : String
urlPrefix =
    "http://elm-in-action.com/"


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
        , div [ id "thumbnails" ]
            (model.photos |> List.map (getThumbnailUrls model.selectedUrl))

        -- returns a list of images which are the children of the div
        , img
            [ class "large"
            , src (urlPrefix ++ "large/" ++ model.selectedUrl)
            ]
            []
        ]


initModel : { photos : List PhotoUrl, selectedUrl : String }
initModel =
    { photos = [ { url = "1.jpeg" }, { url = "2.jpeg" }, { url = "3.jpeg" }, { url = "4.jpeg" } ] -- used to display images
    , selectedUrl = "1.jpeg" -- used to display larger image
    }



-- photoArray : Array PhotoUrl
-- photoArray =
--     Array.fromList initModel.photos


update msg model =
    if msg.description == "ClickedPhoto" then
        { model | selectedUrl = msg.data }

    else
        model


main =
    Browser.sandbox
        { init = initModel
        , view = view
        , update = update
        }
