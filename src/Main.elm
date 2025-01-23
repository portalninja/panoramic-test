module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Dict exposing (Dict)
import DogApi exposing (DogBreedDetailsResponse, DogBreedsResponse, fetchBreedDetails, fetchBreeds)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http
import Routes exposing (BreedName, Route(..))
import Url



-- MAIN


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }



-- MODEL


type alias Model =
    { page : Page
    , cache : Cache
    , navKey : Nav.Key
    , navUrl : Url.Url
    }


type Page
    = BreedListPage BreedListPageState
    | BreedDetailPage BreedDetailPageState


type alias Cache =
    { breeds : Maybe Breeds
    , breedDetails : Dict BreedName (Maybe BreedDetails)
    }


type alias Breeds =
    { breeds : List String }


type alias BreedDetails =
    { images : List String }


type BreedListPageState
    = Loading
    | Loaded Breeds


type BreedDetailPageState
    = LoadingDetail
    | LoadedDetail
        { breedName : String
        , breedDetails : BreedDetails
        , pageNumber : Int
        }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    ( { page = BreedListPage Loading
      , cache =
            { breeds = Nothing
            , breedDetails = Dict.empty
            }
      , navKey = key
      , navUrl = url
      }
    , fetchBreeds GotBreeds
    )



-- UPDATE


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | GotBreeds (Result Http.Error DogBreedsResponse)
    | GotBreedDetails String (Result Http.Error DogBreedDetailsResponse)
    | ChangePage Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.navKey (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            let
                route =
                    Routes.parseUrl url
            in
            withRouteChangeHandler model route

        GotBreeds (Ok breedsResponse) ->
            let
                breeds =
                    Breeds (Dict.keys breedsResponse.message)

                newCache =
                    { breeds = Just breeds
                    , breedDetails = model.cache.breedDetails
                    }
            in
            ( { model
                | page = BreedListPage (Loaded breeds)
                , cache = newCache
              }
            , Cmd.none
            )

        GotBreeds (Err error) ->
            ( model, Cmd.none )

        GotBreedDetails breedName (Ok breedDetailsResponse) ->
            let
                breedDetails =
                    BreedDetails breedDetailsResponse.message

                newCache =
                    { breeds = model.cache.breeds
                    , breedDetails =
                        Dict.insert
                            breedName
                            (Just breedDetails)
                            model.cache.breedDetails
                    }
            in
            ( { model
                | page =
                    BreedDetailPage
                        (LoadedDetail
                            { breedName = breedName
                            , breedDetails = breedDetails
                            , pageNumber = 0
                            }
                        )
                , cache = newCache
              }
            , Cmd.none
            )

        GotBreedDetails breedName (Err error) ->
            ( model, Cmd.none )

        ChangePage pageNumber ->
            case model.page of
                BreedDetailPage (LoadedDetail details) ->
                    ( { model
                        | page =
                            BreedDetailPage
                                (LoadedDetail { details | pageNumber = pageNumber })
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )


withRouteChangeHandler : Model -> Route -> ( Model, Cmd Msg )
withRouteChangeHandler model route =
    case route of
        BreedList ->
            case model.cache.breeds of
                Just breeds ->
                    ( { model | page = BreedListPage (Loaded breeds) }, Cmd.none )

                Nothing ->
                    ( { model | page = BreedListPage Loading }, fetchBreeds GotBreeds )

        BreedDetail breedName ->
            case Dict.get breedName model.cache.breedDetails of
                Just (Just details) ->
                    ( { model
                        | page =
                            BreedDetailPage
                                (LoadedDetail
                                    { breedName = breedName
                                    , breedDetails = details
                                    , pageNumber = 0
                                    }
                                )
                      }
                    , Cmd.none
                    )

                _ ->
                    ( { model | page = BreedDetailPage LoadingDetail }
                    , fetchBreedDetails breedName (GotBreedDetails breedName)
                    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Panoramic Dogs"
    , body =
        viewBody model
    }


viewBody : Model -> List (Html Msg)
viewBody model =
    case model.page of
        BreedListPage listPageState ->
            viewBreedListPage listPageState

        BreedDetailPage detailPageState ->
            viewBreedDetailPage detailPageState


viewBreedListPage : BreedListPageState -> List (Html Msg)
viewBreedListPage listPageState =
    case listPageState of
        Loading ->
            [ text "Loading breeds..." ]

        Loaded breeds ->
            let
                sortedBreeds =
                    List.sort breeds.breeds
            in
            [ ul [] (List.map viewBreedLink sortedBreeds) ]


viewBreedDetailPage : BreedDetailPageState -> List (Html Msg)
viewBreedDetailPage detailPageState =
    case detailPageState of
        LoadingDetail ->
            [ text "Loading breed details..." ]

        LoadedDetail details ->
            let
                paginationSize =
                    20

                totalPages =
                    ceiling (toFloat (List.length details.breedDetails.images) / toFloat paginationSize) - 1

                startIdx =
                    details.pageNumber * paginationSize

                currentPageImages =
                    details.breedDetails.images
                        |> List.drop startIdx
                        |> List.take paginationSize
            in
            [ div []
                [ h1 [] [ text (details.breedName ++ " breed") ]
                , h3 [] [ text (String.fromInt (List.length details.breedDetails.images) ++ " images") ]
                , viewPagination details.pageNumber totalPages
                , div [] (List.map viewBreedImage currentPageImages)
                ]
            ]


viewBreedLink : String -> Html msg
viewBreedLink breed =
    li [] [ a [ href ("/breed/" ++ breed) ] [ text breed ] ]


viewPagination : Int -> Int -> Html Msg
viewPagination currentPage totalPages =
    div [ style "margin" "20px 0" ]
        [ button
            [ onClick (ChangePage (Basics.max 0 (currentPage - 1)))
            , disabled (currentPage == 0)
            ]
            [ text "Previous" ]
        , span [ style "margin" "0 10px" ]
            [ text (String.fromInt (currentPage + 1) ++ " of " ++ String.fromInt (totalPages + 1)) ]
        , button
            [ onClick (ChangePage (Basics.min totalPages (currentPage + 1)))
            , disabled (currentPage == totalPages)
            ]
            [ text "Next" ]
        ]


viewBreedImage : String -> Html msg
viewBreedImage imageUrl =
    img
        [ src imageUrl
        , style "width" "200px"
        , style "margin" "6px"
        , style "border" "1px solid #ccc"
        , style "border-radius" "4px"
        , style "box-shadow" "0 4px 8px rgba(0, 0, 0, 0.1)"
        ]
        []
