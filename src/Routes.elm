module Routes exposing (BreedName, Route(..), parseUrl, routeParser)

import Url
import Url.Parser exposing ((</>), Parser, map, oneOf, s, string)


type alias BreedName =
    String


type Route
    = BreedList
    | BreedDetail BreedName


routeParser : Parser (Route -> a) a
routeParser =
    oneOf
        [ map BreedList (s "")
        , map BreedDetail (s "breed" </> breedNameParser)
        ]


parseUrl : Url.Url -> Route
parseUrl url =
    Maybe.withDefault BreedList (Url.Parser.parse routeParser url)


breedNameParser : Parser (BreedName -> a) a
breedNameParser =
    string
