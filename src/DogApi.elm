module DogApi exposing (..)

import Dict exposing (Dict)
import Http
import Json.Decode as D
import Routes exposing (BreedName)



-- BREEDS


fetchBreeds : (Result Http.Error DogBreedsResponse -> msg) -> Cmd msg
fetchBreeds gotBreedsMsg =
    Http.get
        { url = "https://dog.ceo/api/breeds/list/all"
        , expect = Http.expectJson gotBreedsMsg breedsDecoder
        }


type alias DogBreedsResponse =
    { message : Dict String (List String)
    }


breedsDecoder : D.Decoder DogBreedsResponse
breedsDecoder =
    D.map DogBreedsResponse
        (D.field "message"
            (D.dict (D.list D.string))
        )



-- BREED DETAILS


fetchBreedDetails : BreedName -> (Result Http.Error DogBreedDetailsResponse -> msg) -> Cmd msg
fetchBreedDetails breedName gotBreedDetailsMsg =
    Http.get
        { url = "https://dog.ceo/api/breed/" ++ breedName ++ "/images"
        , expect = Http.expectJson gotBreedDetailsMsg breedDetailsDecoder
        }


type alias DogBreedDetailsResponse =
    { message : List String }


breedDetailsDecoder : D.Decoder DogBreedDetailsResponse
breedDetailsDecoder =
    D.map DogBreedDetailsResponse
        (D.field "message"
            (D.list D.string)
        )
