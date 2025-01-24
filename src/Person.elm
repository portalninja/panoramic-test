module Person exposing (..)

-- EDITING TYPE


type alias EditingPerson =
    { firstName : Maybe String
    , lastName : Maybe String
    , ssn : Maybe String
    , maritalStatus : Maybe MaritalStatus
    , phoneNumber : Maybe String
    }



-- VALIDATED TYPES


type ValidatedPerson
    = ValidatedPerson
        { firstName : NonEmptyString
        , lastName : NonEmptyString
        , ssn : SSN
        , maritalStatus : MaritalStatus
        , phoneNumber : PhoneNumber
        }


type MaritalStatus
    = Single
    | Married
    | Divorced
    | Widowed


type NonEmptyString
    = NonEmptyString String


type SSN
    = SSN String


type PhoneNumber
    = PhoneNumber String



-- CONSTRUCTORS


createNonEmptyString : String -> Result String NonEmptyString
createNonEmptyString str =
    if String.trim str |> String.isEmpty then
        Err "String cannot be empty"

    else
        Ok (NonEmptyString str)


createSSN : String -> Result String SSN
createSSN str =
    let
        cleaned =
            String.filter Char.isDigit str
    in
    if String.length cleaned == 9 then
        Ok (SSN cleaned)

    else
        Err "Invalid SSN format"


createPhoneNumber : String -> Result String PhoneNumber
createPhoneNumber str =
    let
        cleaned =
            String.filter Char.isDigit str
    in
    if String.length cleaned == 10 then
        Ok (PhoneNumber cleaned)

    else
        Err "Invalid phone number format"



-- VALIDATION


validatePerson : EditingPerson -> Result (List String) ValidatedPerson
validatePerson editing =
    let
        firstNameResult =
            editing.firstName
                |> Maybe.map createNonEmptyString
                |> Maybe.withDefault (Err "First name required")

        lastNameResult =
            editing.lastName
                |> Maybe.map createNonEmptyString
                |> Maybe.withDefault (Err "Last name required")

        ssnResult =
            editing.ssn
                |> Maybe.map createSSN
                |> Maybe.withDefault (Err "SSN required")

        maritalStatusResult =
            editing.maritalStatus
                |> Maybe.map Ok
                |> Maybe.withDefault (Err "Marital status required")

        phoneNumberResult =
            editing.phoneNumber
                |> Maybe.map createPhoneNumber
                |> Maybe.withDefault (Err "Phone number required")

        mapError : Result String a -> Maybe String
        mapError result =
            case result of
                Ok _ ->
                    Nothing

                Err msg ->
                    Just msg

        errors =
            List.filterMap
                identity
                [ firstNameResult |> mapError
                , lastNameResult |> mapError
                , ssnResult |> mapError
                , maritalStatusResult |> mapError
                , phoneNumberResult |> mapError
                ]
    in
    if List.isEmpty errors then
        Ok
            (ValidatedPerson
                { firstName = Result.withDefault (NonEmptyString "") firstNameResult
                , lastName = Result.withDefault (NonEmptyString "") lastNameResult
                , ssn = Result.withDefault (SSN "") ssnResult
                , maritalStatus = Maybe.withDefault Single editing.maritalStatus
                , phoneNumber = Result.withDefault (PhoneNumber "") phoneNumberResult
                }
            )

    else
        Err errors
