module Validation exposing (..)

import Dict exposing (Dict)
import Validation.Expect as Expect


type alias Test a =
    { message : String
    , runner : a -> Expect.Expectation
    }


test : String -> (a -> Expect.Expectation) -> Test a
test =
    Test


type alias Group a =
    { key : String
    , tests : List (Test a)
    }


describe : String -> List (Test a) -> Group a
describe =
    Group


type alias Validator a =
    { groups : List (Group a)
    }


create : List (Group a) -> Validator a
create =
    Validator


validate : Validator a -> a -> Dict String (List String)
validate validator value =
    List.foldl
        (\group errors ->
            let
                results =
                    List.filterMap
                        (\nextTest ->
                            case nextTest.runner value of
                                Expect.Pass ->
                                    Nothing

                                Expect.Fail ->
                                    Just nextTest.message
                        )
                        group.tests
            in
            if List.isEmpty results then
                errors

            else
                Dict.insert group.key results errors
        )
        Dict.empty
        validator.groups
