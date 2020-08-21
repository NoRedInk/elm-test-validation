module Validation.Expect exposing
    ( Expectation(..)
    , all
    , atLeast
    , atMost
    , equal
    , equalDicts
    , equalLists
    , equalSets
    , err
    , fail
    , false
    , greaterThan
    , lessThan
    , notEqual
    , ok
    , pass
    , true
    )

import Dict exposing (Dict)
import Set exposing (Set)


type Expectation
    = Pass
    | Fail


true : Bool -> Expectation
true =
    equateWith "Expect.true" (==) True


false : Bool -> Expectation
false =
    equateWith "Expect.false" (==) False


equal : a -> a -> Expectation
equal =
    equateWith "Expect.equal" (==)


notEqual : a -> a -> Expectation
notEqual =
    equateWith "Expect.notEqual" (/=)


lessThan : comparable -> comparable -> Expectation
lessThan =
    compareWith "Expect.lessThan" (<)


atMost : comparable -> comparable -> Expectation
atMost =
    compareWith "Expect.atMost" (<=)


greaterThan : comparable -> comparable -> Expectation
greaterThan =
    compareWith "Expect.greaterThan" (>)


atLeast : comparable -> comparable -> Expectation
atLeast =
    compareWith "Expect.atLeast" (>=)


ok : Result a b -> Expectation
ok result =
    case result of
        Ok _ ->
            Pass

        Err _ ->
            Fail


err : Result a b -> Expectation
err result =
    case result of
        Ok _ ->
            Fail

        Err _ ->
            Pass


equalLists : List a -> List a -> Expectation
equalLists expected actual =
    if expected == actual then
        Pass

    else
        Fail


equalDicts : Dict comparable a -> Dict comparable a -> Expectation
equalDicts expected actual =
    if Dict.toList expected == Dict.toList actual then
        pass

    else
        let
            differ dict k v diffs =
                if Dict.get k dict == Just v then
                    diffs

                else
                    ( k, v ) :: diffs

            missingKeys =
                Dict.foldr (differ actual) [] expected

            extraKeys =
                Dict.foldr (differ expected) [] actual
        in
        Fail


equalSets : Set comparable -> Set comparable -> Expectation
equalSets expected actual =
    if Set.toList expected == Set.toList actual then
        pass

    else
        let
            missingKeys =
                Set.diff expected actual
                    |> Set.toList

            extraKeys =
                Set.diff actual expected
                    |> Set.toList
        in
        Fail


pass : Expectation
pass =
    Pass


fail : Expectation
fail =
    Fail


all : List (subject -> Expectation) -> subject -> Expectation
all list query =
    if List.isEmpty list then
        Fail

    else
        allHelp list query


allHelp : List (subject -> Expectation) -> subject -> Expectation
allHelp list query =
    case list of
        [] ->
            pass

        check :: rest ->
            case check query of
                Pass ->
                    allHelp rest query

                outcome ->
                    outcome


{-| String arg is label, e.g. "Expect.equal".
-}
equateWith : String -> (a -> b -> Bool) -> b -> a -> Expectation
equateWith reason comparison b a =
    let
        isJust x =
            case x of
                Just _ ->
                    True

                Nothing ->
                    False
    in
    testWith reason comparison b a


compareWith : String -> (a -> b -> Bool) -> b -> a -> Expectation
compareWith =
    testWith


testWith : String -> (a -> b -> Bool) -> b -> a -> Expectation
testWith label runTest expected actual =
    if runTest actual expected then
        Pass

    else
        Fail
