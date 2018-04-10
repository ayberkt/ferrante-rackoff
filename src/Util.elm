module Util exposing (indexOf)

-- Helper function for `indexOf`.


indexOfRec xs t pos =
    case ( xs, t, pos ) of
        ( [], _, _ ) ->
            Nothing

        ( x :: xs, t, pos ) ->
            if x == t then
                Just pos
            else
                indexOfRec xs t (pos + 1)



-- Get the index of an element from a given list.


indexOf : List a -> a -> Maybe Int
indexOf xs t =
    indexOfRec xs t 0
