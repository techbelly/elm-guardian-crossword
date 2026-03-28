module ListExtra exposing (findIndex, nextInList, prevInList)


nextInList : a -> List a -> Maybe a
nextInList current list =
    case list of
        [] ->
            Nothing

        first :: _ ->
            nextInListHelper current list first


nextInListHelper : a -> List a -> a -> Maybe a
nextInListHelper current list wrapValue =
    case list of
        [] ->
            Nothing

        item :: rest ->
            if item == current then
                case rest of
                    next :: _ ->
                        Just next

                    [] ->
                        Just wrapValue

            else
                nextInListHelper current rest wrapValue


prevInList : a -> List a -> Maybe a
prevInList current list =
    case list of
        [] ->
            Nothing

        _ ->
            prevInListHelper current list (lastOf list)


prevInListHelper : a -> List a -> Maybe a -> Maybe a
prevInListHelper current list prev =
    case list of
        [] ->
            Nothing

        item :: rest ->
            if item == current then
                prev

            else
                prevInListHelper current rest (Just item)


lastOf : List a -> Maybe a
lastOf list =
    case list of
        [] ->
            Nothing

        [ x ] ->
            Just x

        _ :: rest ->
            lastOf rest


findIndex : a -> List a -> Maybe Int
findIndex target list =
    findIndexHelper 0 target list


findIndexHelper : Int -> a -> List a -> Maybe Int
findIndexHelper i target list =
    case list of
        [] ->
            Nothing

        x :: rest ->
            if x == target then
                Just i

            else
                findIndexHelper (i + 1) target rest
