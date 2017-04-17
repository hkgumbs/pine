
type Count = Zero | OneAnd Count

(+) x y =
    x

f count =
    let
        traverse count =
            case count of
                Zero -> 0
                OneAnd more -> 1 + traverse more
    in
        traverse count
