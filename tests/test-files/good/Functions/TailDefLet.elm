
help =
  "not me!"

count x =
  let
    help n =
      case n of
        0 -> 0
        _ -> help n
  in
    help x
