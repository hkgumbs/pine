
count n x =
  let shadow = recursive 0
      recursive shadow = count shadow
  in
    case n of
      0 -> shadow x
      _ -> count n x
