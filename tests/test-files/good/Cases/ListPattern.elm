
length list =
  case list of
    []        -> 0
    [a]       -> 1
    a :: rest -> 2
