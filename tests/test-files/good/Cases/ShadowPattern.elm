
type Maybe a
  = Nothing
  | Just a

always a b =
  a

stuff =
  always () (\_ ->
    let
      maybe = Nothing
    in
      case maybe of
        Just maybe -> Just maybe
        nothing -> nothing
  )
