
type A = A Bool

unpackParams (A bool1) (A bool2) =
  False

paramAs (A _ as a) =
  True
