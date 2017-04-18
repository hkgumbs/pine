
infiniteCountDown number noOp =
  case number of
    0 -> 0
    1 -> (\_ -> infiniteCountDown number noOp) noOp
    _ -> infiniteCountDown number noOp


withLet =
  let
    infiniteCountDown number =
      case number of
        0 -> 0
        _ -> infiniteCountDown number
  in
    infiniteCountDown 1


withLetCurried =
  let
    infiniteCountDown number =
      case number of
        0 -> \_ -> 0
        _ -> \_ -> infiniteCountDown number ()
  in
    infiniteCountDown 1 ()
