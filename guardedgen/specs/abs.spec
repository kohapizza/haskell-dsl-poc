spec myAbs : Int -> Int
  ensures output >= 0
  ensures output == input || output == negate(input)
