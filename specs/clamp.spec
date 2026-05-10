spec clamp : Int -> Int -> Int -> Int
  requires lo <= hi
  ensures output >= lo
  ensures output <= hi
