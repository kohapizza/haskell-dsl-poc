spec myFilter : (Int -> Bool) -> [Int] -> [Int]
  ensures len(output) <= len(input)
