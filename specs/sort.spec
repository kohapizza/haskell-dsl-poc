spec sort : [Int] -> [Int]
  ensures sorted(output)
  ensures permutation(output, input)
  ensures len(output) == len(input)
