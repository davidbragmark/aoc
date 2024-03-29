namespace AdventOfCode.Input

module Day02 =
  let input : (string * int64) [] =
    [|
      "forward", 9L
      "forward", 7L
      "forward", 4L
      "down", 7L
      "forward", 5L
      "down", 4L
      "down", 2L
      "forward", 1L
      "down", 6L
      "forward", 5L
      "forward", 5L
      "forward", 8L
      "forward", 3L
      "forward", 6L
      "down", 2L
      "up", 3L
      "forward", 1L
      "up", 4L
      "forward", 1L
      "forward", 6L
      "up", 2L
      "forward", 7L
      "up", 2L
      "up", 3L
      "down", 9L
      "up", 5L
      "down", 5L
      "up", 7L
      "down", 5L
      "forward", 4L
      "forward", 1L
      "forward", 8L
      "forward", 9L
      "forward", 3L
      "forward", 9L
      "down", 1L
      "down", 1L
      "down", 1L
      "forward", 6L
      "up", 8L
      "down", 2L
      "forward", 3L
      "down", 9L
      "forward", 7L
      "down", 5L
      "up", 7L
      "down", 3L
      "forward", 5L
      "forward", 9L
      "down", 9L
      "up", 3L
      "forward", 4L
      "forward", 8L
      "up", 2L
      "forward", 4L
      "forward", 2L
      "forward", 2L
      "forward", 8L
      "up", 7L
      "up", 4L
      "down", 3L
      "forward", 2L
      "down", 9L
      "up", 1L
      "forward", 2L
      "down", 3L
      "forward", 2L
      "down", 2L
      "up", 6L
      "forward", 6L
      "forward", 2L
      "down", 9L
      "forward", 3L
      "forward", 2L
      "down", 1L
      "up", 1L
      "down", 1L
      "forward", 5L
      "forward", 4L
      "up", 6L
      "forward", 6L
      "forward", 2L
      "up", 9L
      "forward", 9L
      "forward", 5L
      "down", 8L
      "up", 9L
      "forward", 2L
      "up", 3L
      "forward", 8L
      "forward", 2L
      "down", 4L
      "down", 1L
      "up", 9L
      "up", 8L
      "forward", 3L
      "forward", 9L
      "down", 9L
      "down", 6L
      "forward", 1L
      "forward", 5L
      "up", 9L
      "down", 7L
      "up", 9L
      "down", 8L
      "down", 2L
      "down", 2L
      "up", 9L
      "forward", 7L
      "down", 4L
      "down", 7L
      "down", 8L
      "down", 9L
      "down", 9L
      "forward", 9L
      "down", 9L
      "forward", 2L
      "down", 6L
      "forward", 8L
      "forward", 1L
      "down", 6L
      "down", 8L
      "forward", 5L
      "forward", 3L
      "forward", 5L
      "down", 5L
      "forward", 6L
      "forward", 3L
      "forward", 4L
      "forward", 3L
      "down", 2L
      "down", 9L
      "down", 8L
      "down", 9L
      "up", 7L
      "up", 9L
      "up", 1L
      "down", 6L
      "down", 4L
      "forward", 8L
      "forward", 5L
      "forward", 8L
      "down", 5L
      "forward", 7L
      "down", 9L
      "forward", 9L
      "forward", 1L
      "up", 4L
      "down", 4L
      "forward", 7L
      "forward", 4L
      "up", 9L
      "forward", 6L
      "down", 8L
      "down", 5L
      "forward", 8L
      "down", 6L
      "down", 3L
      "down", 7L
      "forward", 4L
      "down", 8L
      "down", 1L
      "up", 6L
      "down", 4L
      "down", 9L
      "down", 6L
      "forward", 7L
      "down", 8L
      "forward", 5L
      "forward", 8L
      "down", 7L
      "down", 5L
      "forward", 1L
      "up", 1L
      "down", 1L
      "down", 6L
      "forward", 5L
      "forward", 6L
      "down", 1L
      "up", 6L
      "forward", 4L
      "forward", 6L
      "down", 4L
      "forward", 3L
      "up", 6L
      "forward", 2L
      "forward", 9L
      "down", 8L
      "forward", 8L
      "up", 9L
      "forward", 3L
      "forward", 4L
      "forward", 1L
      "down", 4L
      "down", 5L
      "forward", 4L
      "down", 6L
      "forward", 7L
      "down", 7L
      "down", 8L
      "up", 8L
      "up", 5L
      "down", 8L
      "down", 5L
      "forward", 8L
      "forward", 2L
      "forward", 6L
      "up", 5L
      "forward", 4L
      "forward", 2L
      "forward", 1L
      "up", 7L
      "forward", 1L
      "forward", 6L
      "down", 8L
      "down", 5L
      "down", 4L
      "forward", 2L
      "up", 8L
      "forward", 4L
      "up", 8L
      "forward", 7L
      "forward", 5L
      "down", 4L
      "up", 7L
      "down", 5L
      "down", 3L
      "forward", 2L
      "down", 2L
      "down", 2L
      "forward", 5L
      "forward", 2L
      "down", 2L
      "forward", 5L
      "down", 8L
      "forward", 7L
      "up", 8L
      "down", 6L
      "up", 5L
      "forward", 6L
      "up", 9L
      "down", 2L
      "down", 3L
      "up", 1L
      "up", 8L
      "forward", 9L
      "forward", 7L
      "forward", 9L
      "forward", 3L
      "down", 2L
      "up", 2L
      "down", 2L
      "down", 8L
      "up", 8L
      "up", 6L
      "forward", 6L
      "down", 9L
      "down", 9L
      "up", 4L
      "down", 3L
      "forward", 6L
      "forward", 9L
      "down", 6L
      "forward", 7L
      "forward", 4L
      "forward", 4L
      "down", 9L
      "down", 3L
      "forward", 1L
      "down", 7L
      "forward", 2L
      "forward", 3L
      "forward", 9L
      "forward", 5L
      "forward", 2L
      "forward", 4L
      "forward", 8L
      "up", 1L
      "forward", 5L
      "down", 4L
      "down", 2L
      "down", 7L
      "forward", 1L
      "up", 1L
      "up", 8L
      "up", 6L
      "down", 1L
      "forward", 1L
      "forward", 9L
      "forward", 8L
      "down", 7L
      "forward", 6L
      "forward", 8L
      "down", 7L
      "forward", 5L
      "down", 5L
      "down", 8L
      "down", 8L
      "forward", 8L
      "up", 1L
      "down", 7L
      "down", 4L
      "up", 4L
      "forward", 5L
      "up", 7L
      "forward", 3L
      "forward", 2L
      "down", 1L
      "forward", 3L
      "down", 5L
      "forward", 4L
      "down", 4L
      "forward", 6L
      "up", 9L
      "forward", 3L
      "down", 7L
      "forward", 7L
      "forward", 9L
      "forward", 9L
      "forward", 4L
      "up", 9L
      "up", 5L
      "down", 6L
      "down", 6L
      "forward", 8L
      "up", 6L
      "down", 2L
      "up", 5L
      "forward", 7L
      "forward", 4L
      "down", 6L
      "down", 4L
      "down", 9L
      "down", 4L
      "up", 2L
      "down", 3L
      "down", 7L
      "forward", 1L
      "forward", 4L
      "down", 6L
      "forward", 3L
      "forward", 2L
      "forward", 4L
      "down", 9L
      "forward", 8L
      "down", 3L
      "up", 4L
      "down", 5L
      "forward", 2L
      "down", 6L
      "forward", 8L
      "down", 8L
      "down", 7L
      "down", 4L
      "forward", 1L
      "down", 3L
      "forward", 9L
      "down", 2L
      "down", 9L
      "down", 2L
      "forward", 1L
      "down", 3L
      "down", 2L
      "down", 2L
      "up", 4L
      "down", 8L
      "forward", 6L
      "forward", 4L
      "forward", 4L
      "up", 9L
      "forward", 3L
      "forward", 1L
      "forward", 1L
      "up", 3L
      "forward", 9L
      "down", 2L
      "forward", 5L
      "down", 9L
      "down", 2L
      "forward", 1L
      "forward", 9L
      "down", 3L
      "forward", 3L
      "up", 3L
      "forward", 7L
      "down", 6L
      "up", 8L
      "down", 2L
      "down", 5L
      "forward", 7L
      "down", 8L
      "up", 5L
      "down", 4L
      "up", 5L
      "forward", 6L
      "forward", 3L
      "down", 2L
      "forward", 4L
      "forward", 3L
      "down", 8L
      "forward", 5L
      "forward", 5L
      "down", 5L
      "forward", 1L
      "forward", 8L
      "up", 1L
      "down", 7L
      "forward", 6L
      "forward", 3L
      "forward", 8L
      "down", 9L
      "down", 7L
      "forward", 1L
      "down", 2L
      "down", 6L
      "down", 3L
      "forward", 8L
      "down", 7L
      "forward", 2L
      "forward", 1L
      "forward", 5L
      "down", 9L
      "forward", 2L
      "forward", 2L
      "up", 4L
      "down", 9L
      "down", 4L
      "forward", 7L
      "down", 7L
      "up", 8L
      "forward", 6L
      "down", 9L
      "down", 8L
      "up", 5L
      "down", 8L
      "down", 6L
      "forward", 9L
      "up", 5L
      "up", 7L
      "down", 3L
      "up", 2L
      "down", 4L
      "up", 8L
      "up", 3L
      "down", 7L
      "forward", 9L
      "forward", 7L
      "down", 7L
      "forward", 5L
      "up", 8L
      "forward", 1L
      "down", 2L
      "forward", 8L
      "down", 3L
      "up", 5L
      "down", 9L
      "forward", 8L
      "down", 7L
      "down", 3L
      "down", 3L
      "down", 2L
      "forward", 6L
      "up", 5L
      "forward", 4L
      "down", 4L
      "down", 3L
      "down", 5L
      "forward", 8L
      "down", 3L
      "forward", 7L
      "forward", 2L
      "down", 8L
      "down", 6L
      "down", 9L
      "down", 3L
      "down", 6L
      "down", 7L
      "down", 8L
      "up", 6L
      "down", 7L
      "forward", 8L
      "down", 9L
      "forward", 1L
      "down", 6L
      "forward", 8L
      "down", 5L
      "forward", 3L
      "up", 8L
      "forward", 1L
      "down", 6L
      "forward", 4L
      "forward", 5L
      "forward", 8L
      "up", 5L
      "forward", 4L
      "down", 2L
      "down", 9L
      "up", 2L
      "forward", 1L
      "up", 8L
      "forward", 6L
      "up", 4L
      "up", 6L
      "forward", 4L
      "up", 5L
      "forward", 6L
      "forward", 1L
      "down", 3L
      "down", 6L
      "up", 2L
      "forward", 4L
      "up", 2L
      "forward", 4L
      "forward", 6L
      "down", 2L
      "down", 4L
      "up", 5L
      "down", 9L
      "up", 2L
      "down", 4L
      "up", 6L
      "forward", 3L
      "down", 6L
      "down", 2L
      "up", 8L
      "down", 3L
      "down", 1L
      "forward", 6L
      "forward", 5L
      "forward", 8L
      "down", 4L
      "down", 6L
      "down", 2L
      "forward", 3L
      "down", 3L
      "up", 8L
      "down", 4L
      "forward", 5L
      "down", 6L
      "down", 3L
      "up", 2L
      "forward", 5L
      "forward", 2L
      "down", 6L
      "down", 8L
      "forward", 1L
      "forward", 5L
      "forward", 7L
      "forward", 3L
      "forward", 6L
      "down", 9L
      "forward", 7L
      "forward", 4L
      "down", 6L
      "down", 2L
      "up", 8L
      "down", 3L
      "down", 7L
      "down", 7L
      "down", 9L
      "down", 8L
      "down", 6L
      "down", 6L
      "up", 1L
      "up", 6L
      "forward", 4L
      "down", 8L
      "up", 7L
      "down", 8L
      "forward", 9L
      "down", 9L
      "up", 9L
      "forward", 4L
      "forward", 1L
      "down", 3L
      "down", 8L
      "forward", 9L
      "down", 9L
      "forward", 3L
      "down", 2L
      "forward", 9L
      "down", 2L
      "forward", 8L
      "down", 7L
      "down", 2L
      "forward", 4L
      "forward", 3L
      "forward", 3L
      "down", 8L
      "up", 3L
      "forward", 9L
      "down", 1L
      "down", 6L
      "up", 3L
      "down", 6L
      "up", 7L
      "forward", 9L
      "up", 9L
      "down", 5L
      "forward", 6L
      "up", 1L
      "up", 6L
      "down", 4L
      "forward", 9L
      "forward", 6L
      "forward", 9L
      "down", 4L
      "up", 9L
      "up", 4L
      "forward", 2L
      "forward", 2L
      "forward", 4L
      "up", 6L
      "down", 1L
      "down", 4L
      "forward", 9L
      "down", 9L
      "forward", 3L
      "up", 9L
      "down", 4L
      "forward", 4L
      "down", 1L
      "forward", 8L
      "forward", 2L
      "down", 1L
      "down", 7L
      "down", 8L
      "forward", 1L
      "up", 7L
      "up", 7L
      "forward", 1L
      "down", 3L
      "up", 5L
      "down", 4L
      "forward", 2L
      "down", 5L
      "up", 1L
      "down", 4L
      "forward", 7L
      "down", 2L
      "down", 5L
      "down", 4L
      "forward", 7L
      "forward", 6L
      "up", 9L
      "forward", 6L
      "forward", 1L
      "forward", 7L
      "forward", 5L
      "up", 6L
      "down", 8L
      "forward", 8L
      "down", 9L
      "down", 8L
      "forward", 8L
      "down", 2L
      "down", 5L
      "forward", 8L
      "forward", 9L
      "down", 6L
      "down", 3L
      "down", 3L
      "up", 9L
      "down", 6L
      "forward", 6L
      "up", 2L
      "forward", 9L
      "forward", 7L
      "forward", 6L
      "forward", 4L
      "forward", 1L
      "down", 2L
      "forward", 1L
      "forward", 3L
      "forward", 9L
      "down", 9L
      "forward", 7L
      "forward", 3L
      "down", 8L
      "up", 7L
      "forward", 1L
      "down", 8L
      "up", 5L
      "down", 8L
      "up", 3L
      "down", 7L
      "forward", 2L
      "down", 7L
      "forward", 2L
      "down", 3L
      "forward", 3L
      "forward", 8L
      "down", 4L
      "forward", 6L
      "down", 3L
      "up", 9L
      "forward", 9L
      "up", 6L
      "up", 4L
      "up", 6L
      "down", 1L
      "forward", 3L
      "down", 7L
      "down", 9L
      "up", 9L
      "down", 2L
      "up", 6L
      "forward", 4L
      "down", 4L
      "down", 3L
      "down", 2L
      "down", 6L
      "forward", 1L
      "forward", 1L
      "up", 3L
      "forward", 5L
      "forward", 8L
      "down", 1L
      "up", 4L
      "forward", 3L
      "up", 4L
      "down", 5L
      "up", 7L
      "down", 5L
      "down", 6L
      "forward", 9L
      "forward", 8L
      "forward", 9L
      "down", 6L
      "forward", 5L
      "down", 3L
      "up", 5L
      "down", 7L
      "down", 5L
      "down", 7L
      "up", 9L
      "forward", 3L
      "forward", 4L
      "forward", 1L
      "up", 3L
      "forward", 2L
      "down", 4L
      "up", 9L
      "down", 7L
      "forward", 6L
      "forward", 5L
      "forward", 3L
      "forward", 3L
      "forward", 9L
      "up", 7L
      "down", 9L
      "forward", 4L
      "down", 7L
      "forward", 9L
      "forward", 5L
      "down", 8L
      "up", 2L
      "forward", 2L
      "down", 4L
      "up", 5L
      "up", 4L
      "forward", 5L
      "down", 4L
      "down", 9L
      "down", 7L
      "down", 2L
      "forward", 1L
      "forward", 1L
      "down", 4L
      "down", 8L
      "down", 6L
      "forward", 1L
      "up", 6L
      "up", 3L
      "up", 5L
      "down", 1L
      "down", 5L
      "up", 1L
      "up", 5L
      "forward", 2L
      "up", 2L
      "down", 3L
      "forward", 7L
      "forward", 2L
      "down", 1L
      "down", 9L
      "forward", 1L
      "down", 1L
      "forward", 9L
      "up", 9L
      "down", 9L
      "forward", 9L
      "down", 4L
      "down", 1L
      "up", 5L
      "down", 2L
      "forward", 9L
      "down", 2L
      "up", 3L
      "up", 6L
      "forward", 1L
      "forward", 8L
      "down", 5L
      "down", 8L
      "up", 2L
      "down", 2L
      "up", 4L
      "down", 2L
      "down", 4L
      "forward", 6L
      "up", 4L
      "down", 1L
      "forward", 9L
      "forward", 4L
      "down", 9L
      "up", 7L
      "forward", 7L
      "down", 3L
      "forward", 2L
      "down", 6L
      "up", 6L
      "down", 5L
      "down", 7L
      "forward", 4L
      "forward", 1L
      "forward", 7L
      "forward", 4L
      "forward", 4L
      "up", 2L
      "down", 2L
      "down", 5L
      "forward", 7L
      "down", 6L
      "forward", 8L
      "down", 3L
      "down", 9L
      "forward", 7L
      "forward", 1L
      "down", 2L
      "up", 7L
      "forward", 4L
      "forward", 2L
      "forward", 6L
      "forward", 5L
      "forward", 9L
      "forward", 9L
      "down", 9L
      "down", 9L
      "up", 7L
      "forward", 7L
      "forward", 7L
      "forward", 1L
      "forward", 2L
      "down", 1L
      "down", 4L
      "forward", 7L
      "forward", 5L
      "down", 1L
      "up", 2L
      "forward", 3L
      "forward", 2L
      "forward", 1L
      "forward", 6L
      "down", 4L
      "up", 6L
      "forward", 7L
      "down", 1L
      "forward", 4L
      "up", 6L
      "down", 7L
      "down", 4L
      "forward", 1L
      "down", 8L
      "down", 2L
      "down", 1L
      "down", 8L
      "forward", 4L
      "up", 8L
      "down", 4L
      "up", 9L
      "up", 3L
      "forward", 6L
      "up", 9L
      "down", 1L
      "forward", 3L
      "up", 3L
      "forward", 5L
      "up", 3L
      "down", 6L
      "forward", 9L
      "down", 3L
      "down", 3L
      "up", 5L
      "forward", 5L
      "forward", 8L
      "forward", 9L
      "down", 6L
      "down", 3L
      "forward", 6L
      "up", 4L
      "up", 3L
      "forward", 3L
      "forward", 2L
      "down", 2L
      "up", 9L
      "forward", 4L
      "forward", 6L
      "forward", 2L
      "up", 9L
      "down", 2L
      "forward", 7L
      "down", 7L
      "up", 1L
      "forward", 2L
      "forward", 8L
      "down", 2L
      "down", 6L
      "down", 1L
      "forward", 3L
      "forward", 5L
      "forward", 6L
      "forward", 3L
      "down", 3L
      "down", 7L
      "up", 3L
      "forward", 2L
      "forward", 5L
      "down", 9L
      "forward", 3L
      "down", 9L
      "up", 6L
      "down", 6L
      "forward", 3L
      "down", 5L
      "forward", 1L
      "down", 5L
      "up", 3L
      "forward", 8L
      "forward", 8L
      "down", 5L
      "down", 6L
      "down", 1L
      "forward", 9L
      "forward", 4L
      "forward", 1L
      "forward", 8L
      "down", 8L
      "down", 9L
      "forward", 7L
      "forward", 9L
      "down", 2L
      "down", 6L
      "down", 8L
      "down", 3L
      "forward", 5L
      "forward", 7L
      "forward", 4L
      "down", 9L
      "down", 2L
      "forward", 4L
      "forward", 7L
      "down", 2L
      "down", 7L
      "forward", 8L
      "down", 8L
      "forward", 4L
      "up", 8L
      "forward", 3L
      "forward", 9L
      "forward", 4L
      "down", 9L
      "down", 6L
      "up", 1L
      "down", 3L
      "down", 7L
      "down", 4L
      "forward", 9L
      "forward", 4L
      "up", 9L
      "down", 6L
      "forward", 3L
      "up", 1L
      "down", 8L
      "down", 5L
      "forward", 9L
      "down", 4L
      "down", 2L
      "down", 2L
      "down", 5L
      "up", 5L
      "down", 5L
      "forward", 5L
      "forward", 2L
      "up", 1L
      "forward", 2L
      "up", 2L
      "forward", 8L
      "down", 2L
      "down", 7L
      "forward", 1L
    |]
