## Crazy idea adapted from:
## https://gist.github.com/a-ponomarev/eadf5e4305960729cb54cfe5b461245d
##
## ...except that here we turn them into hex strings, as it appears
## that the distress signals are at most depth 5 and there are 11
## numbers, so it feels like this is really what we're being asked
## for.
##
## This solution has no parsing, no recursion, no sort, and is
## extremely quick (not that the full solution is very slow!)
##
## The translation table is:
##
##   12345          1
##   ,,,,,01234567890
##   0123456789abcdef
##
## Where the first 5 entries are commas at depth 1, 2, 3, 4 and 5; the
## remaining 11 items are numbers 0 through 10. Once we compute the
## depth of the nesting we can drop the braces entirely.
##
## With this, the examples are:
##
##   [1,1,3,1,1]                 => 606080606
##   [1,1,5,1,1]                 => 6060a0606
##                                      ^- 8 < a so ok
##
##   [[1],[2,3,4]]               => 6071819
##   [[1],4]                     => 609
##                                    ^- 7 < 9 so ok
##
##   [9]                         => e
##   [[8,7,6]]                   => d1c1b
##                                  ^- e > d so error
##
##   [[4,4],4,4]                 => 9190909
##   [[4,4],4,4,4]               => 919090909
##                                         ^ (empty) < 0 so ok
##
##   [7,7,7,7]                   => c0c0c0c
##   [7,7,7]                     => c0c0c
##                                       ^ 0 > (empty) so error
##
##   []                          => (empty string)
##   [3]                         => 8
##                                  ^ (empty) > 8 so ok
##
##   [[[]]]                      => (empty string)
##   [[]]                        => (empty string)
##                                  ^ (empty) == (empty) so error(*)
##
##   [1,[2,[3,[4,[5,6,7]]]],8,9] => 60718293a4b4c0d0e
##   [1,[2,[3,[4,[5,6,0]]]],8,9] => 60718293a4b450d0e
##                                              ^- c > 5 so error
##
## (*) Case 7 is a bit of a trick really as we do lose information
## here so this might not hold for all ~snailfish numbers~ distress
## signals. It works because we test left < right and here that's
## FALSE...
parse_input <- function(path) {
  txt <- grep("[", readLines(path), fixed = TRUE, value = TRUE)
  txt <- chartr("0123456789", "56789abcde", gsub("10", "f", txt, fixed = TRUE))
  matrix(vapply(strsplit(txt, NULL), normalise, ""), 2)
}

normalise <- function(x) {
  d <- (x == "[") - (x == "]")
  i <- x == ","
  x[i] <- cumsum(d)[i] - 1L
  paste(x[d == 0], collapse = "")
}

part1 <- function(d) {
  sum(which(d[1, ] < d[2, ]))
}

part2 <- function(d) {
  ## [[2]] -> "7" and [[6]] -> "b" from the rules above
  (sum(d < "7") + 1) * (sum(d < "b") + 2)
}

test <- parse_input("test-input-d13.txt")
stopifnot(part1(test) == 13)
stopifnot(part2(test) == 140)

d <- parse_input("input-d13.txt")
part1(d)
part2(d)
