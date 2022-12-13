parse <- function(path) {
  txt <- readLines(path)
  dat <- matrix(unlist(strsplit(txt, "[^a-z]+")), ncol = 14, byrow = TRUE)
  list(input = dat[, 1:10, drop = FALSE], output = dat[, 11:14, drop = FALSE])
}


part1 <- function(dat) {
  sum(nchar(dat$output) %in% c(2, 3, 4, 7))
}


unscramble <- function(input) {
  input <- strsplit(input, NULL)
  len <- lengths(input)
  pos <- letters[1:7]

  one <- input[[which(len == 2)]]
  four <- input[[which(len == 4)]]
  seven <- input[[which(len == 3)]]
  mix6 <- input[len == 6]

  map <- setNames(character(7), pos)
  map[["a"]] <- setdiff(seven, one)
  bd <- setdiff(four, one)
  cde <- vapply(mix6, setdiff, "", x = pos)
  map[["d"]] <- intersect(cde, bd)
  map[["b"]] <- setdiff(bd, map[["d"]])
  ce <- setdiff(cde, map[["d"]])
  map[["c"]] <- intersect(ce, one)
  map[["e"]] <- setdiff(ce, map[["c"]])
  fg <- setdiff(pos, map)
  map[["f"]] <- intersect(fg, one)
  map[["g"]] <- setdiff(fg, map[["f"]])

  map
}


digits_to_int <- function(x) {
  sum(10^(length(x) - seq_along(x)) * x)
}


translate <- function(input, map) {
  digits <- c("abcefg", "cf", "acdeg", "acdfg", "bcdf",
              "abdfg", "abdefg", "acf", "abcdefg", "abcdfg")
  input_split <- strsplit(input, NULL)
  fixed <- vapply(input_split, function(x)
    paste(sort(names(map)[match(x, map)]), collapse = ""), "")
  digits_to_int(match(fixed, digits) - 1)
}


part2 <- function(dat) {
  sum(vapply(seq_len(nrow(dat$input)), function(i)
    translate(dat$output[i, ], unscramble(dat$input[i, ])), double(1)))
}


dat <- parse("test-input-d08-1.txt")

map <- unscramble(dat$input[1, ])
stopifnot(translate(dat$input[1, ], map) == 8523796401)
stopifnot(translate(dat$output[1, ], map) == 5353)
stopifnot(part2(dat) == 5353)

dat <- parse("test-input-d08-2.txt")
stopifnot(part1(dat) == 26)
stopifnot(part2(dat) == 61229)

dat <- parse("input-d08.txt")
part1(dat)
part2(dat)
