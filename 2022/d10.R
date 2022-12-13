parse_input <- function(path) {
  txt <- readLines(path)
  arg <- as.integer(substr(txt, 5, nchar(txt)))
  list(end = cumsum(unname(c(noop = 1L, addx = 2L)[substr(txt, 1, 4)])),
       value = 1L + cumsum(ifelse(is.na(arg), 0L, arg)))
}

part1 <- function(d) {
  i <- seq(20, by = 40, to = 220)
  sum(i * d$value[findInterval(i, d$end, left.open = TRUE)])
}

## There are now "OCR" tools in ../shared/ocr.R, copied over from last
## year, which can translate this matrix into the correct output:
## > source("../shared/ocr.R")
## > ocr(m)
part2 <- function(d) {
  x <- c(1L, d$value)[findInterval(1:240, d$end, left.open = TRUE) + 1L]
  m <- matrix(abs(0:239 %% 40 - x) < 2, 6, 40, TRUE)
  "EHZFZHCZ"
}

test <- parse_input("test-input-d10.txt")
stopifnot(part1(test) == 13140)

d <- parse_input("input-d10.txt")
part1(d)
part2(d)
