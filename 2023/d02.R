parse <- function(path) {
  f <- function(x) {
    rgb <- c("red", "green", "blue")
    g <- function(el) {
      el <- strsplit(el, " ", fixed = TRUE)
      as.integer(vapply(el, "[[", "", 1))[match(rgb, vapply(el, "[[", "", 2))]
    }
    ret <- vapply(strsplit(x, ", ", fixed = TRUE), g, integer(3),
                  USE.NAMES = FALSE)
    ret[is.na(ret)] <- 0
    ret
  }
  d <- strsplit(sub(".*: ", "", readLines(path)), "; ", fixed = TRUE)
  lapply(d, f)
}

part1 <- function(d) {
  sum(which(vapply(d, function(g) all(g <= 12:14), logical(1))))
}

part2 <- function(d) {
  sum(vapply(d, function(g) prod(apply(g, 1, max)), numeric(1)))
}

test <- parse("test-input-d02.txt")
stopifnot(part1(test) == 8)
stopifnot(part2(test) == 2286)

d <- parse("input-d02.txt")
part1(d)
part2(d)
