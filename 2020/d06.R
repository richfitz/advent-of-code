read <- function(path) {
  d <- readLines(path)
  unname(lapply(split(d, cumsum(!nzchar(d))), function(x)
    strsplit(x[nzchar(x)], "")))
}

f1 <- function(d) {
  sum(vapply(d, function(x) length(unique(unlist(x))), integer(1)))
}

f2 <- function(d) {
  sum(vapply(d, function(x) length(Reduce(intersect, x, x[[1]])), integer(1)))
}

d <- read("test-input-d06.txt")
f1(d)
f2(d)

d <- read("input-d06.txt")
part1 <- f1(d)
part2 <- f2(d)
message(sprintf("Part 1: %s\nPart 2: %s", part1, part2))
