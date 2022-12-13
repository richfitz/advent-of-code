parse <- function(txt) {
  as.integer(strsplit(txt, ",")[[1]])
}


calculate <- function(dat, tr) {
  pos <- seq(min(dat), max(dat))
  min(colSums(tr(abs(outer(dat, pos, "-")))))
}


part1 <- function(dat) {
  calculate(dat, identity)
}


part2 <- function(dat) {
  calculate(dat, function(x) x * (x + 1) / 2)
}


dat <- parse("16,1,2,0,4,2,7,1,2,14")
stopifnot(part1(dat) == 37)
stopifnot(part2(dat) == 168)

dat <- parse(readLines("input-d07.txt"))
part1(dat)
part2(dat)
