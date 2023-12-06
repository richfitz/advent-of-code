part1 <- function(d) {
  n <- paste0(sub(".*?([0-9]).*", "\\1", d), sub(".*([0-9]).*", "\\1", d))
  sum(as.integer(n))
}

part2 <- function(d) {
  w <- c("one", "two", "three", "four", "five", "six", "seven", "eight", "nine")
  pat <- sprintf("[0-9]|%s", paste(w, collapse = "|"))
  n <- cbind(sub(sprintf(".*?(%s).*", pat), "\\1", d, perl = TRUE),
             sub(sprintf(".*(%s).*", pat), "\\1", d, perl = TRUE))
  i <- match(n, w)
  n[!is.na(i)] <- i[!is.na(i)]
  sum(as.integer(paste0(n[, 1], n[, 2])))
}

test1 <- readLines("test-input-d01-1.txt")
stopifnot(part1(test1) == 142)
test2 <- readLines("test-input-d01-2.txt")
stopifnot(part2(test2) == 281)

d <- readLines("input-d01.txt")
part1(d)
part2(d)
