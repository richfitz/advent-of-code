parse_input <- function(path) {
  strsplit(readLines(path), ",", fixed = TRUE)[[1]]
}


hash1a <- function(s) {
  value <- 0
  for (char in utf8ToInt(s)) {
    value <- ((value + char) * 17) %% 256
  }
  value
}


part1 <- function(d) {
  sum(vapply(d, hash1a, numeric(1)))
}


part2 <- function(d) {
  boxes <- rep(list(integer()), 256)
  for (s in strsplit(d, "(?=[-=])", perl = TRUE)) {
    label <- hash1a(s[[1]]) + 1
    if (length(s) == 3) {
      boxes[[label]][[s[[1]]]] <- as.integer(s[[3]])
    } else if (any(s[[1]] == names(boxes[[label]]))) {
      boxes[[label]][[s[[1]]]] <- NA_integer_
    }
  }
  sum(seq_along(boxes) *
      vapply(boxes, function(b) sum(na.omit(b) * seq_len(sum(!is.na(b)))), 1))
}


stopifnot(hash1a("rn=1") == 30)
stopifnot(hash1a("cm-") == 253)
test <- parse_input("test-input-d15.txt")
stopifnot(part1(test) == 1320)
stopifnot(part2(test) == 145)

d <- parse_input("input-d15.txt")
part1(d)
part2(d)
