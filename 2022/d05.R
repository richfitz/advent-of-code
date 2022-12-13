parse_input <- function(path) {
  txt <- readLines(path)
  state <- grep("\\[", txt, value = TRUE)
  state <- lapply(seq(2, by = 4, to = nchar(tail(state, 1))), function(i) {
    rev(Filter(function(x) x != " ", substr(state, i, i)))
  })
  moves <- sub("move ", "", grep("move", txt, value = TRUE))
  moves <- lapply(strsplit(moves, "[^0-9]+"), function(x) {
    setNames(as.list(as.integer(x)), c("n", "from", "to"))
  })
  list(state = state, moves = moves)
}

move <- function(state, move, f) {
  state[[move$to]] <- c(state[[move$to]], f(tail(state[[move$from]], move$n)))
  state[[move$from]] <- head(state[[move$from]], -move$n)
  state
}

run <- function(d, f) {
  state <- Reduce(function(s, m) move(s, m, f), d$moves, d$state)
  paste(vapply(state, tail, "", 1), collapse = "")
}

part1 <- function(d) {
  run(d, rev)
}

part2 <- function(d) {
  run(d, identity)
}

test <- parse_input("test-input-d05.txt")
stopifnot(part1(test) == "CMZ")
stopifnot(part2(test) == "MCD")

d <- parse_input("input-d05.txt")
part1(d)
part2(d)
