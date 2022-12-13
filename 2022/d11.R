fn <- function(expr) {
  as.function(c(alist(old = ), expr), envir = baseenv()) # nolint (lintr bug!)
}

parse_input <- function(path) {
  txt <- matrix(c(readLines(path), ""), 7)
  numbers <- matrix(as.integer(gsub(".* ", "", txt[4:6, ])), 3)
  list(op = lapply(parse(text = sub(".*= ", "", txt[3, ])), fn),
       items = lapply(strsplit(sub(".*: ", "", txt[2L, ]), ", "), as.integer),
       test = numbers[1, ], y = numbers[2, ] + 1L, n = numbers[3, ] + 1L)
}

run <- function(state, n_rounds, worry_adjust) {
  list2env(state, environment()) # naughty, but nice
  tot <- integer(length(items))
  for (. in seq_len(n_rounds)) {
    for (i in seq_along(items)) {
      x <- worry_adjust(op[[i]](items[[i]]))
      t <- x %% test[[i]] == 0L
      items[[y[[i]]]] <- c(items[[y[[i]]]], x[t])
      items[[n[[i]]]] <- c(items[[n[[i]]]], x[!t])
      items[[i]] <- integer()
      tot[[i]] <- tot[[i]] + length(x)
    }
  }
  prod(tail(sort(tot), 2))
}

part1 <- function(state) {
  run(state, 20, function(x) x %/% 3)
}

part2 <- function(state) {
  div <- prod(state$test)
  run(state, 10000, function(x) x %% div)
}

test <- parse_input("test-input-d11.txt")
stopifnot(part1(test) == 10605)
stopifnot(part2(test) == 2713310158)

d <- parse_input("input-d11.txt")
part1(d)
part2(d)
