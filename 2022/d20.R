parse_input <- function(path) {
  as.integer(readLines(path))
}

circle <- function(data) {
  nodes <- vector("list", length(data))
  for (i in seq_along(data)) {
    nodes[[i]] <- list2env(list(value = data[[i]]), parent = emptyenv())
  }
  for (i in seq_along(nodes)) {
    nodes[[i]]$up <- if (i == length(data)) nodes[[1L]] else nodes[[i + 1L]]
    nodes[[i]]$down <- if (i == 1L) nodes[[length(nodes)]] else nodes[[i - 1L]]
  }
  list(data = nodes, zero = nodes[[match(0, data)]])
}

circle_find <- function(x, n) {
  if (n < 0) {
    for (i in seq_len(-n)) {
      x <- x$down
    }
  } else {
    for (i in seq_len(n)) {
      x <- x$up
    }
  }
  x
}

circle_move_node <- function(x, n) {
  if (n == 0) {
    return()
  }
  x$down$up <- x$up
  x$up$down <- x$down
  y <- if (n > 0) circle_find(x$up, n - 1L) else circle_find(x$down, n)
  x$down <- y
  x$up <- y$up
  y$up$down <- x
  y$up <- x
}

decrypt <- function(d) {
  len <- length(d$data) - 1L
  for (x in d$data) {
    circle_move_node(x, nearest(x$value, len))
  }
  d$zero
}

part1 <- function(d) {
  sum(extract(decrypt(circle(d)), length(d)))
}

part2 <- function(d) {
  obj <- circle(d * 811589153)
  for (i in 1:10) {
    z <- decrypt(obj)
  }
  format(sum(extract(z, length(d))), digits = 20)
}

extract <- function(x, len) {
  ret <- integer(3)
  for (i in 1:3) {
    ret[[i]] <- (x <- circle_find(x, 1000 %% len))$value
  }
  ret
}

nearest <- function(n, len) {
  ret <- n %% len
  if (2 * ret > len) ret - len else ret
}

d <- parse_input("test-input-d20.txt")
stopifnot(part1(d) == 3)
stopifnot(part2(d) == 1623178306)

d <- parse_input("input-d20.txt")
part1(d) # 9866
part2(d) # 12374299815791
