## GCD and LCM courtesy of Wikipedia
gcd <- function(a, b) {
  if (a > b) {
    return(gcd(b, a))
  }
  if (a == 0) b else gcd(a, b %% a)
}


lcm <- function(x) {
  Reduce(function(a, b) abs(a * b) %/% gcd(a, b), x, 1)
}


parse_input <- function(path) {
  d <- readLines(path)
  directions <- (strsplit(d[[1L]], NULL)[[1L]] == "R") + 1L
  nodes <- d[-(1:2)]
  nms <- substr(nodes, 1, 3)
  nodes <- cbind(match(substr(nodes, 8, 10), nms),
                 match(substr(nodes, 13, 15), nms))
  list(directions = directions, nodes = nodes, names = nms)
}


part1 <- function(d, start = "AAA", end = "ZZZ") {
  nodes <- d$nodes
  directions <- d$directions
  i <- 0L
  pos <- match(start, d$names)
  end <- match(end, d$names)
  while (!any(pos == end)) {
    pos <- nodes[pos, directions[[i %% length(directions) + 1]]]
    i <- i + 1L
  }
  i
}


part2 <- function(d) {
  start <- grep("A$", d$names, value = TRUE)
  end <- grep("Z$", d$names, value = TRUE)
  format(lcm(vapply(start, part1, numeric(1), d = d, end = end)), digits = 20)
}


test1 <- parse_input("test-input-d08-1.txt")
stopifnot(part1(test1) == 2)

test2 <- parse_input("test-input-d08-2.txt")
stopifnot(part2(test2) == 6)

d <- parse_input("input-d08.txt")
part1(d)
part2(d)
