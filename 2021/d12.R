parse <- function(path) {
  d <- matrix(unlist(strsplit(readLines(path), "-")), ncol = 2, byrow = TRUE)
  nms <- unique(c(d))
  small <- !grepl("^[A-Z]+$", nms)
  near <- lapply(nms, function(x)
    setdiff(c(d[rowSums(d == x) > 0, ]), c(x, "start")))
  setNames(Map(list, name = nms, small = small, near = near), nms)
}


descend <- function(cave, d, chain, visited, allow_revisit) {
  if (cave$name == "end") {
    return(1L)
  }

  if (cave$small) {
    allow_revisit <- allow_revisit && !(cave$name %in% chain)
    chain <- c(chain, cave$name)
    chain_key <- paste(chain, collapse = "-")
  } else {
    chain_key <- paste(c(chain, cave$name), collapse = "-")
  }

  if (!is.null(visited[[chain_key]])) {
    return(visited[[chain_key]])
  }

  near <- if (allow_revisit) cave$near else setdiff(cave$near, chain)
  res <- sum(vapply(d[near], descend, d, chain, visited, allow_revisit,
                    FUN.VALUE = integer(1)))

  visited[[chain_key]] <- res

  res
}


explore <- function(d, allow_revisit) {
  visited <- new.env(parent = emptyenv())
  descend(d$start, d, character(0), visited, allow_revisit)
}


part1 <- function(d) {
  explore(d, FALSE)
}


part2 <- function(d) {
  explore(d, TRUE)
}


stopifnot(part1(parse("test-input-d12-1.txt")) == 10)
stopifnot(part1(parse("test-input-d12-2.txt")) == 19)
stopifnot(part1(parse("test-input-d12-3.txt")) == 226)
stopifnot(part2(parse("test-input-d12-1.txt")) == 36)
stopifnot(part2(parse("test-input-d12-2.txt")) == 103)
stopifnot(part2(parse("test-input-d12-3.txt")) == 3509)


d <- parse("input-d12.txt")
part1(d)
part2(d)
