parse_input <- function(path) {
  str <- readLines(path)
  m <- matrix(unlist(strsplit(str, NULL)), length(str), byrow = TRUE)
  rocks <- unname(which(m != ".", TRUE))
  list(n = length(str), rocks = rocks, moves = m[rocks] == "O")
}

show <- function(n, rocks, moves) {
  m <- matrix(".", n, n)
  m[rocks] <- ifelse(moves, "O", "#")
  cat(paste0(apply(m, 1, paste, collapse = ""), "\n", collapse = ""))
}

tilt <- function(n, rocks, moves, direction) {
  idx <- 1L + as.integer(direction %in% c("west", "east"))
  down <- direction %in% c("north", "west")
  base <- rep(if (down) 1L else n, n) # 1L * (!down ) * n
  for (k in order(rocks[, idx], decreasing = !down)) {
    i <- rocks[k, idx]
    j <- rocks[k, 3L - idx]
    if (moves[k]) {
      rocks[k, idx] <- base[[j]]
      base[[j]] <- base[[j]] + (if (down) 1L else -1L)
    } else {
      base[[j]] <- i + (if (down) 1L else -1L)
    }
  }
  rocks
}

spin <- function(n, rocks, moves) {
  for (direction in c("north", "west", "south", "east")) {
    rocks <- tilt(n, rocks, moves, direction)
  }
  rocks
}

score <- function(n, rocks, moves) {
  sum(n - rocks[moves, 1] + 1)
}

part1 <- function(d) {
  score(d$n, tilt(d$n, d$rocks, d$moves, "north"), d$moves)
}

part2 <- function(d) {
  hash <- function(rocks) {
    paste(sort(rocks[moves, 1] + (rocks[moves, 2] - 1) * n), collapse = "-")
  }
  n <- d$n
  rocks <- d$rocks
  moves <- d$moves
  seen <- character(500)
  load <- numeric(500)
  for (i in seq_along(load)) {
    rocks <- spin(n, rocks, moves)
    x <- rocks[moves, ]
    h <- hash(rocks)
    if (h %in% seen) {
      burnin <- i - 1L
      period <- burnin - match(h, seen) + 1L
      k <- (burnin - period + 1)
      return(load[[((1000000000 - k) %% period) + k]])
    }
    seen[[i]] <- h
    load[[i]] <- score(n, rocks, moves)
  }
}

test <- parse_input("test-input-d14.txt")
stopifnot(part1(test) == 136)
stopifnot(part2(test) == 64)

d <- parse_input("input-d14.txt")
part1(d)
part2(d)
