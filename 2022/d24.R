parse_input <- function(path) {
  d <- readLines(path)
  w <- nchar(d[[1]]) - 2L
  h <- length(d) - 2L
  m <- matrix(unlist(strsplit(d, NULL)), length(d), byrow = TRUE)
  direction <- c(">" = 0 + 1i, "<" = 0 - 1i, "v" = 1 + 0i, "^" = -1 + 0i)
  is_wind <- which(matrix(m %in% names(direction), nrow(m)), TRUE)
  is_wind <- is_wind[order(is_wind[, 1], is_wind[, 2]), ]
  list(w = w, h = h, allowed = m != "#",
       start = 1 + 2i, end = complex(1, h + 2, w + 1),
       at = complex(nrow(is_wind), is_wind[, 1], is_wind[, 2]),
       dir = unname(direction[m[is_wind]]))
}

update_wind <- function(i, d) {
  at <- d$at + d$dir * i
  i <- cbind((Re(at) - 2) %% d$h + 2, (Im(at) - 2) %% d$w + 2)
  m <- d$allowed
  m[i] <- FALSE
  m
}

run <- function(from, to, i, d) {
  impossible <- c(d$start - 1, d$end + 1)
  repeat {
    v <- setdiff(unique(c(outer(from, moves, "+"))), impossible)
    from <- v[update_wind(i, d)[cbind(Re(v), Im(v))]]
    if (to %in% from) {
      return(i)
    }
    i <- i + 1L
  }
}

part1 <- function(d) {
  run(d$start, d$end, 1L, d)
}

part2 <- function(d) {
  i <- run(d$start, d$end, 1L, d)
  i <- run(d$end, d$start, i + 1, d)
  run(d$start, d$end, i + 1, d)
}

moves <- c(0 + 0i, 1 + 0i, -1 + 0i, 0 - 1i, 0 + 1i)
d <- parse_input("test-input-d24.txt")
stopifnot(part1(d) == 18)
stopifnot(part2(d) == 54)

d <- parse_input("input-d24.txt")
part1(d)
part2(d)
