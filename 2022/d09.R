parse_input <- function(path) {
  d <- readLines(path)
  rep(substr(d, 1, 1), as.integer(substr(d, 3, nchar(d))))
}

moves <- list(R = 1 + 0i, L = -1 + 0i, U = 0 + 1i, D = 0 - 1i)

move_rope <- function(rope, move) {
  for (i in seq_along(rope)) {
    if (i == 1L) {
      rope[[1L]] <- rope[[1L]] + moves[[move]]
    } else {
      d <- rope[[i - 1L]] - rope[[i]]
      if (abs(d) >= 2) {
        rope[[i]] <-  rope[[i]] + complex(1L, sign(Re(d)), sign(Im(d)))
      }
    }
  }
  rope
}

rope_on_a_plane <- function(d, n) {
  rope <- rep(0 + 0i, n)
  seen <- complex(length(d))
  for (i in seq_along(d)) {
    rope <- move_rope(rope, d[[i]])
    seen[[i]] <- rope[[n]]
  }
  length(unique(seen))
}

part1 <- function(d) {
  rope_on_a_plane(d, 2)
}

part2 <- function(d) {
  rope_on_a_plane(d, 10)
}

test1 <- parse_input("test-input-d09-1.txt")
stopifnot(part1(test1) == 13)
test2 <- parse_input("test-input-d09-2.txt")
stopifnot(part2(test2) == 36)

d <- parse_input("input-d09.txt")
part1(d)
part2(d)
