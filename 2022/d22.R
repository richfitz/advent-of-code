## See rich.md for an explanation of this bit of carryon
info <- function(to, facing) {
  to <- matrix(match(strsplit(to, NULL)[[1L]], LETTERS[1:6]), 4)
  facing <- matrix(match(strsplit(facing, NULL)[[1]], c("r", "d", "l", "u")), 4)
  lapply(1:6, function(i) Map(list, face = to[, i], facing = facing[, i]))
}

extract_test <- list(n = 4,  loc = c(7, 2, 5, 8, 9, 12))
extract_real <- list(n = 50, loc = c(5, 3, 4, 6, 9, 7))

info_test <- list(info("ADAECBDBDCBCBECAFAFDEFEF", "rdlurdlurdlurdlurdlurdlu"),
                  info("FDCBCEFADEBAFECAFBCDABED", "ldddruudrrlrddluruuulrll"))
info_real <- list(info("EDEFFCFCCBCBDFDAAEAEBABD", "rdlurdlurdlurdlurdlurdlu"),
                  info("EDBCFCADFEABEFBAFDACECBD", "rdrrrdrrudduuddulllulllu"))

parse_input <- function(path, info) {
  d <- readLines(path)

  x <- strsplit(grep("[.#]", d, value = TRUE), NULL)
  nc_max <- max(lengths(x))
  map <- t(vapply(x, function(el) c(el == ".", logical(nc_max - length(el))),
                  logical(nc_max)))

  n <- info$n
  m <- nrow(map) / n
  idx <- array(seq_along(map), dim(map))
  cube <- lapply(info$loc, function(i) {
    map[n * ((i - 1L) %% m) + seq_len(n), n * ((i - 1L) %/% m) + seq_len(n)]
  })
  index <- lapply(info$loc, function(i) {
    idx[n * ((i - 1L) %% m) + seq_len(n), n * ((i - 1L) %/% m) + seq_len(n)]
  })

  p <- strsplit(d[[length(d)]], "(?!\\d)", perl = TRUE)[[1L]]
  p <- unname(Map(list, is_move = grepl("^[0-9]+$", p), value = p))

  list(cube = cube, index = index, path = p, map = map)
}

## This is where almost all the pain comes from; we need a nice way of
## translating a position as we wrap around the edges. We'll feed this
## function an over-wrapped number so one of 'at' will be either 0 or
## n + 1
rotate <- function(at, from, to, n) {
  switch((from - 1L) * 4L + to,
         c(at[[1L]], 1L),          # rr
         c(1L, n - at[[1L]] + 1L), # rd
         c(n - at[[1L]] + 1L, n),  # rl
         c(n, at[[1L]]),           # ru
         stop("unused"),           # dr
         c(1L, at[[2L]]),          # dd
         c(at[[2L]], n),           # dl
         c(n, n - at[[2L]] + 1L),  # du
         c(n - at[[1L]] + 1L, 1L), # lr
         c(1L, at[[1L]]),          # ld
         c(at[[1L]], n),           # ll
         stop("unused"),           # lu
         c(at[[2L]], 1L),          # ur
         stop("unused"),           # ud
         stop("unused"),           # ul
         c(n, at[[2L]]))           # uu
}

move <- function(pos, n, cube, transition) {
  horiz <- pos$facing == 1L || pos$facing == 3L
  at <- pos$at
  k <- at[[horiz + 1]]
  x <- if (horiz) cube[[pos$face]][at[[1L]], ] else cube[[pos$face]][, at[[2L]]]
  dk <- if (pos$facing == 1L || pos$facing == 2L) 1L else -1L
  for (step in seq_len(n)) {
    k_next <- k + dk
    if (k_next == 0 || k_next > nrow(cube[[1]])) {
      to <- transition[[pos$face]][[pos$facing]]
      at_next <- rotate(at, pos$facing, to$facing, nrow(cube[[1]]))
      if (cube[[to$face]][[at_next[[1L]], at_next[[2L]]]]) {
        pos_next <- list(face = to$face, at = at_next, facing = to$facing)
        return(if (n - step == 0)
                 pos_next else move(pos_next, n - step, cube, transition))
      }
      break
    } else if (!x[[k_next]]) {
      break
    } else {
      k <- k_next
    }
  }
  pos$at[[if (horiz) 2 else 1]] <- k
  pos
}

run <- function(d, info) {
  pos <- list(face = 1L, at = c(1, which(d$cube[[1]])[[1]]), facing = 1L)
  turn <- list(R = c(2L, 3L, 4L, 1L), L = c(4L, 1L, 2L, 3L))
  for (el in d$path) {
    if (el$is_move) {
      pos <- move(pos, as.integer(el$value), d$cube, info)
    } else {
      pos$facing <- turn[[el$value]][[pos$facing]]
    }
  }
  i <- d$index[[pos$face]][pos$at[[1]], pos$at[[2]]]
  ((i - 1L) %% nrow(d$map) + 1L) * 1000 + ((i - 1L) %/% nrow(d$map) + 1L) * 4 +
    pos$facing - 1L
}

part1 <- function(d, info) {
  run(d, info[[1L]])
}

part2 <- function(d, info) {
  run(d, info[[2L]])
}

test <- parse_input("test-input-d22.txt", extract_test)
stopifnot(part1(test, info_test) == 6032)
stopifnot(part2(test, info_test) == 5031)

d <- parse_input("input-d22.txt", extract_real)
part1(d, info_real)
part2(d, info_real)
