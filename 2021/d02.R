parse <- function(path) {
  dat <- matrix(unlist(strsplit(readLines(path), " ")), 2)
  data.frame(dir = dat[1, ], n = as.integer(dat[2, ]),
             stringsAsFactors = FALSE)
}


part1 <- function(dat) {
  x <- sum((dat$dir == "forward") * dat$n)
  y <- sum((dat$dir == "down") * dat$n) - sum((dat$dir == "up") * dat$n)
  x * y
}


part2 <- function(dat) {
  aim <- x <- y <- 0
  for (i in seq_len(nrow(dat))) {
    if (dat$dir[i] == "forward") {
      x <- x + dat$n[i]
      y <- y + aim * dat$n[i]
    } else {
      dir <- if (dat$dir[i] == "down") 1 else -1
      aim <- aim + dir * dat$n[i]
    }
  }
  x * y
}


dat <- parse("test-input-d02.txt")
stopifnot(part1(dat) == 150)
stopifnot(part2(dat) == 900)

dat <- parse("input-d02.txt")
part1(dat)
part2(dat)
