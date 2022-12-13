parse <- function(txt) {
  d <- as.integer(strsplit(txt, "[^-0-9]+")[[1]][-1])
  list(x = sort(d[1:2]), y = rev(sort(d[3:4])))
}


## for part 1 we will go up with velocity vy, and spend 2 * vy
## timesteps there, reaching a height of vy * (vy + 1) / 2, then will
## take a single very long step down to within the target area.  This
## means that if we have distances (d1, d2) to the top and bottom of
## the area we're happy to have velocity (-vy - 1) of [-d1, -d2] at
## that point, and we'll have spent a total of 2 * vy + 1 steps on the
## trajectory.
part1 <- function(d) {
  vy <- -d$y[[2]] - 1
  n <- 2 * vy + 1
  vy * (vy + 1) / 2
}


fx <- function(vx, target) {
  value <- cumsum(rev(seq_len(vx)))
  i <- value >= target[1] & value <= target[2]
  valid <- any(i)
  list(valid = valid,
       index = which(i),
       stopped = valid && i[[length(i)]])
}


max_n <- function(vy, target) {
  vy <- max(vy, 0)
  dist <- vy * (vy + 1) / 2 - target
  0.5 * (sqrt(8 * dist + 1) - 1) + vy + 1
}


shoot <- function(xx, vy, target) {
  n <- ceiling(max_n(vy, d$y[[2]]))
  y <- cumsum(seq(vy, by = -1, length.out = n))
  i <- which(y <= d$y[[1]] & y >= d$y[[2]])
  if (length(i) == 0) {
    return(FALSE)
  }
  if (xx$stopped) {
    any(i >= xx$index[[1]])
  } else {
    any(i >= xx$index[[1]] & i <= xx$index[[length(xx$index)]])
  }
}


part2 <- function(d) {
  tot <- 0
  vy_min <- d$y[[2]]
  vy_max <- -d$y[[2]] - 1
  vy_range <- vy_min:vy_max
  for (vx in 0:d$x[[2]]) {
    xx <- fx(vx, d$x)
    if (!xx$valid) {
      next
    }
    for (vy in vy_range) {
      if (shoot(xx, vy, d)) {
        tot <- tot + 1
      }
    }
  }
  tot
}

d <- parse("target area: x=20..30, y=-10..-5")
stopifnot(part1(d) == 45)
stopifnot(part2(d) == 112)

d <- parse("target area: x=34..67, y=-215..-186")
part1(d)
part2(d)
