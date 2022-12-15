parse_input <- function(path) {
  d <- t(vapply(
    strsplit(gsub("(^[^0-9]+|[^ 0-9-])", "", readLines(path)), " +"),
    as.integer, numeric(4)))
  i <- order(d[, 1])
  x <- d[i, 1]
  y <- d[i, 2]
  list(x = x, y = y, r = abs(x - d[i, 3]) + abs(y - d[i, 4]))
}

part1 <- function(d, i = 2000000) {
  delta <- d$r - abs(i - d$y)
  j <- delta > 0
  diff(range(c(d$x[j] - delta[j], d$x[j] + delta[j])))
}

## Geometry-based solution for p2, with comments as it's a bit hard to
## follow. It is though quite quick.
part2 <- function(d) {
  ## Find all possiblities which have a gap between their search
  ## distances of exactly 2; just enough to hide a space in:
  pos <- which(as.matrix(dist(cbind(d$x, d$y), "manhattan")) -
               outer(d$r, d$r, "+") == 2L, TRUE, FALSE)
  ## The *indices* of x and y coordinates of our distinct set of
  ## candidates; in the real example there is only one, but in the
  ## test case there are quite a few. Because the above calculation
  ## finds all pairs we'll have (i, j) and (j, i) represented, the
  ## filtering on rows removes this.
  i <- pos[pos[, 1] < pos[, 2], 1]
  j <- pos[pos[, 1] < pos[, 2], 2]

  ## Compute the slope and intercept of the line that slides between
  ## pairs of somewhat close beacons, one for each pair of beacons.
  m <- sign(d$y[i] - d$y[j])
  c <- d$y[i] - m * (d$x[i] + d$r[i] + 1)

  ## Then find the coordinates of the intersections by solving the
  ## pairs of equations. Only lines of opposite sign intersect, so
  ## look at the combinations of positive sloping lines with negative
  ## sloping lines as those are our candidate positions.  There are
  ## some duplicates here in the test case, we can ignore this while
  ## only doing a little extra work; de-duplicating them is more
  ## effort than just testing them all, especially as we are only
  ## interested in the first valid point.
  x <- c(-outer(c[m == 1], c[m == -1], "-") / 2)
  y <- c[m == 1] + x

  ## Finally, check to see that no beacon is close to these points. In
  ## the test case this is necessary but not on my input which has
  ## just a single point. To do this make sure that the Manhattan
  ## distance between our candidate points (x, y) and beacons (d$x,
  ## d$y) is more than the search distance of each beacon (d$r); doing
  ## this gives a matrix with n_beacons rows and n_candidate columns,
  ## and we care about cases where the entire column satisfies this
  ## condition (i.e., the column representing a candidate who is out
  ## of the reach of every beacon). In the test case there will be
  ## multiple copies of the true solution here, so we need to take
  ## the first element, but there is only one distinct solution
  ## really.
  ok <- abs(outer(d$x, x, "-")) + abs(outer(d$y, y, "-")) > d$r
  k <- which(apply(ok, 2, all))[[1]]

  format(4000000 * x[k] + y[k], digits = 20)
}

test <- parse_input("test-input-d15.txt")
stopifnot(part1(test, 10) == 26)
stopifnot(part2(test) == "56000011")

d <- parse_input("input-d15.txt")
part1(d)
part2(d)
