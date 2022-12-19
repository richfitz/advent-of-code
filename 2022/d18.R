## Explanation for part 2:
##
## Assume that all points are in a equally sized cube with dimension
## 'r'; there are sufficiently few of these in the real problem (20^3
## = 8000) that we can just immediately enumerate them all and compute
## their coordinates (xyz).
##
## Then, to keep the main loop simple, work out their neighbours
## within this list (near) - there could be up to 6 of these (two on
## each of the x, y, z dimensions), but fewer for the points on the
## edge.
##
## After building a big logical vector (air; TRUE where rocks are
## *not*), flood fill with steam from (1, 1, 1) taking care not to
## re-queue places we have either seen already or have already queued
## to see later.
##
## Once we have a vector of air and steam, we can easily find the
## unreachable spots, map them back into (x, y, z) space and compute
## the surface area of this volume. Subtracting this from our original
## count in part1 returns the steam-visible volume only.
##
## Because of this last step, we will use the first surface area
## calculation twice, and it's not actually trivial (~20ms), so store
## it as part of the parsed data.
parse_input <- function(path) {
  m <- unname(as.matrix(read.csv(path, header = FALSE))) + 1L
  list(m = m, surface = surface(m))
}

surface <- function(m) {
  6 * nrow(m) - 2 * (sum(dist(m, method = "manhattan") == 1))
}

part1 <- function(d) {
  d$surface
}

part2 <- function(d) {
  r <- max(d$m)
  xyz <- expand.grid(x = seq_len(r), y = seq_len(r), z = seq_len(r))
  f <- function(a, d) ifelse(a == if (d > 0) r else 1, NA_integer_, a + d)
  idx <- function(x, y, z) x + (y - 1L) * r + (z - 1L) * r * r
  near <- apply(cbind(
    idx(f(xyz$x, -1L), xyz$y, xyz$z), idx(f(xyz$x, 1L), xyz$y, xyz$z),
    idx(xyz$x, f(xyz$y, -1L), xyz$z), idx(xyz$x, f(xyz$y, 1L), xyz$z),
    idx(xyz$x, xyz$y, f(xyz$z, -1L)), idx(xyz$x, xyz$y, f(xyz$z, 1L))),
    1, function(x) x[!is.na(x)])

  air <- rep_len(TRUE, length(near))
  air[idx(d$m[, 1], d$m[, 2], d$m[, 3])] <- FALSE

  q <- 1L
  steam <- pending <- logical(length(near))
  while (length(q) > 0) {
    i <- q[[1L]]
    steam[i] <- TRUE
    j <- near[[i]]
    k <- j[air[j] & !steam[j] & !pending[j]]
    q <- c(q[-1L], k)
    pending[k] <- TRUE
  }

  part1(d) - surface(unname(as.matrix(xyz[air & !steam, ])))
}

d <- parse_input("test-input-d18-1.txt")
stopifnot(part1(d) == 10)
stopifnot(part2(d) == 10)

d <- parse_input("test-input-d18-2.txt")
stopifnot(part1(d) == 64)
stopifnot(part2(d) == 58)

d <- parse_input("input-d18.txt")
part1(d) # 3500
part2(d) # 2048
