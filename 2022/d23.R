parse_input <- function(path) {
  i <- lapply(strsplit(readLines(path), NULL), function(x) which(x == "#"))
  complex(length(unlist(i)), rep(seq_along(i), lengths(i)), unlist(i))
}

delta <- c(-1 - 1i, -1 + 0i, -1 + 1i, 0 - 1i, 0 + 1i, 1 - 1i,  1 + 0i,  1 + 1i)
check <- list(
  list(c(1, 2, 3), -1 + 0i), list(c(6, 7, 8),  1 + 0i),
  list(c(1, 4, 6),  0 - 1i), list(c(3, 5, 8),  0 + 1i))

## The bulk of the time here is doing the big lookup between the
## proposed sites and the occupied sites. You can do this slightly
## more efficiently by computing some normalised coordinates for the
## two parts (proposed and occupied) that spans the current space,
## filling that logical vector with TRUE where it is occupied, then
## looking it up with the proposed sites.
##
## The conversion looks like
##
##   x[Re(a) - min_real + 1 + (Im(a) - min_imag) * real_range]
##
## where real_range is the number of rows required to fill the real
## range. However, it's not *that* much faster and much uglier.
update <- function(v, check_order) {
  is_empty <- !matrix(outer(v, delta, "+") %in% v, length(v))
  proposed <- v
  for (i in seq_along(v)) {
    k <- is_empty[i, ]
    if (!all(k)) {
      for (j in check_order) {
        if (all(k[check[[j]][[1]]])) {
          proposed[[i]] <- v[[i]] + check[[j]][[2]]
          break
        }
      }
    }
  }
  ok <- !(proposed %in% proposed[duplicated(proposed)])
  v[ok] <- proposed[ok]
  v
}

part1 <- function(v) {
  for (i in 1:10) {
    v <- update(v, (0:3 + i - 1L) %% 4L + 1L)
  }
  (diff(range(Re(v))) + 1) * (diff(range(Im(v))) + 1) - length(v)
}

part2 <- function(v) {
  for (i in seq_len(1e6)) {
    v_next <- update(v, (0:3 + i - 1L) %% 4L + 1L)
    if (identical(v_next, v)) {
      return(i)
    }
    v <- v_next
  }
}

v <- parse_input("test-input-d23-2.txt")
stopifnot(part1(v) == 110)
stopifnot(part2(v) == 20)

v <- parse_input("input-d23.txt")
part1(v)
part2(v)
