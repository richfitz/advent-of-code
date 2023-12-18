parse_input <- function(path) {
  s <- readLines(path)
  matrix(as.integer(unlist(strsplit(s, NULL))), length(s), byrow = TRUE)
}


translate <- function(n) {
  n2 <- n * n
  list(encode = function(r, c, d) r + (c - 1L) * n + (d - 1L) * n2,
       row = function(x) (x - 1L) %% n + 1L,
       col = function(x) (x - 1L) %% n2 %/% n + 1L,
       dir = function(x) (x - 1L) %/% n2 + 1L)
}


best_path <- function(grid, min_steps, max_steps) {
  ##             west       south      east        north
  move <- list(c(0L, 1L), c(1L, 0L), c(0L, -1L), c(-1L, 0L))
  turn <- list(c(4L, 2L), c(1L, 3L), c(2L, 4L),  c(3L, 1L))
  n <- nrow(grid)
  tr <- translate(n)
  dist <- rep(Inf, n * n * 4)
  ## A very poor priority queue!
  value <- c(0, 0)
  data <- c(tr$encode(1, 1, 1), tr$encode(1, 1, 2))
  
  repeat {
    idx <- which.min(value)
    cost <- value[[idx]]
    x <- data[[idx]]
    value[idx] <- Inf
    r <- tr$row(x)
    c <- tr$col(x)
    d <- tr$dir(x)
    if (r == n && c == n) {
      return(cost)
    }
    if (cost > dist[[x]]) {
      next
    }
    for (d_next in turn[[d]]) {
      delta <- move[[d_next]]
      cost_next <- cost
      for (i in seq_len(max_steps)) {
        r_next <- r + delta[[1]] * i
        c_next <- c + delta[[2]] * i
        if (r_next < 1 || r_next > n || c_next < 1 || c_next > n) {
          break
        }
        cost_next <- cost_next + grid[r_next, c_next]
        if (i <= min_steps) { # 1, 2, 3 rejected for min_steps = 3
          next
        }
        x_next <- tr$encode(r_next, c_next, d_next)
        if (cost_next < dist[[x_next]]) {
          dist[[x_next]] <- cost_next
          value[[length(value) + 1]] <- cost_next
          data[[length(data) + 1]] <- x_next
        }
      }
    }
  }
}


part1 <- function(grid) {
  best_path(grid, 0, 3)
}


part2 <- function(grid) {
  best_path(grid, 3, 10)
}


options(error=recover)
test <- parse_input("test-input-d17.txt")
stopifnot(part1(test) == 102)
stopifnot(part2(test) == 94)

d <- parse_input("input-d17.txt")
part1(d) # 1260
part2(d) # 1416
