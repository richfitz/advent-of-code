parse <- function(path) {
  d <- readLines(path)
  d <- d[nzchar(d)]
  i <- grep("^---", d)
  f <- function(i, j) {
    matrix(as.integer(unlist(strsplit(d[i:j], ","))), ncol = 3, byrow = TRUE)
  }
  Map(f, i + 1, c(i[-1] - 1, length(d)))
}


manhattan_distance <- function(x) {
  unname(as.matrix(dist(x, "manhattan")))
}


detect <- function(a, b) {
  order <- offset <- scale <- rep(NA_integer_, 3)

  for (j in 1:3) {
    for (k in which(is.na(order))) {
      if (all(diff(b[, j] - a[, k]) == 0)) {
        scale[k] <- 1
      } else if (all(diff(-b[, j] - a[, k]) == 0)) {
        scale[k] <- -1
      }
      if (!is.na(scale[k])) {
        order[k] <- j
        offset[k] <- a[1, k] - scale[k] * b[1, j]
        break
      }
    }
  }

  list(order = order, offset = offset, scale = scale)
}


align <- function(test, target, d_target) {
  d_test <- manhattan_distance(test)

  i_sub <- which(rowSums(array(d_target %in% d_test, dim(d_target))) >= 12)
  d_target_sub <- d_target[i_sub, i_sub, drop = FALSE]
  if (nrow(d_target_sub) < 12) {
    return(NULL)
  }

  m <- apply(d_test, 1, function(x)
    rowSums(array(d_target_sub %in% x, dim(d_target_sub))))
  i <- which(m >= 12, TRUE)
  if (nrow(i) < 12) {
    return(NULL)
  }

  dat <- detect(target[i_sub[i[, 1]], ], test[i[, 2], ])
  p <- test[-i[, 2], dat$order]
  n <- nrow(p)
  p <- p * rep(dat$scale, each = n) + rep(dat$offset, each = n)

  list(offset = dat$offset, points = p)
}


assemble <- function(x) {
  offset <- matrix(0, length(x), 3)
  pending <- rep(TRUE, length(x))
  pending[[1]] <- FALSE
  p <- x[[1]]
  d <- manhattan_distance(p)

  while (any(pending)) {
    for (i in which(pending)) {
      el <- x[[i]]
      res <- align(el, p, d)
      if (!is.null(res)) {
        pending[[i]] <- FALSE
        offset[i, ] <- res$offset
        p <- rbind(p, res$points)
        d <- manhattan_distance(p)
      }
    }
  }

  list(points = p, offset = offset)
}


part1 <- function(res) {
  nrow(res$points)
}


part2 <- function(res) {
  max(manhattan_distance(res$offset))
}


x <- parse("test-input-d19-2.txt")
res <- assemble(x)
stopifnot(part1(res) == 79)
stopifnot(part2(res) == 3621)

x <- parse("input-d19.txt")
res <- assemble(x)
part1(res)
part2(res)
