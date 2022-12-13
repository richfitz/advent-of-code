ocr <- function(input) {
  ## If given as a n x 2 row,column index:
  if (ncol(input) == 2) {
    i <- input[, 1] + 1 - min(input[, 1])
    j <- input[, 2] + 1 - min(input[, 2])
    m <- matrix(FALSE, max(i), max(j))
    m[cbind(i, j)] <- TRUE
    return(ocr(m))
  }

  ## Note that some are unavailable (D, M, N, Q, T, V, W, X)
  letters = c(A = 16290430,
              B = 6969727,
              C = 4855902,
              E = 8804735,
              F = 283007,
              G = 15374430,
              H = 16531775,
              I = 139233,
              J = 8263696,
              K = 8757567,
              L = 8521791,
              O = 8001630,
              P = 1610367,
              R = 10064511,
              S = 4627046,
              U = 8259615,
              Y = 51609859,
              Z = 9329265)

  i <- rle(apply(input, 2, any))
  start <- (cumsum(c(0, i$lengths[-length(i$lengths)])) + 1)[i$values]
  end <- start + i$lengths[i$values] - 1

  res <- character(length(start))
  for (i in seq_along(start)) {
    x <- which(input[, start[i]:end[i]])
    j <- match(as.integer(sum(2^(x - 1))), letters)
    res[[i]] <- if (is.na(j)) "?" else names(letters)[[j]]
  }
  paste(res, collapse = "")
}
