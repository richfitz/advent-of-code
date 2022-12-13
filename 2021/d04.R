parse <- function(path) {
  txt <- readLines(path)
  order <- as.integer(strsplit(txt[[1]], ",")[[1]])
  boards <- as.integer(unlist(strsplit(trimws(txt[-1]), " +")))
  boards <- array(boards, c(5, 5, length(boards) / 25))
  list(order = order, boards = boards)
}


score <- function(board, seen, x) {
  sum(board[!seen]) * x
}


play <- function(dat) {
  first <- NULL
  boards <- dat$boards
  seen <- array(FALSE, dim(boards))
  for (x in dat$order) {
    seen[boards == x] <- TRUE
    won <- apply(apply(seen, c(1, 3), all) | apply(seen, c(2, 3), all), 2, any)
    if (any(won)) {
      i <- which(won)
      if (length(won) == 1) {
        last <- score(boards[, , i], seen[, , i], x)
        break
      } else {
        if (is.null(first)) {
          first <- score(boards[, , i], seen[, , i], x)
        }
        boards <- boards[, , -i, drop = FALSE]
        seen <- seen[, , -i, drop = FALSE]
      }
    }
  }

  list(first = first, last = last)
}


part1 <- function(res) {
  res$first
}


part2 <- function(res) {
  res$last
}


dat <- parse("test-input-d04.txt")
res <- play(dat)
stopifnot(part1(res) == 4512)
stopifnot(part2(res) == 1924)


dat <- parse("input-d04.txt")
res <- play(dat)
part1(res)
part2(res)
