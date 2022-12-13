source("../shared/ocr.R")

parse <- function(path) {
  txt <- readLines(path)
  i <- which(txt == "")
  dots <- matrix(as.integer(unlist(strsplit(txt[seq_len(i - 1)], ","))),
                 ncol = 2, byrow = TRUE)[, 2:1] # # move from x,y => i,j
  f <- txt[-seq_len(i)]
  re <- "^fold along ([xy])=([0-9]+)$"
  folds <- Map(list,
               along = sub(re, "\\1", f), at = as.integer(sub(re, "\\2", f)))
  list(dots = dots, folds = folds)
}


fold <- function(dots, fold) {
  j <- if (fold$along == "y") 1 else 2
  i <- dots[, j] > fold$at
  dots[i, j] <- 2 * fold$at - dots[i, j]
  unique(dots)
}


part1 <- function(dat) {
  nrow(fold(dat$dots, dat$folds[[1]]))
}


part2 <- function(dat) {
  dots <- dat$dots
  for (f in dat$folds) {
    dots <- fold(dots, f)
  }
  ocr(dots)
}


show <- function(dots, blank = ".") {
  dots <- dots + 1
  m <- matrix(blank, max(dots[, 1]), max(dots[, 2]))
  m[dots] <- "#"
  cat(paste0(apply(m, 1, paste, collapse = ""), "\n", collapse = ""))
}

d <- parse("test-input-d13.txt")
stopifnot(part1(d) == 17)

d <- parse("input-d13.txt")
part1(d)
part2(d)
