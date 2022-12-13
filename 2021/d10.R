
parse_line <- function(x) {
  ## Mapping of symbol to enum so that we have an open/close property
  ## using negation
  map <- c("(" = -1, ")" = 1,
           "[" = -2, "]" = 2,
           "{" = -3, "}" = 3,
           "<" = -4, ">" = 4)
  stack <- integer(length(x))
  ptr <- 0L
  x <- unname(map[x])
  for (i in x) {
    if (i < 0) { # open
      ptr <- ptr + 1L
      stack[[ptr]] <- i
    } else if (i == -stack[[ptr]]) { # close and valid
      ptr <- ptr - 1L
    } else { # close and invalid
      return(list(valid = FALSE, value = i))
    }
  }
  list(valid = TRUE, value = stack[seq_len(ptr)])
}


parse <- function(txt) {
  res <- lapply(strsplit(txt, NULL), parse_line)
  valid <- vapply(res, "[[", logical(1), "valid")
  list(valid = lapply(res[valid], "[[", "value"),
       invalid = vapply(res[!valid], "[[", numeric(1), "value"))
}


part1 <- function(dat) {
  cost <- c(3, 57, 1197, 25137) # in same order as map
  sum(cost[dat$invalid])
}


part2 <- function(dat) {
  score <- vapply(dat$valid, function(x)
    sum(5^(seq_along(x) - 1) * -x), numeric(1))
  median(score)
}


dat <- parse(readLines("test-input-d10.txt"))
stopifnot(part1(dat) == 26397)
stopifnot(part2(dat) == 288957)

dat <- parse(readLines("input-d10.txt"))
part1(dat)
part2(dat)
