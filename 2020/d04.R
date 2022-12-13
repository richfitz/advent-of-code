read <- function(file) {
  d <- readLines(file)
  cols <- c("byr", "cid", "ecl", "eyr", "hcl", "hgt", "iyr", "pid")
  f <- function(x) {
    xx <- strsplit(strsplit(trimws(paste(x, collapse = " ")), " ")[[1]], ":")
    kv <- matrix(unlist(xx), 2)
    setNames(kv[2, match(cols, kv[1, ])], cols)
  }
  ret <- do.call(rbind, lapply(split(d, cumsum(!nzchar(d))), f))
  rownames(ret) <- NULL
  ret
}

between <- function(x, min, max) {
  x >= min & x <= max
}

valid_height <- function(h) {
  valid <- rep(FALSE, length(h))

  re_cm <- "^([0-9]+)cm$"
  i_cm <- grepl(re_cm, h)
  valid[i_cm] <- between(as.integer(sub(re_cm, "\\1", h[i_cm])), 150, 193)

  re_in <- "^([0-9]+)in$"
  i_in <- grepl(re_in, h)
  valid[i_in] <- between(as.integer(sub(re_in, "\\1", h[i_in])), 59, 76)

  valid
}

validate <- function(d) {
  rowSums(is.na(d[, colnames(d) != "cid"])) == 0 &
    between(as.integer(d[, "byr"]), 1920, 2002) &
    between(as.integer(d[, "iyr"]), 2010, 2020) &
    between(as.integer(d[, "eyr"]), 2020, 2030) &
    valid_height(d[, "hgt"]) &
    grepl("^#[[:xdigit:]]{6}$", d[, "hcl"]) &
    d[, "ecl"] %in% c("amb", "blu", "brn", "gry", "grn", "hzl", "oth") &
    grepl("^[0-9]{9}$", d[, "pid"])
}

## Part1
d <- read("input-d04.txt")
part1 <- sum(rowSums(is.na(d[, colnames(d) != "cid"])) == 0)

## Part2
part2 <- sum(validate(d))
message(sprintf("Part 1: %s\nPart 2: %s", part1, part2))
