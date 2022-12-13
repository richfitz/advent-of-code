parse <- function(d) {
  re <- "^(.+) bags contain (.+)$"
  parent <- sub(re, "\\1", d)
  re2 <- "^([0-9]+) (.+)$"
  contents <- strsplit(gsub(" bags?\\.?", "", sub(re, "\\2", d)), ", ")
  contents <- lapply(contents, function(x)
    if (identical(x, "no other")) {
      NULL
    } else {
      setNames(as.integer(sub(re2, "\\1", x)), sub(re2, "\\2", x))
    })
  setNames(contents, parent)
}

f1 <- function(d, what = "shiny gold") {
  found <- character(0)
  while (length(what) > 0) {
    what <- setdiff(
      names(which(vapply(d, function(x) any(what %in% names(x)), logical(1)))),
      found)
    found <- c(found, what)
  }
  length(found)
}

f2 <- function(d, what = "shiny gold") {
  f <- function(i) {
    if (is.null(d[[i]])) {
      return(1L)
    } else {
      1L + sum(vapply(names(d[[i]]), f, integer(1)) * d[[i]])
    }
  }
  f(what) - 1L
}

f1(parse(readLines("test-input-d071.txt")))
f2(parse(readLines("test-input-d071.txt")))
f2(parse(readLines("test-input-d072.txt")))

d <- parse(readLines("input-d07.txt"))
part1 <- f1(d)
part2 <- f2(d)
message(sprintf("Part 1: %s\nPart 2: %s", part1, part2))
