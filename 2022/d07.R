parse_input <- function(filename) {
  contents <- list("/" = list(.s = 0L))
  p <- character(0)
  for (el in strsplit(readLines(filename), " ")) {
    if (el[[1]] == "$" && el[[2]] == "cd") {
      p <- if (el[[3]] == "..") head(p, -1) else c(p, el[[3]])
    } else if (el[[1]] == "dir") {
      contents[[c(p, el[[2]])]] <- list(.s = 0L)
    } else if (el[[1]] != "$") {
      contents[[p]]$.s <- contents[[p]]$.s + as.integer(el[[1]])
    }
  }
  dir_size(contents[[1]], "/")
}

dir_size <- function(contents, path) {
  sub <- Filter(is.recursive, contents)
  nms <- file.path(if ((path) == "/") "" else path, names(sub))
  sub_size <- unlist(Map(dir_size, sub, nms, USE.NAMES = FALSE))
  c(setNames(contents$.s + sum(sub_size[nms]), path), sub_size)
}

part1 <- function(size) {
  sum(size[size <= 100000])
}

part2 <- function(size) {
  min(size[size >= 30000000 - (70000000 - size[["/"]])])
}

test <- parse_input("test-input-d07.txt")
stopifnot(part1(test) == 95437)
stopifnot(part2(test) == 24933642)

d <- parse_input("input-d07.txt")
part1(d)
part2(d)
