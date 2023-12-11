## This is a direct python-to-R translation of my python solution, so
## it does not feel fabulously R-ish, but now that I have my solution
## it's hard to see another way through it!
move <- list(north = c(-1, 0), south = c(1, 0), east = c(0, 1), west = c(0, -1))
turn <- list(north = list("|" = "north", "F" = "east",  "7" = "west"),
             south = list("|" = "south", "L" = "east",  "J" = "west"),
             east  = list("-" = "east",  "J" = "north", "7" = "south"),
             west  = list("-" = "west",  "L" = "north", "F" = "south"))
             
             
parse_input <- function(path) {
  map <- readLines(path)
  map <- matrix(unlist(strsplit(map, NULL)), length(map), byrow = TRUE)
  loop <- find_loop(c(which(map == "S", TRUE)), map)

  key <- paste(loop[nrow(loop), ] - loop[2, ] + 2, collapse = "")
  start <- c("24" = "-", "20" = "-", "42" = "|", "02" = "|",
             "13" = "F", "11" = "7", "31" = "J", "33" = "L")
  map[loop[1, , drop = FALSE]] <- start[[key]]

  map_clean <- array(NA_character_, dim(map))
  map_clean[loop] <- map[loop]

  list(loop = loop, map = map_clean)
}

find_loop <- function(start, map) {
  loop <- integer(2 * length(map)) # avoid concatenation in the loop
  for (direction in c("north", "south", "east", "west")) {
    tryCatch({
      pos <- start
      for (i in seq.int(1, 2 * length(map), 2)) {
        loop[c(i, i + 1L)] <- pos
        pos <- pos + move[[direction]]
        if (all(pos == start)) {
          return(matrix(loop[seq_len(i + 1)], ncol = 2, byrow = TRUE))
        }
        direction <- turn[[direction]][[map[pos[[1]], pos[[2]]]]]
      }
    }, error = function(e) NULL)
  }
}

part1 <- function(d) {
  nrow(d$loop) / 2
}

part2 <- function(d) {
  n_inside <- 0L
  map <- d$map
  for (i in seq_len(nrow(map))) {
    is_inside <- FALSE
    for (ch in map[i, ]) {
      if (is.na(ch)) {
        n_inside <- n_inside + is_inside
      } else if (ch == "|") {
        is_inside <- !is_inside
      } else if (ch == "F" || ch == "L") {
        start <- ch
      } else if ((ch == "7" && start == "L") || (ch == "J" && start == "F")) {
        is_inside = !is_inside
      }
    }
  }
  n_inside
}

stopifnot(part1(parse_input("test-input-d10-1.txt")) == 4)
stopifnot(part1(parse_input("test-input-d10-2.txt")) == 4)
stopifnot(part1(parse_input("test-input-d10-3.txt")) == 8)
stopifnot(part1(parse_input("test-input-d10-4.txt")) == 8)
stopifnot(part2(parse_input("test-input-d10-5.txt")) == 4)
stopifnot(part2(parse_input("test-input-d10-6.txt")) == 8)
stopifnot(part2(parse_input("test-input-d10-7.txt")) == 10)

d <- parse_input("input-d10.txt")
part1(d)
part2(d)
