parse <- function(d) {
  m <- matrix(unlist(strsplit(d, NULL)), ncol = length(d))
  row <- colSums((m[1:7, ] == "B") * 2^(6:0))
  col <- colSums((m[8:10, ] == "R") * 2^(2:0))
  list(row = row, col = col, id = row * 8 + col)
}

d <- parse(readLines("input-d05.txt"))
part1 <- max(d$id)
part2 <- setdiff(min(d$id):max(d$id), d$id)
message(sprintf("Part 1: %s\nPart 2: %s", part1, part2))
