parse_input <- function(path) {
  d <- grep("\\[", readLines(path), value = TRUE)
  matrix(lapply(gsub("\\(", "list(", chartr("[]", "()", d)),
                function(x) eval(parse(text = x))), 2)
}

compare <- function(a, b) {
  switch(is.list(a) * 2L + is.list(b) + 1L,
         switch(sign(a - b) + 2L, TRUE, NULL, FALSE),
         compare(list(a), b),
         compare(a, list(b)),
         {
           for (i in seq_len(min(length(a), length(b)))) {
             if (!is.null(x <- compare(a[[i]], b[[i]]))) {
               return(x)
             }
           }
           compare(length(a), length(b))
         })
}

part1 <- function(d) {
  sum(which(apply(d, 2, function(el) compare(el[[1L]], el[[2L]]))))
}

## Originally, this included an insertion sort with a custom
## comparison operator, which was fun but fairly slow because it
## requires O(n^2) comparisons. However, we only need to know where
## these two extra items fit into the entire list, so it's sufficient
## to ask how many they are bigger then - just O(n). Then offset the
## index of the first by 1 (that's where it is) and the second by 2
## (because it sorts higher than the first).
part2 <- function(d) {
  (sum(vapply(d, compare, TRUE, list(list(2)))) + 1) *
    (sum(vapply(d, compare, TRUE, list(list(6)))) + 2)
}

test <- parse_input("test-input-d13.txt")
stopifnot(part1(test) == 13)
stopifnot(part2(test) == 140)

d <- parse_input("input-d13.txt")
part1(d)
part2(d)
