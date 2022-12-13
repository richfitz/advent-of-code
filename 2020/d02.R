txt <- readLines("input-d02.txt")
re <- "^([0-9]+)-([0-9]+) ([a-z]): ([a-z]+)$"
pw_n1 <- as.integer(sub(re, "\\1", txt))
pw_n2 <- as.integer(sub(re, "\\2", txt))
pw_char <- sub(re, "\\3", txt)
pw_str <- strsplit(sub(re, "\\4", txt), NULL)

is_valid1 <- function(c, s, min, max) sum(c == s) >= min && sum(c == s) <= max
is_valid2 <- function(c, s, a, b) xor(s[[a]] == c, s[[b]] == c)

part1 <- sum(mapply(is_valid1, pw_char, pw_str, pw_n1, pw_n2))
part2 <- sum(mapply(is_valid2, pw_char, pw_str, pw_n1, pw_n2))

message(sprintf("Part 1: %s\nPart 2: %s", part1, part2))
