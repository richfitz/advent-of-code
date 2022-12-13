parse <- function(path) {
  m <- matrix(unlist(strsplit(readLines(path), " ", fixed = TRUE)), 2)
  unname(Map(function(name, value) list(name = name, value = value),
             m[1, ], as.integer(m[2, ])))
}

run <- function(d) {
  state <- c(pos = 1L, acc = 0L)
  seen <- logical(length(d))
  repeat {
    i <- state[["pos"]]
    if (i > length(d)) {
      return(list(terminated = TRUE, value = state[["acc"]]))
    }
    if (seen[[i]]) {
      return(list(terminated = FALSE, value = state[["acc"]]))
    }
    seen[[i]] <- TRUE
    state <- state + switch(d[[i]]$name,
                            nop = c(1L, 0L),
                            acc = c(1L, d[[i]]$value),
                            jmp = c(d[[i]]$value, 0L))
  }
}

f1 <- function(d) {
  run(d)$value
}

f2 <- function(d) {
  check <- function(i) {
    if (d[[i]]$name == "nop") {
      d[[i]]$name <- "jmp"
      run(d)
    } else if (d[[i]]$name == "jmp") {
      d[[i]]$name <- "nop"
      run(d)
    } else {
      list(terminated = FALSE, value = NA_integer_)
    }
  }
  for (i in seq_along(d)) {
    ans <- check(i)
    if (ans$terminated) {
      return(ans$value)
    }
  }
}

f1(parse("test-input-d08.txt"))
f2(parse("test-input-d08.txt"))

part1 <- f1(parse("input-d08.txt"))
part2 <- f2(parse("input-d08.txt"))
message(sprintf("Part 1: %s\nPart 2: %s", part1, part2))
