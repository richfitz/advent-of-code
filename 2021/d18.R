## A simple doubly linked list, created from a set of data
dll_create <- function(x) {
  ret <- lapply(x, dll_node)
  n <- length(x)
  for (i in seq_along(ret)) {
    ret[[i]]$prv <- if (i == 1) NULL else ret[[i - 1]]
    ret[[i]]$nxt <- if (i == n) NULL else ret[[i + 1]]
  }
  ret[[1]]
}

dll_node <- function(data) {
  list2env(data, hash = FALSE, parent = emptyenv())
}

dll_last <- function(x) {
  while (!is.null(x$nxt)) {
    x <- x$nxt
  }
  x
}

dll_join <- function(a, b) {
  a_end <- dll_last(a)
  b$prv <- a_end
  a_end$nxt <- b
  a
}

sn_parse <- function(s) {
  f <- function(s) {
    if (s == "[") {
      sn_node(FALSE, TRUE)
    } else if (s == "]") {
      sn_node(FALSE, FALSE)
    } else {
      sn_node(TRUE, as.integer(s))
    }
  }
  dll_create(lapply(strsplit(gsub(",", "", s), NULL)[[1]], f))
}

sn_node <- function(number, value) {
  list(number = number, value = value)
}

## Purely for display and helping debug:
sn_format <- function(x) {
  ret <- character()
  last <- NULL
  while (!is.null(x)) {
    comma <- if (!is.null(last) && (last$number || !last$value)) ","
    if (x$number) {
      ret <- c(ret, comma, x$value)
    } else if (x$value) {
      ret <- c(ret, comma, "[")
    } else {
      ret <- c(ret, "]")
    }
    last <- x
    x <- x$nxt
  }
  paste(ret, collapse = "")
}

sn_add <- function(a, b) {
  start <- dll_node(sn_node(FALSE, TRUE))
  end   <- dll_node(sn_node(FALSE, FALSE))
  sn_reduce(dll_join(start, dll_join(a, dll_join(b, end))))
}

sn_sum <- function(x) {
  result <- x[[1]]
  for (value in x[-1]) {
    result <- sn_add(result, value)
  }
  result
}

sn_reduce <- function(x) {
  repeat {
    changed <- sn_explode(x)
    if (changed) {
      next
    }
    changed <- sn_split(x)
    if (!changed) {
      break
    }
  }
  x
}

sn_explode <- function(x) {
  find_next_number <- function(x) {
    while (!is.null(x <- x$nxt)) {
      if (x$number) {
        return(x)
      }
    }
    NULL
  }
  
  depth <- 0L
  last_number <- NULL
  while (!is.null(x)) {
    if (!x$number) {
      depth <- depth + (if (x$value) 1L else -1L)
    } else if (depth > 4) {
      y <- x$nxt
      next_number <- find_next_number(y)
      
      if (!is.null(last_number)) {
        last_number$value <- last_number$value + x$value
      }
      if (!is.null(next_number)) {
        next_number$value <- next_number$value + y$value
      }

      new <- dll_node(sn_node(TRUE, 0L))      

      ## Here, it's not super obvious.  We remove 4 nodes ('[', 'x', 'y', ']')
      ##          0123456789
      ##          [[[[xy]]]]
      ##              ^^
      ##      prv    ^  ^    nxt
      ##  prv$prv   ^    ^   nxt$nxt
      head <- x$prv$prv
      tail <- y$nxt$nxt
      head$nxt <- new
      tail$prv <- new
      new$prv <- head
      new$nxt <- tail

      return(TRUE)
    }
    if (x$number) {
      last_number <- x
    }
    x <- x$nxt
  }

  FALSE # unchanged
}

sn_split <- function(x) {
  while (!is.null(x)) {
    if (x$number && x$value >= 10) {
      n <- x$value / 2
      new <- dll_create(list(sn_node(FALSE, TRUE),
                             sn_node(TRUE, floor(n)),
                             sn_node(TRUE, ceiling(n)),
                             sn_node(FALSE, FALSE)))
      head <- x$prv
      tail <- x$nxt
      head$nxt <- new
      new$prv <- head
      new <- dll_last(new)
      new$nxt <- tail
      tail$prv <- new
      return(TRUE)
    }
    x <- x$nxt
  }
  FALSE
}

sn_magnitude <- function(x) {
  first <- x
  while (!is.null(first$nxt)) {
    while (!is.null(x)) {
      if (x$number && isTRUE(x$prv$number)) {
        value <- x$prv$value * 3 + x$value * 2
        head <- x$prv$prv
        head$number <- TRUE
        head$value <- value
        head$nxt <- x$nxt$nxt
        x$nxt$nxt$prv <- head
        x <- head
      } else {
        x <- x$nxt
      }
    }
  }
  first$value
}

parse <- function(path) {
  lapply(readLines(path), sn_parse)
}

part1 <- function(d) {
  sn_magnitude(sn_sum(lapply(d, sn_parse)))
}

part2 <- function(d) {
  n <- length(d)
  idx <- expand.grid(i = 1:n, j = 1:n)
  idx <- idx[idx$i != idx$j, ]
  f <- function(i, j) {
    sn_magnitude(sn_add(sn_parse(d[[i]]), sn_parse(d[[j]])))
  }
  max(unlist(Map(f, idx$i, idx$j)))
}

d <- readLines("input-d18.txt")
part1(d)
part2(d)
