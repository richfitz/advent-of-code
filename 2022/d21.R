parse_input <- function(path) {
  d <- strsplit(readLines(path), ": ", fixed = TRUE)
  setNames(as.list(parse(text = vapply(d, "[[", "", 2))),
           vapply(d, "[[", "", 1))
}

part1 <- function(d) {
  e <- list2env(d, parent = baseenv())
  f <- function(nm) {
    if (is.recursive(e[[nm]])) {
      expr <- e[[nm]]
      f(as.character(expr[[2L]]))
      f(as.character(expr[[3L]]))
      e[[nm]] <- eval(expr, e)
    }
  }
  f("root")
  format(e[["root"]], digits = 20)
}

part2 <- function(d) {
  f <- function(nm) {
    e <- d[[nm]]
    if (nm == "humn") {
      quote(x)
    } else if (is.recursive(e)) {
      e[[2L]] <- f(as.character(e[[2L]]))
      e[[3L]] <- f(as.character(e[[3L]]))
      if (is.numeric(e[[2L]]) && is.numeric(e[[3L]])) eval(e, baseenv()) else e
    } else {
      e
    }
  }
  inverse <- c("-" = "+", "+" = "-", "/" = "*", "*" = "/")
  expr <- call("-", f(as.character(d$root[[2]])), f(as.character(d$root[[3]])))
  y <- 0
  while (is.recursive(expr)) {
    op <- as.character(expr[[1]])
    lhs_is_language <- is.language(expr[[3L]])
    if (lhs_is_language && (op == "-" || op == "/")) {
      y <- eval(call(op, expr[[2L]], y))
    } else {
      y <- eval(call(inverse[[op]], y, expr[[3 - lhs_is_language]]))
    }
    expr <- expr[[lhs_is_language + 2L]]
  }
  format(y, digits = 20)
}

d <- parse_input("test-input-d21.txt")
stopifnot(part1(d) == "152")
stopifnot(part2(d) == "301")

d <- parse_input("input-d21.txt")
part1(d)
part2(d)
