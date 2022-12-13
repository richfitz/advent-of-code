hex2bin <- local({
  map <- t(unname(as.matrix(expand.grid(0:1, 0:1, 0:1, 0:1)))[, 4:1])
  colnames(map) <- c(0:9, LETTERS[1:6])
  function(s) {
    c(unname(map[, strsplit(s, NULL)[[1]]]))
  }
})

operations <- list(sum = sum, prod = prod, min = min, max = max,
                   literal = identity,
                   gt = function(x) as.integer(x[[1]] > x[[2]]),
                   lt = function(x) as.integer(x[[1]] < x[[2]]),
                   equals = function(x) as.integer(x[[1]] == x[[2]]))


bin2int <- function(x) {
  sum(2^(rev(seq_along(x)) - 1) * x)
}


bytes <- function(d) {
  force(d)
  ptr <- 1L
  list(
    read = function(n) {
      i <- seq(ptr, length.out = n)
      ptr <<- ptr + n
      d[i]
    },
    at = function(offset) {
      d[ptr + offset]
    },
    complete = function() {
      ptr > length(d)
    }
  )
}


reader <- function(d) {
  self <- environment()
  data <- bytes(d)

  version <- function() {
    bin2int(self$data$read(3))
  }
  type <- function() {
    names(operations)[[bin2int(self$data$read(3)) + 1]]
  }
  packet_length <- function() {
    if (self$data$read(1) == 0) {
      list(packets = FALSE, n = bin2int(self$data$read(15)))
    } else {
      list(packets = TRUE, n = bin2int(self$data$read(11)))
    }
  }
  literal <- function() {
    n <- 1L
    while (self$data$at(5 * (n - 1)) == 1L) {
      n <- n + 1L
    }
    bin2int(c(matrix(self$data$read(n * 5), 5)[-1, ]))
  }
  decode <- function() {
    if (self$data$complete()) {
      return(NULL)
    }
    version <- self$version()
    type <- self$type()
    if (type == "literal") {
      value <- self$literal()
    } else {
      len <- self$packet_length()
      if (len$packets) {
        value <- vector("list", len$n)
        for (i in seq_along(value)) {
          value[[i]] <- self$decode()
        }
      } else {
        sub <- reader(self$data$read(len$n))
        value <- list()
        while (!is.null(x <- sub$decode())) {
          value[[length(value) + 1L]] <- x
        }
      }
    }
    list(version = version, type = type, value = value)
  }

  self
}


decode_hex <- function(s) {
  reader(hex2bin(s))$decode()
}


part1 <- function(d) {
  f <- function(x) {
    if (is.atomic(x$value)) {
      x$version
    } else if (is.null(names(x$value))) {
      x$version + sum(vapply(x$value, f, numeric(1)))
    } else {
      x$version + x$value$version
    }
  }
  f(d)
}


part2 <- function(d) {
  f <- function(x) {
    if (x$type == "literal") {
      x$value
    } else {
      operations[[x$type]](vapply(x$value, f, numeric(1)))
    }
  }
  format(f(d), digits = 16)
}


stopifnot(
  part1(decode_hex("8A004A801A8002F478")) == 16,
  part1(decode_hex("620080001611562C8802118E34")) == 12,
  part1(decode_hex("C0015000016115A2E0802F182340")) == 23,
  part1(decode_hex("A0016C880162017C3686B18A3D4780")) == 31)

stopifnot(
  part2(decode_hex("C200B40A82")) == "3",
  part2(decode_hex("04005AC33890")) == "54",
  part2(decode_hex("880086C3E88112")) == "7",
  part2(decode_hex("CE00C43D881120")) == "9",
  part2(decode_hex("D8005AC2A8F0")) == "1",
  part2(decode_hex("F600BC2D8F")) == "0",
  part2(decode_hex("9C005AC2F8F0")) == "0",
  part2(decode_hex("9C0141080250320F1802104A08")) == "1")

dat <- decode_hex(readLines("input-d16.txt"))
part1(dat)
part2(dat)
