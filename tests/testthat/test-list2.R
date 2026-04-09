# -- no arguments ----------------------------------------------------------

test_that(".list2() with no arguments returns an empty list", {
  expect_identical(
    .list2(),
    list()
  )
})

# -- trailing commas -------------------------------------------------------

test_that(".list2() throws when an argument is missing", {
  expect_error(
    .list2(,),
    regexp = "argument is missing, with no default"
  )
  expect_error(
    .list2("a", , ),
    regexp = "argument is missing, with no default"
  )
})

test_that(".list2() with a trailing comma after one unnamed arg", {
  expect_identical(
    .list2(1, ),
    list(1)
  )
})

test_that(".list2() with a trailing comma after one named arg", {
  expect_identical(
    .list2(a = 1, ),
    list(a = 1)
  )
})

test_that(".list2() with a trailing comma after multiple args", {
  expect_identical(
    .list2(1, 2, 3, ),
    list(1, 2, 3)
  )
  expect_identical(
    .list2(a = 1, b = 2, ),
    list(a = 1, b = 2)
  )
})

test_that(".list2() trailing comma with mixed named/unnamed args", {
  expect_identical(
    .list2(a = 1, "hello", b = 3, ),
    list(a = 1, "hello", b = 3)
  )
})

# -- NULL handling ---------------------------------------------------------

test_that(".list2(NULL) returns list(NULL)", {
  expect_identical(
    .list2(NULL),
    list(NULL)
  )
})

test_that(".list2() preserves NULL among other elements", {
  expect_identical(
    .list2(1, NULL, 3),
    list(1, NULL, 3)
  )
})

test_that(".list2() preserves named NULL", {
  expect_identical(
    .list2(a = NULL, ),
    list(a = NULL)
  )
  expect_identical(
    .list2(a = 1, b = NULL, c = 3, ),
    list(a = 1, b = NULL, c = 3)
  )
})

# -- unnamed arguments -----------------------------------------------------

test_that(".list2() with unnamed arguments", {
  expect_identical(
    .list2(1, 2, 3),
    list(1, 2, 3)
  )
  expect_identical(
    .list2("a", "b", "c", ),
    list("a", "b", "c")
  )
})

# -- named arguments -------------------------------------------------------

test_that(".list2() with a single named argument", {
  expect_identical(
    .list2(x = 1, ),
    list(x = 1)
  )
})

test_that(".list2() with multiple named arguments", {
  expect_identical(
    .list2(a = 1, b = "two", c = TRUE, ),
    list(a = 1, b = "two", c = TRUE)
  )
})

test_that(".list2() preserves names correctly", {
  result <- .list2(foo = 10, bar = 20)
  expect_identical(
    names(result),
    c("foo", "bar")
  )
  expect_identical(result$foo, 10)
  expect_identical(result$bar, 20)
})

# -- mixed named and unnamed arguments ------------------------------------

test_that(".list2() with mixed named and unnamed arguments", {
  expect_identical(
    .list2(a = 1, 2, b = 3, ),
    list(a = 1, 2, b = 3)
  )
})

test_that(".list2() mixed args have partially empty names", {
  result <- .list2(a = 1, 2, b = 3)
  expect_equal(
    names(result),
    c("a", "", "b")
  )
})

test_that(".list2() mixed args: unnamed first, named later", {
  expect_identical(
    .list2(1, 2, z = 3),
    list(1, 2, z = 3)
  )
})

test_that(".list2() mixed args: named first, unnamed later", {
  expect_identical(
    .list2(z = 3, 1, 2),
    list(z = 3, 1, 2)
  )
})

# -- various value types ---------------------------------------------------

test_that(".list2() with numeric values", {
  expect_identical(
    .list2(1L, 2.5, 3 + 1i),
    list(1L, 2.5, 3 + 1i)
  )
})

test_that(".list2() with character values", {
  expect_identical(
    .list2("a", "b"),
    list("a", "b")
  )
})

test_that(".list2() with logical values", {
  expect_identical(
    .list2(TRUE, FALSE, NA),
    list(TRUE, FALSE, NA)
  )
})

test_that(".list2() with vectors as elements", {
  expect_identical(
    .list2(1:3, c("a", "b")),
    list(1:3, c("a", "b"))
  )
})

test_that(".list2() with nested lists", {
  inner <- list(a = 1, b = 2)
  expect_identical(
    .list2(inner, 3),
    list(inner, 3)
  )
  expect_identical(
    .list2(x = inner),
    list(x = inner)
  )
})

test_that(".list2() with a function as an element", {
  fn <- function(x) x + 1
  expect_identical(
    .list2(fn),
    list(fn)
  )
})

test_that(".list2() with a formula as an element", {
  f <- y ~ x
  result <- .list2(f)
  expect_identical(
    .list2(f),
    list(f)
  )
})

test_that(".list2() with an environment as an element", {
  e <- new.env(parent = emptyenv())
  e$x <- 42
  expect_identical(
    .list2(e),
    list(e)
  )
})

test_that(".list2() with data.frame as an element", {
  df <- data.frame(x = 1:3, y = letters[1:3])
  expect_identical(
    .list2(df),
    list(df)
  )
})

test_that(".list2() with NA values", {
  expect_identical(
    .list2(NA),
    list(NA)
  )
  expect_identical(
    .list2(NA_real_),
    list(NA_real_)
  )
  expect_identical(
    .list2(NA_character_),
    list(NA_character_)
  )
})

# -- expressions and computed values ---------------------------------------

test_that(".list2() evaluates expressions in arguments", {
  expect_identical(
    .list2(1 + 1),
    list(1 + 1)
  )
  expect_identical(
    .list2(x = paste0("a", "b")),
    list(x = paste0("a", "b"))
  )
})

test_that(".list2() with variables as arguments", {
  a <- 10
  b <- "hello"
  expect_identical(
    .list2(a, b),
    list(a, b)
  )
  expect_identical(
    .list2(x = a, y = b),
    list(x = a, y = b)
  )
})

# -- edge cases ------------------------------------------------------------

test_that(".list2() with duplicate names", {
  result <- .list2(a = 1, a = 2)
  expected <- list(a = 1, a = 2)
  expect_identical(result, expected)
})

test_that(".list2() matches base::list() with do.call", {
  args <- list(a = 1, b = "two", c = TRUE)
  expect_identical(
    do.call(.list2, args),
    do.call(base::list, args)
  )
})

# -- deep forwarding -------------------------------------------------------

test_that(".list2() matches list() through one level of forwarding", {
  f <- function(...) {
    .list2(...)
  }
  g <- function(...) {
    list(...)
  }

  x <- 1
  y <- "a"

  expect_identical(
    f(x, y, z = x + 1),
    g(x, y, z = x + 1)
  )
})

test_that(".list2() matches list() through multiple levels of forwarding", {
  f3 <- function(...) {
    .list2(...)
  }
  f2 <- function(...) {
    f3(...)
  }
  f1 <- function(...) {
    f2(...)
  }

  g3 <- function(...) {
    list(...)
  }
  g2 <- function(...) {
    g3(...)
  }
  g1 <- function(...) {
    g2(...)
  }

  x <- 1
  y <- "a"

  expect_identical(
    f1(x, y, z = x + 1),
    g1(x, y, z = x + 1)
  )
})

test_that(".list2() deep forwarding uses the original caller environment", {
  f3 <- function(...) {
    .list2(...)
  }
  f2 <- function(...) {
    x <- "wrong"
    y <- "wrong"
    f3(...)
  }
  f1 <- function(...) {
    x <- "also wrong"
    f2(...)
  }

  g3 <- function(...) {
    list(...)
  }
  g2 <- function(...) {
    x <- "wrong"
    y <- "wrong"
    g3(...)
  }
  g1 <- function(...) {
    x <- "also wrong"
    g2(...)
  }

  local({
    x <- 10
    y <- 20

    expect_identical(
      f1(x, y, z = x + y),
      g1(x, y, z = x + y)
    )
  })
})

test_that(".list2() deep forwarding preserves names", {
  f2 <- function(...) {
    .list2(...)
  }
  f1 <- function(...) {
    f2(...)
  }

  g2 <- function(...) {
    list(...)
  }
  g1 <- function(...) {
    g2(...)
  }

  x <- 1
  y <- 2

  expect_identical(
    f1(a = x, y, c = y + 1),
    g1(a = x, y, c = y + 1)
  )
})

test_that(".list2() deep forwarding works with trailing commas", {
  f2 <- function(...) {
    .list2(...)
  }
  f1 <- function(...) {
    f2(...)
  }

  g2 <- function(...) {
    list(...)
  }
  g1 <- function(...) {
    g2(...)
  }

  x <- 1
  y <- 2

  expect_identical(
    f1(x, y, ),
    g1(x, y)
  )
  expect_identical(
    f1(a = x, b = y, ),
    g1(a = x, b = y)
  )
})

test_that(".list2() deep forwarding preserves NULL", {
  f2 <- function(...) {
    .list2(...)
  }
  f1 <- function(...) {
    f2(...)
  }

  g2 <- function(...) {
    list(...)
  }
  g1 <- function(...) {
    g2(...)
  }

  x <- NULL
  y <- 2

  expect_identical(
    f1(x, y, z = NULL),
    g1(x, y, z = NULL)
  )
})

test_that(".list2() deep forwarding evaluates expressions like list()", {
  f2 <- function(...) {
    .list2(...)
  }
  f1 <- function(...) {
    f2(...)
  }

  g2 <- function(...) {
    list(...)
  }
  g1 <- function(...) {
    g2(...)
  }

  x <- 2
  y <- 3

  expect_identical(
    f1(x + y, a = x * y, paste0("v", x)),
    g1(x + y, a = x * y, paste0("v", x))
  )
})
