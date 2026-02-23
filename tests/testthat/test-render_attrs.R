test_that(".render_attrs returns empty string for empty list", {
  expect_equal(hypertext:::.render_attrs(list()), "")
})

test_that(".render_attrs renders a simple character attribute", {
  expect_equal(
    hypertext:::.render_attrs(list(class = "foo")),
    ' class="foo"'
  )
})

test_that(".render_attrs renders multiple attributes", {
  result <- hypertext:::.render_attrs(list(id = "main", class = "container"))
  expect_equal(result, ' id="main" class="container"')
})

test_that(".render_attrs drops NULL values silently", {
  expect_equal(
    hypertext:::.render_attrs(list(id = "x", class = NULL)),
    ' id="x"'
  )
})

test_that(".render_attrs drops FALSE values silently", {
  expect_equal(
    hypertext:::.render_attrs(list(id = "x", disabled = FALSE)),
    ' id="x"'
  )
})

test_that(".render_attrs renders TRUE as boolean attribute", {
  expect_equal(
    hypertext:::.render_attrs(list(disabled = TRUE)),
    " disabled"
  )
})

test_that(".render_attrs renders NA as boolean attribute", {
  expect_equal(
    hypertext:::.render_attrs(list(disabled = NA)),
    " disabled"
  )
})

test_that(".render_attrs collapses character vectors with space", {
  expect_equal(
    hypertext:::.render_attrs(list(class = c("a", "b", "c"))),
    ' class="a b c"'
  )
})

test_that(".render_attrs escapes special characters in values", {
  expect_equal(
    hypertext:::.render_attrs(list(title = 'say "hello"')),
    ' title="say &quot;hello&quot;"'
  )
})

test_that(".render_attrs renders numeric values", {
  expect_equal(
    hypertext:::.render_attrs(list(tabindex = 1)),
    ' tabindex="1"'
  )
})

test_that(".render_attrs returns empty string when all values are NULL", {
  expect_equal(
    hypertext:::.render_attrs(list(a = NULL, b = NULL)),
    ""
  )
})

test_that(".render_attrs returns empty string when all values are FALSE", {
  expect_equal(
    hypertext:::.render_attrs(list(a = FALSE, b = FALSE)),
    ""
  )
})

test_that(".render_attrs mixes boolean and valued attributes", {
  result <- hypertext:::.render_attrs(list(
    class = "btn",
    disabled = TRUE,
    hidden = NA,
    id = NULL
  ))
  expect_equal(result, ' class="btn" disabled hidden')
})

test_that(".render_attrs includes leading space when non-empty", {
  result <- hypertext:::.render_attrs(list(id = "x"))
  expect_true(startsWith(result, " "))
})
