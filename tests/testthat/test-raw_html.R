# -- raw_html construction ----------------------------------------------------

test_that("raw_html returns object with correct class", {
  r <- raw_html("<b>bold</b>")
  expect_s3_class(r, "hypertext.raw")
})

test_that("raw_html preserves the input string", {
  r <- raw_html("<em>hi</em>")
  expect_equal(as.character(r), "<em>hi</em>")
})

test_that("raw_html concatenates multiple arguments", {
  r <- raw_html("<b>one</b>", "<i>two</i>")
  expect_equal(as.character(r), "<b>one</b> <i>two</i>")
})

test_that("raw_html with no arguments returns empty string", {
  r <- raw_html()
  expect_s3_class(r, "hypertext.raw")
  expect_equal(as.character(r), "")
})

test_that("raw_html coerces non-character input via paste", {
  expect_equal(as.character(raw_html(42)), "42")
  expect_equal(as.character(raw_html(TRUE)), "TRUE")
})

# -- render.hypertext.raw ----------------------------------------------------

test_that("render does not escape raw_html content", {
  r <- raw_html("<script>alert('hi')</script>")
  expect_equal(render(r), "<script>alert('hi')</script>")
})

test_that("render preserves special characters in raw_html", {
  r <- raw_html('a < b & c > d "e" \'f\'')
  expect_equal(render(r), 'a < b & c > d "e" \'f\'')
})

test_that("raw_html works as a child of a tag", {
  node <- tags$div(raw_html("<em>raw</em>"))
  expect_equal(render(node), "<div><em>raw</em></div>")
})

test_that("raw_html mixes with text and tag children", {
  node <- tags$div(
    "before ",
    raw_html("<em>raw</em>"),
    " ",
    tags$strong("tag"),
    " after"
  )
  expect_equal(
    render(node),
    "<div>before <em>raw</em> <strong>tag</strong> after</div>"
  )
})

test_that("raw_html works inside tag_list", {
  tl <- tag_list(raw_html("<hr />"), tags$p("text"))
  expect_equal(render(tl), "<hr /><p>text</p>")
})

test_that("raw_html renders inside a list", {
  nodes <- list(raw_html("<!DOCTYPE html>"), tags$html())
  expect_equal(render(nodes), "<!DOCTYPE html><html></html>")
})

# -- render with file output -------------------------------------------------

test_that("render writes raw_html to a file and returns invisibly", {
  tmp <- tempfile(fileext = ".html")
  on.exit(unlink(tmp), add = TRUE)

  r <- raw_html("<div>raw</div>")
  result <- withVisible(render(r, file = tmp))

  expect_false(result$visible)
  expect_equal(result$value, "<div>raw</div>")
  expect_equal(readLines(tmp, warn = FALSE), "<div>raw</div>")
})

test_that("render appends raw_html to a file", {
  tmp <- tempfile(fileext = ".html")
  on.exit(unlink(tmp), add = TRUE)

  render(raw_html("<p>first</p>"), file = tmp)
  render(raw_html("<p>second</p>"), file = tmp, write_mode = "append")

  expect_equal(
    readLines(tmp, warn = FALSE),
    "<p>first</p><p>second</p>"
  )
})

# -- print method ------------------------------------------------------------

test_that("print.hypertext.raw outputs rendered content", {
  r <- raw_html("<b>bold</b>")
  expect_output(print(r), "<b>bold</b>")
})
