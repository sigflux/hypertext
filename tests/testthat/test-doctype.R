# -- doctype construction -----------------------------------------------------

test_that("doctype returns a hypertext.raw object", {
  d <- doctype()
  expect_s3_class(d, "hypertext.raw")
})

test_that("doctype renders to <!DOCTYPE html>", {
  expect_equal(render(doctype()), "<!DOCTYPE html>")
})

# -- full page usage ---------------------------------------------------------

test_that("doctype works in a full page with tag_list", {
  page <- tag_list(
    doctype(),
    tags$html(
      tags$head(tags$title("Test")),
      tags$body(tags$h1("Hello"))
    )
  )
  html <- render(page)
  expect_equal(
    html,
    "<!DOCTYPE html><html><head><title>Test</title></head><body><h1>Hello</h1></body></html>"
  )
})

test_that("doctype works in a plain list", {
  page <- list(
    doctype(),
    tags$html(tags$body("hi"))
  )
  html <- render(page)
  expect_equal(html, "<!DOCTYPE html><html><body>hi</body></html>")
})

# -- file output -------------------------------------------------------------

test_that("full page with doctype writes correctly to file", {
  tmp <- tempfile(fileext = ".html")
  on.exit(unlink(tmp), add = TRUE)

  page <- tag_list(
    doctype(),
    tags$html(
      tags$head(tags$title("Test")),
      tags$body(tags$p("content"))
    )
  )
  render(page, file = tmp)

  expect_equal(
    readLines(tmp, warn = FALSE),
    "<!DOCTYPE html><html><head><title>Test</title></head><body><p>content</p></body></html>"
  )
})

# -- print method ------------------------------------------------------------

test_that("print.hypertext.raw works for doctype", {
  expect_output(print(doctype()), "<!DOCTYPE html>")
})
