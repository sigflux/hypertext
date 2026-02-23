test_that("print.hypertext.tag outputs rendered HTML to console", {
  node <- tags$p("hello")
  expect_output(print(node), "<p>hello</p>")
})

test_that("print.hypertext.tag returns the node invisibly", {
  node <- tags$div("test")
  result <- withVisible(print(node))
  expect_false(result$visible)
  expect_identical(result$value, node)
})

test_that("print.hypertext.tag appends a newline", {
  node <- tags$span("x")
  output <- capture.output(print(node))
  # cat(render(x), "\n") separates args with a space, so output has trailing space
  expect_equal(trimws(output), "<span>x</span>")
})
