test_that("print.hypertext.tag outputs leaf HTML to console", {
  node <- tags$p("hello")
  expect_output(print(node), "<p>hello</p>")
})

test_that("print.hypertext.tag pretty prints nested HTML", {
  node <- tags$html(
    tags$head(
      tags$title("hypertext")
    ),
    tags$body(
      tags$h1("Hello"),
      tags$p(class = c("lead", "mb-2"), "Server-side HTML."),
      tags$input(type = "text", placeholder = "enter your nickname"),
      tags$button("Click")
    )
  )

  expect_equal(
    capture.output(print(node)),
    c(
      "<html>",
      "  <head>",
      "    <title>hypertext</title>",
      "  </head>",
      "  <body>",
      "    <h1>Hello</h1>",
      "    <p class=\"lead mb-2\">Server-side HTML.</p>",
      "    <input type=\"text\" placeholder=\"enter your nickname\" />",
      "    <button>Click</button>",
      "  </body>",
      "</html>"
    )
  )
})

test_that("print.hypertext.tag pretty prints mixed text and nested children", {
  node <- tags$p("Hello ", tags$strong("world"), "!")

  expect_equal(
    capture.output(print(node)),
    c(
      "<p>",
      "  Hello ",
      "  <strong>world</strong>",
      "  !",
      "</p>"
    )
  )
})

test_that("print.hypertext.tag returns the node invisibly", {
  node <- tags$div("test")
  result <- withVisible(print(node))
  expect_false(result$visible)
  expect_identical(result$value, node)
})

test_that("print.hypertext.tag appends a newline without extra spaces", {
  node <- tags$span("x")
  output <- capture.output(print(node))
  expect_equal(output, "<span>x</span>")
})
