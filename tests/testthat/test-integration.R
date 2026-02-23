# -- integration tests: real-world patterns --------------------------------

test_that("full HTML document structure renders correctly", {
  doc <- tags$html(
    tags$head(
      tags$title("My Page"),
      tags$meta(charset = "utf-8"),
      tags$link(rel = "stylesheet", href = "style.css")
    ),
    tags$body(
      tags$h1("Hello"),
      tags$p("Welcome")
    )
  )
  html <- render(doc)
  expect_match(html, "^<html>")
  expect_match(html, "</html>$")
  expect_match(html, "<title>My Page</title>")
  expect_match(html, '<meta charset="utf-8" />')
  expect_match(html, '<link rel="stylesheet" href="style.css" />')
  expect_match(html, "<h1>Hello</h1>")
})

test_that("form with mixed void and non-void elements", {
  form <- tags$form(
    action = "/submit",
    method = "post",
    tags$label(`for` = "name", "Name:"),
    tags$input(type = "text", id = "name", name = "name"),
    tags$button(type = "submit", "Submit")
  )
  html <- render(form)
  expect_match(html, '<form action="/submit" method="post">')
  expect_match(html, '<label for="name">Name:</label>')
  expect_match(html, '<input type="text" id="name" name="name" />')
  expect_match(html, "<button type=\"submit\">Submit</button>")
})

test_that("list generated with lapply is properly flattened", {
  items <- c("Alpha", "Beta", "Gamma")
  ul <- tags$ul(lapply(items, function(x) tags$li(x)))
  html <- render(ul)
  expect_equal(
    html,
    "<ul><li>Alpha</li><li>Beta</li><li>Gamma</li></ul>"
  )
})

test_that("table with multiple rows and cells", {
  tbl <- tags$table(
    class = "data-table",
    tags$thead(
      tags$tr(tags$th("Name"), tags$th("Age"))
    ),
    tags$tbody(
      tags$tr(tags$td("Alice"), tags$td("30")),
      tags$tr(tags$td("Bob"), tags$td("25"))
    )
  )
  html <- render(tbl)
  expect_match(html, '<table class="data-table">')
  expect_match(html, "<th>Name</th><th>Age</th>")
  expect_match(html, "<td>Alice</td><td>30</td>")
})

test_that("XSS content is properly escaped in text and attributes", {
  node <- tags$div(
    title = '<script>alert("xss")</script>',
    "<img onerror='alert(1)' src=x>"
  )
  html <- render(node)
  # Attribute value should be escaped
  expect_false(grepl("<script>", html, fixed = TRUE))
  # Child text should be escaped
  expect_false(grepl("<img", html, fixed = TRUE))
  expect_match(html, "&lt;img")
})

test_that("deeply nested structure renders correctly", {
  node <- tags$div(
    tags$div(
      tags$div(
        tags$div(
          tags$span("deep")
        )
      )
    )
  )
  expect_equal(
    render(node),
    "<div><div><div><div><span>deep</span></div></div></div></div>"
  )
})

test_that("siblings rendered via render.list", {
  siblings <- list(
    tags$p("first"),
    tags$hr(),
    tags$p("second")
  )
  html <- render(siblings)
  expect_equal(html, "<p>first</p><hr /><p>second</p>")
})

test_that("multiple class values are collapsed", {
  node <- tags$div(class = c("container", "mx-auto", "p-4"), "content")
  expect_equal(
    render(node),
    '<div class="container mx-auto p-4">content</div>'
  )
})

test_that("boolean attributes in real form elements", {
  node <- tags$input(
    type = "checkbox",
    checked = TRUE,
    disabled = FALSE,
    required = NA
  )
  html <- render(node)
  expect_match(html, "checked")
  expect_false(grepl("disabled", html))
  expect_match(html, "required")
})

test_that("mixed numeric and string children", {
  node <- tags$p("Score: ", 42)
  html <- render(node)
  expect_equal(html, "<p>Score: 42</p>")
})
