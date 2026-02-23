# -- render.hypertext.tag ---------------------------------------------------

test_that("render produces correct HTML for simple element", {
  node <- tags$div("hello")
  expect_equal(render(node), "<div>hello</div>")
})

test_that("render produces correct HTML for element with attributes", {
  node <- tags$p(class = "lead", "text")
  expect_equal(render(node), '<p class="lead">text</p>')
})

test_that("render handles empty element", {
  node <- tags$div()
  expect_equal(render(node), "<div></div>")
})

test_that("render handles void elements as self-closing", {
  node <- tags$br()
  expect_equal(render(node), "<br />")
})

test_that("render handles void element with attributes", {
  node <- tags$input(type = "text", name = "user")
  expect_equal(render(node), '<input type="text" name="user" />')
})

test_that("render handles nested elements", {
  node <- tags$div(tags$p("inner"))
  expect_equal(render(node), "<div><p>inner</p></div>")
})

test_that("render handles deeply nested elements", {
  node <- tags$div(tags$ul(tags$li("item")))
  expect_equal(render(node), "<div><ul><li>item</li></ul></div>")
})

test_that("render handles multiple children", {
  node <- tags$div(tags$p("first"), tags$p("second"))
  expect_equal(render(node), "<div><p>first</p><p>second</p></div>")
})

test_that("render handles mixed text and tag children", {
  node <- tags$p("Hello ", tags$strong("world"), "!")
  expect_equal(render(node), "<p>Hello <strong>world</strong>!</p>")
})

test_that("render escapes text children", {
  node <- tags$p("<script>alert('xss')</script>")
  expect_equal(
    render(node),
    "<p>&lt;script&gt;alert(&#39;xss&#39;)&lt;/script&gt;</p>"
  )
})

test_that("render escapes attribute values", {
  node <- tags$div(title = 'say "hi"')
  expect_equal(render(node), '<div title="say &quot;hi&quot;"></div>')
})

test_that("render handles boolean attributes", {
  node <- tags$input(type = "checkbox", checked = TRUE)
  expect_equal(render(node), '<input type="checkbox" checked />')
})

test_that("render handles NA as boolean attribute", {
  node <- tags$input(disabled = NA)
  expect_equal(render(node), "<input disabled />")
})

test_that("render drops FALSE attributes", {
  node <- tags$input(type = "text", disabled = FALSE)
  expect_equal(render(node), '<input type="text" />')
})

test_that("render drops NULL attributes", {
  node <- tags$div(id = "x", class = NULL)
  expect_equal(render(node), '<div id="x"></div>')
})

test_that("render collapses multi-value class attribute", {
  node <- tags$div(class = c("a", "b", "c"))
  expect_equal(render(node), '<div class="a b c"></div>')
})

# -- render.default --------------------------------------------------------

test_that("render.default escapes text", {
  expect_equal(render("a<b"), "a&lt;b")
})

test_that("render.default coerces to character", {
  expect_equal(render(42), "42")
})

test_that("render.default handles empty string", {
  expect_equal(render(""), "")
})

# -- render.list -----------------------------------------------------------

test_that("render.list concatenates rendered elements", {
  nodes <- list(tags$p("a"), tags$p("b"))
  expect_equal(render(nodes), "<p>a</p><p>b</p>")
})

test_that("render.list handles mixed tags and text", {
  nodes <- list("hello ", tags$strong("world"))
  expect_equal(render(nodes), "hello <strong>world</strong>")
})

test_that("render.list handles empty list", {
  expect_equal(render(list()), "")
})

test_that("render.list handles single element", {
  nodes <- list(tags$div("one"))
  expect_equal(render(nodes), "<div>one</div>")
})
