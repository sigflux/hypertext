# -- tag() public API for custom elements ----------------------------------

test_that("tag() creates a hypertext.tag node", {
  node <- tag("my-element")
  expect_s3_class(node, "hypertext.tag")
  expect_equal(node$tag, "my-element")
})

test_that("tag() accepts named args as attributes", {
  node <- tag("calcite-action-bar", layout = "horizontal")
  expect_equal(node$attrs$layout, "horizontal")
})

test_that("tag() accepts unnamed args as children", {
  node <- tag("my-card", "hello", "world")
  expect_equal(node$children, list("hello", "world"))
})

test_that("tag() renders correctly with attributes and children", {
  html <- render(tag("calcite-action-bar", layout = "horizontal"))
  expect_equal(
    html,
    "<calcite-action-bar layout=\"horizontal\"></calcite-action-bar>"
  )
})

test_that("tag() supports nested custom tags", {
  node <- tag(
    "my-card",
    class = "shadow",
    tag("my-card-header", "Title"),
    tag("my-card-body", "Content")
  )
  html <- render(node)
  expect_equal(
    html,
    paste0(
      "<my-card class=\"shadow\">",
      "<my-card-header>Title</my-card-header>",
      "<my-card-body>Content</my-card-body>",
      "</my-card>"
    )
  )
})

test_that("tag() works with tag_type = 'void' (self-closing)", {
  node <- tag("my-icon", name = "home", tag_type = "void")
  expect_length(node$children, 0)
  html <- render(node)
  expect_equal(html, "<my-icon name=\"home\" />")
})

test_that("tag() tag_type = 'void' ignores children", {
  node <- tag("my-icon", "ignored child", name = "home", tag_type = "void")
  expect_length(node$children, 0)
})

test_that("tag() defaults to tag_type = 'normal'", {
  node <- tag("my-element")
  expect_equal(node$tag_type, "normal")
  html <- render(node)
  expect_equal(html, "<my-element></my-element>")
})

test_that("tag() rejects invalid tag_type", {
  expect_error(tag("my-el", tag_type = "bogus"), "arg")
})

test_that("tag() can mix with built-in tags", {
  html <- render(
    tags$div(
      class = "wrapper",
      tag("calcite-shell", tag("calcite-action-bar", layout = "horizontal"))
    )
  )
  expect_equal(
    html,
    paste0(
      "<div class=\"wrapper\">",
      "<calcite-shell>",
      "<calcite-action-bar layout=\"horizontal\">",
      "</calcite-action-bar>",
      "</calcite-shell>",
      "</div>"
    )
  )
})

test_that("tag() flattens list children like built-in tags", {
  items <- list("a", "b", "c")
  node <- tag("my-list", lapply(items, function(x) tag("my-item", x)))
  expect_length(node$children, 3)
  html <- render(node)
  expect_equal(
    html,
    paste0(
      "<my-list>",
      "<my-item>a</my-item>",
      "<my-item>b</my-item>",
      "<my-item>c</my-item>",
      "</my-list>"
    )
  )
})

test_that("tag() escapes attribute values", {
  html <- render(tag("my-el", title = "a \"quoted\" <value>"))
  expect_match(html, "&quot;")
  expect_match(html, "&lt;")
})

test_that("tag() supports boolean attributes", {
  html <- render(tag("my-el", disabled = TRUE))
  expect_equal(html, "<my-el disabled></my-el>")
})

test_that("tag() drops FALSE and NULL attributes", {
  html <- render(tag("my-el", hidden = FALSE, data = NULL))
  expect_equal(html, "<my-el></my-el>")
})

test_that("tag() drops NULL children", {
  node <- tag("p", "this", NULL)
  expect_equal(node$children, list("this"))
  expect_equal(render(node), "<p>this</p>")
})

test_that("tags$p drops NULL children", {
  html <- render(tags$p("this", NULL))
  expect_equal(html, "<p>this</p>")
})

test_that("tag() handles only NULL children", {
  node <- tag("div", NULL)
  expect_equal(node$children, list())
  expect_equal(render(node), "<div></div>")
})
