# -- .make_tag and .make_void_tag factories --------------------------------

test_that(".make_tag returns a function", {
  fn <- hypertext:::.make_tag("div")
  expect_true(is.function(fn))
})

test_that(".make_tag creates correct tag nodes", {
  fn <- hypertext:::.make_tag("section")
  node <- fn(class = "main", "content")
  expect_s3_class(node, "hypertext.tag")
  expect_equal(node$tag, "section")
  expect_equal(node$attrs$class, "main")
  expect_equal(node$children[[1]], "content")
})

test_that(".make_void_tag returns a function", {
  fn <- hypertext:::.make_void_tag("hr")
  expect_true(is.function(fn))
})

test_that(".make_void_tag creates void tag nodes (no children)", {
  fn <- hypertext:::.make_void_tag("input")
  node <- fn(type = "email", "ignored child")
  expect_s3_class(node, "hypertext.tag")
  expect_equal(node$tag, "input")
  expect_equal(node$attrs$type, "email")
  expect_length(node$children, 0)
})

test_that(".make_tag captures tag_name correctly via force()", {
  # Create functions in a loop to test closure capture
  fns <- list()
  for (name in c("div", "span", "p")) {
    fns[[name]] <- hypertext:::.make_tag(name)
  }
  expect_equal(fns$div()$tag, "div")
  expect_equal(fns$span()$tag, "span")
  expect_equal(fns$p()$tag, "p")
})

test_that(".make_void_tag captures tag_name correctly via force()", {
  fns <- list()
  for (name in c("br", "hr", "img")) {
    fns[[name]] <- hypertext:::.make_void_tag(name)
  }
  expect_equal(fns$br()$tag, "br")
  expect_equal(fns$hr()$tag, "hr")
  expect_equal(fns$img()$tag, "img")
})

# -- the `tags` list -------------------------------------------------------

test_that("tags is a named list", {
  expect_true(is.list(tags))
  expect_true(!is.null(names(tags)))
})

test_that("all entries in tags are functions", {
  for (nm in names(tags)) {
    expect_true(is.function(tags[[nm]]), info = paste("tags$", nm))
  }
})

test_that("tags contains common HTML elements", {
  common <- c(
    "div",
    "span",
    "p",
    "a",
    "h1",
    "h2",
    "h3",
    "ul",
    "ol",
    "li",
    "table",
    "tr",
    "td",
    "th",
    "form",
    "input",
    "button",
    "select",
    "img",
    "br",
    "hr",
    "head",
    "body",
    "html",
    "script",
    "style"
  )
  for (el in common) {
    expect_true(el %in% names(tags), info = paste("missing:", el))
  }
})

test_that("tags has 115 elements", {
  expect_equal(length(tags), 115)
})

test_that("void tags produce self-closing HTML", {
  void_names <- c(
    "area",
    "base",
    "br",
    "col",
    "embed",
    "hr",
    "img",
    "input",
    "link",
    "meta",
    "param",
    "source",
    "track",
    "wbr"
  )
  for (nm in void_names) {
    html <- render(tags[[nm]]())
    expect_match(html, "/>$", info = paste(nm, "should be self-closing"))
  }
})

test_that("non-void tags produce opening and closing tags", {
  non_void <- c("div", "span", "p", "a", "ul", "li", "table")
  for (nm in non_void) {
    html <- render(tags[[nm]]())
    expect_match(
      html,
      paste0("^<", nm, "><\\/", nm, ">$"),
      info = paste(nm, "should have closing tag")
    )
  }
})
