# -- tag_list constructor ---------------------------------------------------

test_that("tag_list returns object with correct classes", {
  tl <- tag_list(tags$p("a"))
  expect_s3_class(tl, "hypertext.tag.list")
  expect_s3_class(tl, "list")
  expect_equal(class(tl), c("hypertext.tag.list", "list"))
})

test_that("tag_list stores children as list elements", {
  tl <- tag_list(tags$p("a"), tags$p("b"))
  expect_length(tl, 2L)
  expect_s3_class(tl[[1]], "hypertext.tag")
  expect_s3_class(tl[[2]], "hypertext.tag")
})

test_that("tag_list with no arguments produces empty tag list", {
  tl <- tag_list()
  expect_s3_class(tl, "hypertext.tag.list")
  expect_length(tl, 0L)
})

test_that("tag_list accepts mixed tags and text", {
  tl <- tag_list("hello ", tags$strong("world"))
  expect_length(tl, 2L)
  expect_equal(tl[[1]], "hello ")
  expect_s3_class(tl[[2]], "hypertext.tag")
})

test_that("tag_list accepts a single element", {
  tl <- tag_list(tags$div("only"))
  expect_length(tl, 1L)
})

# -- rendering tag_list ----------------------------------------------------

test_that("render concatenates tag_list children", {
  tl <- tag_list(tags$p("a"), tags$p("b"))
  expect_equal(render(tl), "<p>a</p><p>b</p>")
})

test_that("render handles empty tag_list", {
  tl <- tag_list()
  expect_equal(render(tl), "")
})

test_that("render handles tag_list with mixed tags and text", {
  tl <- tag_list("hello ", tags$strong("world"))
  expect_equal(render(tl), "hello <strong>world</strong>")
})

test_that("render handles tag_list with single element", {
  tl <- tag_list(tags$div("one"))
  expect_equal(render(tl), "<div>one</div>")
})

test_that("render tag_list writes to file and returns invisibly", {
  tmp <- tempfile(fileext = ".html")
  on.exit(unlink(tmp), add = TRUE)

  tl <- tag_list(tags$p("a"), tags$p("b"))
  result <- withVisible(render(tl, file = tmp))

  expect_false(result$visible)
  expect_equal(result$value, "<p>a</p><p>b</p>")
  expect_equal(readLines(tmp, warn = FALSE), "<p>a</p><p>b</p>")
})

test_that("render tag_list supports write_mode = 'append'", {
  tmp <- tempfile(fileext = ".html")
  on.exit(unlink(tmp), add = TRUE)

  render(tags$h1("Title"), file = tmp)
  render(tag_list(tags$p("a"), tags$p("b")), file = tmp, write_mode = "append")

  expect_equal(
    readLines(tmp, warn = FALSE),
    "<h1>Title</h1><p>a</p><p>b</p>"
  )
})

test_that("render handles nested tag_list", {
  inner <- tag_list(tags$li("a"), tags$li("b"))
  outer <- tags$ul(inner)
  expect_equal(render(outer), "<ul><li>a</li><li>b</li></ul>")
})

# -- print.hypertext.tag.list ----------------------------------------------

test_that("print.hypertext.tag.list outputs rendered HTML with newline", {
  tl <- tag_list(tags$p("a"), tags$p("b"))
  expect_output(print(tl), "<p>a</p><p>b</p>")
})

test_that("print.hypertext.tag.list returns invisibly", {
  tl <- tag_list(tags$p("a"))
  result <- withVisible(print(tl))
  expect_false(result$visible)
})

# -- .flatten_children preserves tag_list ----------------------------------

test_that(".flatten_children does not unwrap tag_list", {
  tl <- tag_list(tags$li("a"), tags$li("b"))
  result <- hypertext:::.flatten_children(list(tl))
  expect_length(result, 1L)
  expect_s3_class(result[[1]], "hypertext.tag.list")
})

test_that(".flatten_children still flattens plain lists", {
  plain <- list(tags$li("a"), tags$li("b"))
  result <- hypertext:::.flatten_children(list(plain))
  expect_length(result, 2L)
  expect_s3_class(result[[1]], "hypertext.tag")
  expect_s3_class(result[[2]], "hypertext.tag")
})

# -- tag_list as child of a tag --------------------------------------------

test_that("tag_list as child of tag renders its children inline", {
  tl <- tag_list(tags$li("one"), tags$li("two"))
  node <- tags$ul(tl)
  expect_equal(render(node), "<ul><li>one</li><li>two</li></ul>")
})

test_that("tag_list mixed with other children in a tag", {
  tl <- tag_list(tags$li("b"), tags$li("c"))
  node <- tags$ul(tags$li("a"), tl, tags$li("d"))
  expect_equal(
    render(node),
    "<ul><li>a</li><li>b</li><li>c</li><li>d</li></ul>"
  )
})
