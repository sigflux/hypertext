test_that(".tag creates a hypertext.tag object", {
  node <- hypertext:::.tag("div")
  expect_s3_class(node, "hypertext.tag")
})

test_that(".tag stores the tag name", {
  node <- hypertext:::.tag("span")
  expect_equal(node$tag, "span")
})

test_that(".tag separates named args as attributes", {
  node <- hypertext:::.tag("div", id = "main", class = "container")
  expect_equal(node$attrs$id, "main")
  expect_equal(node$attrs$class, "container")
})

test_that(".tag separates unnamed args as children", {
  node <- hypertext:::.tag("p", "hello", "world")
  expect_length(node$children, 2)
  expect_equal(node$children[[1]], "hello")
  expect_equal(node$children[[2]], "world")
})

test_that(".tag handles mixed named and unnamed args", {
  node <- hypertext:::.tag("a", href = "/home", "Click me")
  expect_equal(node$attrs$href, "/home")
  expect_length(node$children, 1)
  expect_equal(node$children[[1]], "Click me")
})

test_that(".tag with .void = TRUE ignores children", {
  node <- hypertext:::.tag(
    "input",
    type = "text",
    "should be ignored",
    .void = TRUE
  )
  expect_equal(node$attrs$type, "text")
  expect_length(node$children, 0)
})

test_that(".tag with no arguments creates empty element", {
  node <- hypertext:::.tag("div")
  expect_equal(node$tag, "div")
  expect_length(node$attrs, 0)
  expect_length(node$children, 0)
})

test_that(".tag flattens nested lists in children", {
  child_list <- list("a", "b")
  node <- hypertext:::.tag("ul", child_list)
  expect_length(node$children, 2)
  expect_equal(node$children[[1]], "a")
  expect_equal(node$children[[2]], "b")
})

test_that(".tag preserves hypertext.tag children without flattening", {
  inner <- hypertext:::.tag("span", "inside")
  node <- hypertext:::.tag("div", inner)
  expect_length(node$children, 1)
  expect_s3_class(node$children[[1]], "hypertext.tag")
})

test_that(".tag structure has expected components", {
  node <- hypertext:::.tag("p", class = "lead", "text")
  expect_named(node, c("tag", "attrs", "children"))
})
