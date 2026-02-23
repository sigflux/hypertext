test_that(".flatten_children returns empty list for empty input", {
  expect_equal(hypertext:::.flatten_children(list()), list())
})

test_that(".flatten_children keeps non-list elements as-is", {
  result <- hypertext:::.flatten_children(list("a", "b"))
  expect_equal(result, list("a", "b"))
})

test_that(".flatten_children unpacks plain lists", {
  result <- hypertext:::.flatten_children(list(list("a", "b"), "c"))
  expect_equal(result, list("a", "b", "c"))
})

test_that(".flatten_children recursively unpacks nested lists", {
  result <- hypertext:::.flatten_children(list(list(list("deep"))))
  expect_equal(result, list("deep"))
})

test_that(".flatten_children preserves hypertext.tag objects", {
  tag_node <- hypertext:::.tag("span", "text")
  result <- hypertext:::.flatten_children(list(tag_node))
  expect_length(result, 1)
  expect_s3_class(result[[1]], "hypertext.tag")
})

test_that(".flatten_children unpacks lists but preserves tags inside", {
  tag_node <- hypertext:::.tag("li", "item")
  result <- hypertext:::.flatten_children(list(list(tag_node, "text")))
  expect_length(result, 2)
  expect_s3_class(result[[1]], "hypertext.tag")
  expect_equal(result[[2]], "text")
})

test_that(".flatten_children handles lapply-style output", {
  items <- c("a", "b", "c")
  tag_list <- lapply(items, function(x) hypertext:::.tag("li", x))
  result <- hypertext:::.flatten_children(list(tag_list))
  expect_length(result, 3)
  for (i in seq_along(result)) {
    expect_s3_class(result[[i]], "hypertext.tag")
  }
})

test_that(".flatten_children preserves numeric elements", {
  result <- hypertext:::.flatten_children(list(1, 2, 3))
  expect_equal(result, list(1, 2, 3))
})
