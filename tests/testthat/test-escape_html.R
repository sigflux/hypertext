test_that(".escape_html escapes ampersand", {
  expect_equal(hypertext:::.escape_html("a&b"), "a&amp;b")
})

test_that(".escape_html escapes less-than", {
  expect_equal(hypertext:::.escape_html("a<b"), "a&lt;b")
})

test_that(".escape_html escapes greater-than", {
  expect_equal(hypertext:::.escape_html("a>b"), "a&gt;b")
})

test_that(".escape_html escapes double quotes", {
  expect_equal(hypertext:::.escape_html('a"b'), "a&quot;b")
})

test_that(".escape_html escapes single quotes", {
  expect_equal(hypertext:::.escape_html("a'b"), "a&#39;b")
})

test_that(".escape_html escapes all special characters together", {
  expect_equal(
    hypertext:::.escape_html("&<>\"'"),
    "&amp;&lt;&gt;&quot;&#39;"
  )
})

test_that(".escape_html does not double-escape ampersands", {
  expect_equal(
    hypertext:::.escape_html("&amp;"),
    "&amp;amp;"
  )
})

test_that(".escape_html returns plain strings unchanged", {
  expect_equal(hypertext:::.escape_html("hello world"), "hello world")
})

test_that(".escape_html handles empty string", {
  expect_equal(hypertext:::.escape_html(""), "")
})

test_that(".escape_html handles string with only special characters", {
  expect_equal(hypertext:::.escape_html("<>"), "&lt;&gt;")
})
