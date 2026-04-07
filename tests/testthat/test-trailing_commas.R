# -- trailing commas in tags$*() --------------------------------------------

test_that("trailing comma after single child in tags$*()", {
  node <- tags$p(
    "hello",
  )
  expect_s3_class(node, "hypertext.tag")
  expect_equal(node$children, list("hello"))
  expect_equal(render(node), "<p>hello</p>")
})

test_that("trailing comma after multiple children in tags$*()", {
  node <- tags$p(
    tags$span("a"),
    tags$span("b"),
  )
  expect_length(node$children, 2L)
  expect_equal(
    render(node),
    "<p><span>a</span><span>b</span></p>"
  )
})

test_that("trailing comma after single attribute in tags$*()", {
  node <- tags$div(
    class = "container",
  )
  expect_equal(node$attrs$class, "container")
  expect_length(node$children, 0L)
  expect_equal(render(node), '<div class="container"></div>')
})

test_that("trailing comma after children when attributes present", {
  node <- tags$div(
    class = "wrapper",
    tags$p("content"),
  )
  expect_equal(node$attrs$class, "wrapper")
  expect_length(node$children, 1L)
  expect_equal(
    render(node),
    '<div class="wrapper"><p>content</p></div>'
  )
})

test_that("trailing comma in void tag", {
  node <- tags$input(
    type = "text",
    name = "field",
  )
  expect_equal(node$tag_type, "void")
  expect_equal(
    render(node),
    '<input type="text" name="field" />'
  )
})

# -- trailing commas in tag_list() ------------------------------------------

test_that("trailing comma in tag_list() with single element", {
  tl <- tag_list(
    tags$p("one"),
  )
  expect_s3_class(tl, "hypertext.tag.list")
  expect_length(tl, 1L)
  expect_equal(render(tl), "<p>one</p>")
})

test_that("trailing comma in tag_list() with multiple elements", {
  tl <- tag_list(
    tags$h1("title"),
    tags$p("body"),
  )
  expect_length(tl, 2L)
  expect_equal(render(tl), "<h1>title</h1><p>body</p>")
})

test_that("trailing comma in tag_list() with mixed tags and text", {
  tl <- tag_list(
    "hello ",
    tags$strong("world"),
  )
  expect_length(tl, 2L)
  expect_equal(render(tl), "hello <strong>world</strong>")
})

# -- nested tags with trailing commas ---------------------------------------

test_that("trailing comma in both outer and inner nested tags", {
  node <- tags$div(
    tags$p(
      tags$span("inner"),
    ),
  )
  expect_equal(
    render(node),
    "<div><p><span>inner</span></p></div>"
  )
})

test_that("trailing comma only in outer tag", {
  node <- tags$div(
    tags$p(tags$span("inner")),
  )
  expect_equal(
    render(node),
    "<div><p><span>inner</span></p></div>"
  )
})

test_that("trailing comma only in inner tag", {
  node <- tags$div(
    tags$p(
      tags$span("inner"),
    )
  )
  expect_equal(
    render(node),
    "<div><p><span>inner</span></p></div>"
  )
})

test_that("deeply nested tags with trailing commas at every level", {
  node <- tags$div(
    tags$section(
      tags$article(
        tags$p(
          tags$span("deep"),
        ),
      ),
    ),
  )
  expect_equal(
    render(node),
    paste0(
      "<div><section><article>",
      "<p><span>deep</span></p>",
      "</article></section></div>"
    )
  )
})

# -- trailing commas referencing outer scope variables ----------------------

test_that("trailing comma after variable indexing", {
  x <- c("alpha", "beta", "gamma")
  node <- tags$p(
    x[1],
  )
  expect_equal(node$children, list("alpha"))
  expect_equal(render(node), "<p>alpha</p>")
})

test_that("trailing comma after multiple variable references", {
  x <- c("hello", "world")
  node <- tag_list(
    tags$div(
      tags$p(
        x[1],
        " ",
      ),
      tags$p(
        x[2],
      ),
    ),
  )
  expect_equal(render(node), "<div><p>hello </p><p>world</p></div>")
})

test_that("trailing comma after lapply referencing outer variable", {
  items <- c("a", "b", "c")
  node <- tags$ul(
    lapply(
      X = items,
      FUN = function(x) {
        tags$li(
          paste("item:", x),
        )
      }
    ),
  )
  expect_length(node$children, 3L)
  expect_equal(
    render(node),
    "<ul><li>item: a</li><li>item: b</li><li>item: c</li></ul>"
  )
})

test_that("tag_list with lapply and trailing comma", {
  items <- c("a", "b", "c")
  tl <- tag_list(
    tags$h3("a page"),
    lapply(items, tags$p),
  )
  expect_length(tl, 2L)
  expect_equal(
    render(tl),
    "<h3>a page</h3><p>a</p><p>b</p><p>c</p>"
  )
})

test_that("nested tags referencing outer variable with trailing commas", {
  x <- c("a", "b", "c")
  node <- tag_list(
    tags$h3("heading"),
    tags$p(x[1], ),
    lapply(x, tags$span),
  )
  expect_length(node, 3L)
  expect_equal(
    render(node),
    paste0(
      "<h3>heading</h3>",
      "<p>a</p>",
      "<span>a</span><span>b</span><span>c</span>"
    )
  )
})

test_that("trailing comma with variable used as attribute value", {
  cls <- "highlight"
  node <- tags$div(
    class = cls,
    tags$p("text"),
  )
  expect_equal(node$attrs$class, "highlight")
  expect_equal(
    render(node),
    '<div class="highlight"><p>text</p></div>'
  )
})

test_that("trailing comma with computed expression as child", {
  vals <- 1:3
  node <- tags$p(
    paste(vals, collapse = ", "),
  )
  expect_equal(render(node), "<p>1, 2, 3</p>")
})

test_that("trailing comma after variable in nested tag inside tag_list", {
  title <- "My Page"
  items <- c("first", "second")
  tl <- tag_list(
    tags$h1(title, ),
    tags$ul(
      lapply(items, function(item) {
        tags$li(item, )
      }),
    ),
  )
  expect_equal(
    render(tl),
    paste0(
      "<h1>My Page</h1>",
      "<ul><li>first</li><li>second</li></ul>"
    )
  )
})

# -- trailing commas with NULL handling -------------------------------------

test_that("trailing comma with NULL child", {
  node <- tags$p(
    "text",
    NULL,
  )
  expect_equal(node$children, list("text"))
  expect_equal(render(node), "<p>text</p>")
})

test_that("trailing comma in tag_list with NULL element", {
  tl <- tag_list(
    tags$p("a"),
    NULL,
  )
  expect_length(tl, 1L)
  expect_equal(render(tl), "<p>a</p>")
})

test_that("trailing comma with only NULL in tags$*()", {
  node <- tags$div(
    NULL,
  )
  expect_equal(node$children, list())
  expect_equal(render(node), "<div></div>")
})

# -- trailing commas with raw_html -----------------------------------------

test_that("trailing comma after raw_html child", {
  node <- tags$div(
    raw_html("<em>raw</em>"),
  )
  expect_equal(render(node), "<div><em>raw</em></div>")
})

test_that("trailing comma in tag_list with raw_html", {
  tl <- tag_list(
    tags$p("text"),
    raw_html("<hr />"),
  )
  expect_equal(render(tl), "<p>text</p><hr />")
})

# -- trailing commas with boolean and special attributes --------------------

test_that("trailing comma after boolean attribute", {
  node <- tags$input(
    type = "checkbox",
    checked = TRUE,
  )
  html <- render(node)
  expect_match(html, "checked")
  expect_match(html, 'type="checkbox"')
})

test_that("trailing comma with FALSE and NULL attributes", {
  node <- tags$div(
    hidden = FALSE,
    data = NULL,
  )
  expect_equal(render(node), "<div></div>")
})

# -- trailing commas with numeric children ----------------------------------

test_that("trailing comma after numeric child", {
  node <- tags$span(
    42,
  )
  expect_equal(render(node), "<span>42</span>")
})

test_that("trailing comma with mixed numeric and text", {
  node <- tags$p(
    "Score: ",
    100,
  )
  expect_equal(render(node), "<p>Score: 100</p>")
})

# -- trailing commas in realistic page structure ----------------------------

test_that("realistic page with trailing commas throughout", {
  items <- c("Home", "About", "Contact")
  page <- tags$html(
    tags$head(
      tags$title("My Site"),
      tags$meta(charset = "utf-8", ),
    ),
    tags$body(
      tags$nav(
        class = "main-nav",
        tags$ul(
          lapply(items, function(item) {
            tags$li(
              tags$a(href = "#", item, ),
            )
          }),
        ),
      ),
      tags$main(
        tags$h1("Welcome", ),
        tags$p("Hello world", ),
      ),
    ),
  )
  html <- render(page)
  expect_match(html, "^<html>")
  expect_match(html, "</html>$")
  expect_match(html, "<title>My Site</title>")
  expect_match(html, '<meta charset="utf-8" />')
  expect_match(html, '<nav class="main-nav">')
  expect_match(html, "<li><a href=\"#\">Home</a></li>")
  expect_match(html, "<li><a href=\"#\">About</a></li>")
  expect_match(html, "<li><a href=\"#\">Contact</a></li>")
  expect_match(html, "<h1>Welcome</h1>")
  expect_match(html, "<p>Hello world</p>")
})

test_that("form with trailing commas", {
  form <- tags$form(
    action = "/submit",
    method = "post",
    tags$label(`for` = "email", "Email:", ),
    tags$input(type = "email", id = "email", name = "email", ),
    tags$button(type = "submit", "Send", ),
  )
  html <- render(form)
  expect_match(html, '<form action="/submit" method="post">')
  expect_match(html, '<label for="email">Email:</label>')
  expect_match(html, '<input type="email" id="email" name="email" />')
  expect_match(html, "<button type=\"submit\">Send</button>")
})

test_that("table built with trailing commas and lapply", {
  headers <- c("Name", "Age")
  rows <- list(
    c("Alice", "30"),
    c("Bob", "25")
  )
  tbl <- tags$table(
    class = "data",
    tags$thead(
      tags$tr(
        lapply(headers, tags$th),
      ),
    ),
    tags$tbody(
      lapply(rows, function(row) {
        tags$tr(
          lapply(row, tags$td),
        )
      }),
    ),
  )
  html <- render(tbl)
  expect_match(html, '<table class="data">')
  expect_match(html, "<th>Name</th><th>Age</th>")
  expect_match(html, "<td>Alice</td><td>30</td>")
  expect_match(html, "<td>Bob</td><td>25</td>")
})

# -- trailing commas with tag_list as child of tag --------------------------

test_that("trailing comma with tag_list as child of tag", {
  tl <- tag_list(
    tags$li("one"),
    tags$li("two"),
  )
  node <- tags$ul(
    tl,
  )
  expect_equal(render(node), "<ul><li>one</li><li>two</li></ul>")
})

test_that("trailing comma with tag_list mixed with other children", {
  tl <- tag_list(
    tags$li("b"),
    tags$li("c"),
  )
  node <- tags$ul(
    tags$li("a"),
    tl,
    tags$li("d"),
  )
  expect_equal(
    render(node),
    "<ul><li>a</li><li>b</li><li>c</li><li>d</li></ul>"
  )
})

# -- trailing commas with multiple class values ----------------------------

test_that("trailing comma with collapsed class vector", {
  node <- tags$div(
    class = c("container", "mx-auto", "p-4"),
    "content",
  )
  expect_equal(
    render(node),
    '<div class="container mx-auto p-4">content</div>'
  )
})

# -- trailing commas with conditional (NULL) children ----------------------

test_that("trailing comma with conditional NULL children from expressions", {
  show_extra <- FALSE
  node <- tags$div(
    tags$p("always"),
    if (show_extra) tags$p("extra"),
  )
  expect_length(node$children, 1L)
  expect_equal(render(node), "<div><p>always</p></div>")
})

test_that("trailing comma with conditional TRUE children from expressions", {
  show_extra <- TRUE
  node <- tags$div(
    tags$p("always"),
    if (show_extra) tags$p("extra"),
  )
  expect_length(node$children, 2L)
  expect_equal(
    render(node),
    "<div><p>always</p><p>extra</p></div>"
  )
})

test_that("trailing comma in tag_list with conditional children", {
  show_nav <- FALSE
  tl <- tag_list(
    tags$h1("Title"),
    if (show_nav) tags$nav("nav content"),
    tags$main("main content"),
  )
  # NULL from if() is filtered out by tag_list
  expect_length(tl, 2L)
  expect_equal(
    render(tl),
    "<h1>Title</h1><main>main content</main>"
  )
})
