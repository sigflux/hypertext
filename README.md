# hypertext

<!-- badges: start -->

[![R-CMD-check](https://github.com/sigflux/hypertext/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/sigflux/hypertext/actions/workflows/R-CMD-check.yaml)
[![CRAN status](https://www.r-pkg.org/badges/version/hypertext)](https://CRAN.R-project.org/package=hypertext)
[![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)

<!-- badges: end -->

html element construction for R.

`hypertext` provides a deterministic, framework-agnostic DSL for building
html nodes and rendering them to a string. it does _not_ implement templating,
dependency management, widgets or framework integrations.

heavily inspired by [{htmltools}](https://github.com/rstudio/htmltools).

## installation

install the stable version from CRAN:

```r
install.packages("hypertext")
```

or get the development version from github:

```r
devtools::install_github("sigflux/hypertext")
```

## quick start

```r
library(hypertext)

page <- tags$html(
  tags$head(
    tags$title("hypertext")
  ),
  tags$body(
    tags$h1("Hello"),
    tags$p(
      class = c("lead", "mb-2"),
      "Server-side HTML."
    ),
    tags$input(
      type = "text",
      placeholder = "enter your nickname"
    ),
    tags$button(
      "Click"
    )
  )
)

render(page)
```

## rendering

`render()` takes a tag tree and returns a single HTML string:

```r
x <- tags$p(
  class = "lead",
  "hello"
)

# `x` contains the tag tree
class(x)
#> [1] "hypertext.tag"

# rendering produces an HTML string:
render(x)
#> [1] "<p class=\"lead\">hello</p>"
```

## creating html files

```r
library(hypertext)

page <- tags$html(
  tags$head(
    tags$title("hypertext")
  ),
  tags$body(
    tags$h1("Hello"),
    tags$p(
      class = c("lead", "mb-2"),
      "Server-side HTML."
    ),
    tags$input(
      type = "text",
      placeholder = "enter your nickname"
    ),
    tags$button(
      "Click"
    )
  )
)

content <- render(page)

writeLines(text = content, con = "index.html")
```

## usage in frameworks

- [ambiorix](https://ambiorix.dev/) is the perfect example of
  a web framework where you will find {hypertext} useful:

  ```r
  library(ambiorix)
  library(hypertext)

  app <- Ambiorix$new(port = 3000L)

  app$get("/", function(req, res) {
    html <- tags$h1("hello, world!") |>
      render()

    res$send(html)
  })

  app$get("/about", function(req, res) {
    html <- list(
      tags$h1("about us"),
      tags$p(
        "minimal ",
        tags$strong("html construction"),
        " for R."
      )
    ) |>
      render()

    res$send(html)
  })

  app$get("/team", function(req, res) {
    teammates <- c("you", "me", "other")

    html <- tags$div(
      class = "team",
      tags$p("meet the team:"),
      tags$ul(
        lapply(teammates, tags$li)
      )
    ) |>
      render()

    res$send(html)
  })

  app$start()
  ```

  - [shiny](https://shiny.posit.co/) already has {htmltools} tags
    internally, so you do not need {hypertext} in your shiny apps,
    but in case you do:

  ```r
  library(shiny)
  library(bslib)
  library(hypertext)

  # use `hypertext::tags` explicitly to avoid clashing with `shiny::tags`.
  ht <- hypertext::tags

  card <- function(title, body) {
    ht$div(
      class = "card mt-3",
      ht$div(class = "card-header", title),
      ht$div(class = "card-body", ht$p(class = "card-text", body))
    )
  }

  content <- ht$div(
    class = "container py-4",
    card("First card", "Some quick example text."),
    card("Second card", "Another body of text.")
  ) |>
    render()

  ui <- page(
    theme = bs_theme(version = 5L),
    # hypertext renders an HTML string, so wrap in shiny::HTML()
    HTML(content)
  )

  server <- function(input, output, session) {}

  shinyApp(ui, server)
  ```
