# hypertext

minimal HTML element construction for R.

`hypertext` provides a small, deterministic DSL for
building HTML nodes in R and rendering them to a string.
it does not implement templating, dependency management,
widgets or framework integrations.

heavily inspired by [{htmltools}](https://github.com/rstudio/htmltools).

## installation

install the stable version from CRAN:

```r
install.packages("hypertext")
```

or get the development version from github:

```r
devtools::install_github("kennedymwavu/hypertext")
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
    tags$button(
      disabled = NA,
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
#> [1] "ht_tag"

# rendering produces an HTML string:
render(x)
#> [1] "<p class=\"lead\">hello</p>"
```
