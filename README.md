# hypertext

minimal HTML element construction for R.

`hypertext` provides a small, deterministic DSL for
building HTML nodes in R and rendering them to a string.
it does not implement templating, dependency management,
widgets or framework integrations.

heavily inspired by [{htmltools}](https://github.com/rstudio/htmltools).

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

