# HTML and SVG tag functions

A named list of functions, one per HTML and SVG element. Access
individual tags with `tags$div(...)`, `tags$p(...)`, `tags$svg(...)`,
etc.

## Usage

``` r
tags
```

## Details

Named arguments become HTML attributes; unnamed arguments become child
nodes or text content.

## Boolean / valueless attributes

Pass `NA` or `TRUE` as the attribute value to produce a valueless
attribute (e.g. `disabled`). `FALSE` or `NULL` suppresses the attribute
entirely.

## Multi-value attributes

Character vectors of length \> 1 are collapsed with a space, so
`class = c("a", "b")` renders as `class="a b"`.

## Examples

``` r
tags$p(class = "lead", "Hello, world!")
#> <p class="lead">Hello, world!</p> 
render(tags$div(id = "app", tags$h1("Title")))
#> [1] "<div id=\"app\"><h1>Title</h1></div>"
```
