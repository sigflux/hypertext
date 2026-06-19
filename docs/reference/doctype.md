# Render the `<!DOCTYPE html>` declaration

Convenience wrapper around
[`raw_html()`](https://sigflux.github.io/hypertext/reference/raw_html.md)
that returns the HTML5 document-type declaration. Useful when building a
full page.

## Usage

``` r
doctype()
```

## Value

A `"hypertext.raw"` object containing `<!DOCTYPE html>`.

## Examples

``` r
page <- tag_list(
  doctype(),
  tags$html(
    tags$head(tags$title("Home")),
    tags$body(tags$h1("Welcome"))
  )
)
render(page)
#> [1] "<!DOCTYPE html><html><head><title>Home</title></head><body><h1>Welcome</h1></body></html>"
```
