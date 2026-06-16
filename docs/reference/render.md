# Render an HTML node tree to a character string

Converts a `hypertext.tag` object (and all its descendants) into an HTML
string. Text children are escaped; nested `hypertext.tag` children are
rendered recursively.

## Usage

``` r
render(x, ...)

# S3 method for class 'hypertext.tag'
render(x, file = "", write_mode = c("overwrite", "append"), ...)

# Default S3 method
render(x, file = "", write_mode = c("overwrite", "append"), ...)

# S3 method for class 'hypertext.raw'
render(x, file = "", write_mode = c("overwrite", "append"), ...)

# S3 method for class 'list'
render(x, file = "", write_mode = c("overwrite", "append"), ...)
```

## Arguments

- x:

  `hypertext.tag` object, a string, or a list of these /// Required.

- ...:

  Further arguments passed from or to other methods.

- file:

  String /// Optional. Path to file to print to.

- write_mode:

  String /// Optional. Either "overwrite" (default) which overwrites the
  contents of `file`, or "append" which appends the HTML string to
  `file`.

## Value

A single character string of HTML.

## Details

When `file` is provided, the rendered HTML is written to the specified
file via [`cat()`](https://rdrr.io/r/base/cat.html) and the HTML string
is returned invisibly.

## Examples

``` r
page <- tags$html(
  tags$head(
    tags$title("Home")
  ),
  tags$body(
    tags$h1("Welcome")
  )
)

# return HTML as a string:
render(page)
#> [1] "<html><head><title>Home</title></head><body><h1>Welcome</h1></body></html>"

if (FALSE) { # \dontrun{
  # write to a file:
  render(page, file = "index.html")
} # }
```
