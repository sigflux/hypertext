# Mark a string as raw HTML

Wraps one or more character strings so that
[`render()`](https://sigflux.github.io/hypertext/reference/render.md)
outputs them verbatim, **without** escaping HTML special characters.

## Usage

``` r
raw_html(...)
```

## Arguments

- ...:

  Character strings of raw HTML /// Optional.

## Value

A character vector of class `"hypertext.raw"`.

## Details

This is useful for injecting pre-rendered HTML, inline
`<script>`/`<style>` blocks, SVG markup, or any content that should not
be entity-escaped.

## Examples

``` r
raw_html("<script>alert('hi')</script>")
#> <script>alert('hi')</script>
render(tags$div(raw_html("<em>already formatted</em>")))
#> [1] "<div><em>already formatted</em></div>"
```
