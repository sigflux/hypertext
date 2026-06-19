# Create a tag list (sibling container)

Useful when you want to collect sibling nodes into a single object
without wrapping them in a parent element.

## Usage

``` r
tag_list(...)
```

## Arguments

- ...:

  Tags, text, or other renderable objects /// Optional.

## Value

A list of class `"hypertext.tag.list"`.

## Examples

``` r
tl <- tag_list(tags$p("one"), tags$p("two"))
render(tl)
#> [1] "<p>one</p><p>two</p>"
```
