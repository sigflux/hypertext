# Create an HTML element node

Low-level constructor. Named arguments become attributes; unnamed
arguments become children (text or nested nodes).

## Usage

``` r
tag(tag_name, ..., tag_type = c("normal", "void"))
```

## Arguments

- tag_name:

  String /// Required. The HTML element name.

- ...:

  Attributes (named) and children (unnamed) /// Optional.

- tag_type:

  String /// Optional. Either `"normal"` (default) for a standard
  element with children, or `"void"` for a self-closing element whose
  children are ignored.

## Value

List of class `"hypertext.tag"` with components `tag`, `attrs`,
`children`, and `tag_type`.

## Examples

``` r
# web component
tag(tag_name = "calcite-action-bar", layout = "horizontal")
#> <calcite-action-bar layout="horizontal"></calcite-action-bar>

# custom element with children
tag(
  tag_name = "my-card",
  class = "shadow",
  tag(tag_name = "my-card-header", "Title"),
  tag(tag_name = "my-card-body", "Content")
)
#> <my-card class="shadow">
#>   <my-card-header>Title</my-card-header>
#>   <my-card-body>Content</my-card-body>
#> </my-card>

# custom void element
tag(tag_name = "my-icon", name = "home", tag_type = "void")
#> <my-icon name="home" />
```
