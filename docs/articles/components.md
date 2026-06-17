# building components

``` r
library(hypertext)
```

the core idea in hypertext is simple: tag functions are just R
functions, and R functions compose naturally. no need for a framework or
a special component system — a component is just a function that returns
a `hypertext.tag`.

## basic component

the simplest component is a function that wraps a tag with a fixed
structure, exposing only the parts that change as arguments.

``` r
#' A Badge
#'
#' @param label String /// Required.
#'              Label to show on the badge.
#'
#' @param variant String /// Optional.
#'                Utility class. Must be one of:
#'                - "primary" (default)
#'                - "secondary"
#'                - "success"
#'                - "danger"
#'                - "warning"
#'                - "info"
#'                - "light"
#'                - "dark"
#'
#' @return `hypertext.tag`
#'
#' @export
badge <- function(
  label,
  variant = c(
    "primary",
    "secondary",
    "success",
    "danger",
    "warning",
    "info",
    "light",
    "dark"
  )
) {
  variant <- match.arg(arg = variant)

  tags$span(
    class = c(
      "badge",
      paste0("text-bg-", variant)
    ),
    label
  )
}

badge_a <- badge("new")
badge_b <- badge("deprecated", variant = "danger")
badge_c <- badge("stable", variant = "success")
```

``` r
cat(
  render(badge_a),
  render(badge_b),
  render(badge_c)
)
```

new deprecated stable

the `variant` argument maps directly to a bootstrap utility class. the
caller controls the label and colour; the structure is fixed inside the
function.

## composition

components can call other components.

an `alert()` component can embed a `badge()` for the severity label,
keeping both independently reusable.

``` r
#' An Alert
#'
#' @param message String /// Required.
#'                Message to show on the alert.
#'
#' @param type String /// Optional.
#'                Alert type. A utility class. Must be one of:
#'                - "info" (default)
#'                - "danger"
#'                - "warning"
#'
#' @param dismissable String /// Optional.
#'                 Should the alert be dismissable? Either:
#'                 - "no" (default)
#'                 - "yes"
#'
#' @return `hypertext.tag`
#'
#' @export
alert <- function(
  message,
  type = c("info", "danger", "warning"),
  dismissable = c("no", "yes")
) {
  type <- match.arg(arg = type)
  dismissable <- match.arg(arg = dismissable)

  alert_classes <- c("alert", paste0("alert-", type))
  if (dismissable == "yes") {
    alert_classes <- c(
      alert_classes,
      "alert-dismissible fade show"
    )
  }

  label <- switch(
    type,
    info = badge(label = "info", variant = "primary"),
    warning = badge(label = "warning", variant = "warning"),
    danger = badge(label = "danger", variant = "danger"),
    badge(label = type)
  )

  tags$div(
    class = alert_classes,
    role = "alert",
    label,
    " ",
    message
  )
}

alert_a <- alert(
  message = "Your session will expire in 5 minutes.",
  type = "warning"
)

alert_b <- alert(
  message = "Record deleted successfully.",
  type = "danger"
)

alert_c <- alert(
  message = "This is some info you need to know.",
  dismissable = "yes"
)
```

``` r
cat(
  render(alert_a),
  render(alert_b),
  render(alert_c)
)
```

warning Your session will expire in 5 minutes.

danger Record deleted successfully.

info This is some info you need to know.

## Conditional children

Use `NULL` as a sentinel for optional children.
[`render()`](https://sigflux.github.io/hypertext/reference/render.md)
drops `NULL` values silently, so you can conditionally include children
without special-casing the rendering logic.

``` r
card <- function(
  title,
  body,
  footer = NULL
) {
  tags$div(
    class = "card",
    tags$div(
      class = "card-header",
      tags$strong(title)
    ),
    tags$div(
      class = "card-body",
      tags$p(
        class = "card-text",
        body
      )
    ),
    if (!is.null(footer)) {
      tags$div(
        class = "card-footer text-muted",
        footer
      )
    }
  )
}

# without footer
card_a <- card(
  title = "Title A",
  body = "Some body text."
)
# with footer
card_b <- card(
  title = "Title B",
  body = "Some body text.",
  footer = "Last updated: today"
)

render(card_a)
#> [1] "<div class=\"card\"><div class=\"card-header\"><strong>Title A</strong></div><div class=\"card-body\"><p class=\"card-text\">Some body text.</p></div></div>"
render(card_b)
#> [1] "<div class=\"card\"><div class=\"card-header\"><strong>Title B</strong></div><div class=\"card-body\"><p class=\"card-text\">Some body text.</p></div><div class=\"card-footer text-muted\">Last updated: today</div></div>"
```

``` r
cat(
render(card_a),
render(card_b)
)
```

**Title A**

Some body text.

**Title B**

Some body text.

Last updated: today

When `footer` is `NULL` the `if` expression evaluates to `NULL`, which
hypertext simply ignores. No conditional rendering plumbing needed.

## List children

[`lapply()`](https://rdrr.io/r/base/lapply.html) returns a plain list,
and hypertext flattens plain lists of children automatically. This means
you can map over a vector to produce a list of child nodes without any
extra unwrapping.

``` r
nav_bar <- function(links) {
  # links: named character vector, names = labels, values = hrefs
  items <- lapply(
    X = names(links),
    FUN = function(label) {
      tags$li(
        class = "nav-item",
        tags$a(
          class = "nav-link",
          href = links[[label]],
          label
        )
      )
    }
  )

  tags$nav(
    tags$ul(
      class = "nav",
      items
    )
  )
}

site_links <- c(
  "Home" = "/",
  "Reference" = "/reference/",
  "Articles" = "/articles/"
)

my_nav <- nav_bar(site_links)
```

``` r
cat(
  render(my_nav)
)
```

- [Home](https://sigflux.github.io/)
- [Reference](https://sigflux.github.io/reference/)
- [Articles](https://sigflux.github.io/articles/)

## Worked example — profile card

Putting it together: a `profile_card()` component that combines an
image, a heading, a paragraph, and the `badge()` component from earlier.

``` r
profile_card <- function(name, role, bio, avatar_url) {
  tags$div(
    class = "card",
    style = "max-width: 320px;",
    tags$img(
      src = avatar_url,
      class = "card-img-top",
      alt = paste("Photo of", name)
    ),
    tags$div(
      class = "card-body",
      tags$h5(class = "card-title", name),
      badge(role, variant = "secondary"),
      tags$p(class = "card-text mt-2", bio)
    )
  )
}

my_profile <- profile_card(
  name = "Ada Lovelace",
  role = "mathematician",
  bio = paste(
    "First to recognise that a computing machine had",
    "applications beyond pure calculation."
  ),
  avatar_url = "https://example.com/ada.jpg"
)
```

``` r
cat(
  render(my_profile)
)
```

![Photo of Ada Lovelace](https://example.com/ada.jpg)

##### Ada Lovelace

mathematician

First to recognise that a computing machine had applications beyond pure
calculation.

## Summary

| Pattern | How |
|----|----|
| Fixed structure, variable content | Function argument → attribute or child |
| Optional children | Default to `NULL`; `if (!is.null(x))` guard |
| Composition | Call one component function inside another |
| Lists of children | [`lapply()`](https://rdrr.io/r/base/lapply.html) → plain list → auto-flattened |
| Reuse across pages | Source the file or put components in a package |

A component library for a real project is just a collection of these
functions. There is no registration, no lifecycle, no virtual DOM — just
R functions that return tag objects.
