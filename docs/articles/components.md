# building components

``` r
library(hypertext)
```

the core idea in hypertext is simple: tag functions are just R
functions, and R functions compose naturally. a component is just a
function that returns a `hypertext.tag`.

with `hypertext`, you can use any css framework of your choice. but for
the examples that follow we’ll use bootstrap classes for demo purposes
and because this site is built using `pkgdown` which uses bootstrap by
default.

## basic component

the simplest component is a function that wraps a tag with a fixed
structure, exposing only the parts that change as arguments.

a really good example here is a button:

``` r
btn <- tags$button(
  type = "button",
  class = "btn btn-primary",
  "Click Me!"
)
```

in your app and depending on your styling, this snippet might look like
this:

``` r
cat(
  render(btn)
)
```

Click Me!

if you want to re-use this snippet in other parts of your app, you need
to convert it to a component:

``` r
Button <- function() {
  tags$button(
    type = "button",
    class = "btn btn-primary",
    "Click Me!"
  )
}

cat(
  render(Button())
)
```

Click Me!

that’d be the default button rendered. but sometimes, you need to tweak
the button a little bit. so let’s identify the moving parts:

- `type`: As per [mdn
  docs](https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/button#type),
  the type can either be:
  - “submit”: the button submits form data to the server.
  - “reset”: the button resets all the controls to their initial values.
  - “button”: the button has no default behaviour, and does nothing when
    pressed by default.
- `class`: we’d need to pass more \[custom\] classes to our button.
  these extra classes would dictate it’s styling and behaviour when
  clicked.
- `label`: each button in our app probably has a different label.

with that, let’s now improve the `Button()` component:

``` r
#' A Button
#'
#' @param label String /// Required.
#'              The button label.
#'
#' @param type String /// Optional.
#'             Button type. Either:
#'             - "button" (default)
#'             - "submit"
#'             - "reset"
#'
#' @param class Character vector /// Optional.
#'              CSS classes to apply to the button.
#'
#' @return `hypertext.tag`
#'
#' @export
Button <- function(
  label,
  type = c("button", "submit", "reset"),
  class = NULL
) {
  type <- match.arg(arg = type)

  tags$button(
    type = type,
    class = c("btn", class),
    label
  )
}
```

see how this works? we even added some function docs there. practically,
you’d call the component like this:

``` r
btn_a <- Button(
  label = "Are you sure?",
  class = "btn-warning"
)

btn_b <- Button(
  label = "Submit",
  type = "submit",
  class = "btn-primary"
)

btn_c <- Button(
  label = "Confirm",
  class = "btn-success"
)

btn_d <- Button(
  label = "Delete",
  class = "btn-danger"
)
```

and this how those would look:

``` r
my_btns <- tag_list(
  btn_a,
  btn_b,
  btn_c,
  btn_d
)

cat(
  render(my_btns)
)
```

Are you sure?

Submit

Confirm

Delete

## composition

say we want a `Badge()` component. let’s first define some color
variants:

``` r
#' Bootstrap Color Variants
#'
#' @return Character vector.
color_variants <- function() {
  c(
    "primary",
    "secondary",
    "success",
    "danger",
    "warning",
    "info",
    "light",
    "dark"
  )
}
```

these variants map directly to the bootstrap utility classes.

here comes the `Badge()`:

``` r
#' A Badge
#'
#' @param label String /// Required.
#'              Label to show on the badge.
#'
#' @param variant String /// Optional.
#'                Color variant of the badge.
#'                Must be one of `color_variants()`.
#'                Defaults to the "primary" variant.
#'
#' @return `hypertext.tag`
#'
#' @export
Badge <- function(
  label,
  variant = color_variants() 
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

badge_a <- Badge("new")
badge_b <- Badge("deprecated", variant = "danger")
badge_c <- Badge("stable", variant = "success")
```

the caller controls the label and colour; the structure is fixed inside
the function.

``` r
cat(
  render(badge_a),
  render(badge_b),
  render(badge_c)
)
```

new deprecated stable

components can wrap other components.

an `Alert()` component can embed a `Badge()` for the severity label,
keeping both independently reusable.

``` r
#' An Alert
#'
#' @param message String /// Required.
#'                Message to show on the alert.
#'
#' @param variant String /// Optional.
#'                Color variant of the alert.
#'                Must be one of `color_variants()`.
#'                Defaults to the "primary" variant.
#'
#' @param dismissible String /// Optional.
#'                 Should the alert be dismissible? Either:
#'                 - "no" (default)
#'                 - "yes"
#'
#' @return `hypertext.tag`
#'
#' @export
Alert <- function(
  message,
  variant = color_variants(),
  dismissible = c("no", "yes")
) {
  variant <- match.arg(arg = variant)
  dismissible <- match.arg(arg = dismissible)

  severity_label <- Badge(
    label = variant,
    variant = variant
  )

  classes <- c(
    "alert",
    paste0("alert-", variant)
  )
  dismiss_btn <- NULL
  if (dismissible == "yes") {
    classes <- c(
      classes,
      "alert-dismissible fade show"
    )

    dismiss_btn <- tags$button(
      type = "button",
      class = "btn-close",
      `data-bs-dismiss` = "alert",
      `aria-label` = "Close"
    )
  }

  tags$div(
    role = "alert",
    class = classes,
    severity_label,
    " ",
    message,
    dismiss_btn
  )
}

alert_a <- Alert(
  message = "Your session will expire in 5 minutes.",
  variant = "warning"
)

alert_b <- Alert(
  message = "An error while processing your request.",
  variant = "danger"
)

alert_c <- Alert(
  message = "This is some info you need to know.",
  variant = "info",
  dismissible = "yes"
)

alert_d <- Alert(
  message = "Report rendered successfully.",
  variant = "success",
  dismissible = "yes"
)
```

again, depending on your styling and the classes you apply, those might
look like this in your app:

``` r
cat(
  render(alert_a),
  render(alert_b),
  render(alert_c),
  render(alert_d)
)
```

warning Your session will expire in 5 minutes.

danger An error while processing your request.

info This is some info you need to know.

success Report rendered successfully.

in that `Alert()` example, `dismiss_btn` is a conditional child, that’s
why we set it to `NULL` by default. `hypertext` tags drop all `NULL`
children and properties.

## ellipsis

sometimes, you need to pass an unknown number of tags as children to a
tag. that’s where the dots (`...`) come in.

the dots are useful when the component owns the outer structure, but the
caller owns the inner content.

let’s make a `Card()` component. the header structure is fixed, but the
body can contain any number of children.

``` r
#' A Card
#'
#' @param title String /// Required.
#'              Title to show in the card header.
#'
#' @param ... Tags, text, or other renderable objects /// Optional.
#'            Body content for the card.
#'
#' @return `hypertext.tag`
#'
#' @export
Card <- function(
  title,
  ...
) {
  tags$div(
    class = "card my-2",
    tags$div(
      class = "card-header",
      tags$strong(title)
    ),
    tags$div(
      class = "card-body",
      ...
    )
  )
}

card_a <- Card(
  title = "Report status",
  tags$p(
    class = "card-text",
    "The report rendered successfully."
  )
)

card_b <- Card(
  title = "Package status",
  tags$p(
    class = "card-text",
    "Current release channel: ",
    Badge("stable", variant = "success")
  ),
  Button(
    label = "View details",
    class = "btn-sm btn-primary"
  )
)
```

and here’s how those might look:

``` r
cards <- tag_list(
  card_a,
  card_b
)

cat(
  render(cards)
)
```

**Report status**

The report rendered successfully.

**Package status**

Current release channel: stable

View details

inside `Card()`, the `...` are forwarded into
`tags$div(class = "card-body")`. that means each call site can decide
whether the body is plain text, a paragraph, a badge, a button, or a mix
of all of them.

## mapping over items

you can map over items and return `hypertext.tag` objects. to do the
mapping, use list-returning R functions eg.
[`lapply()`](https://rdrr.io/r/base/lapply.html),
[`Map()`](https://rdrr.io/r/base/funprog.html), etc. plain lists
returned by these functions are flattened automatically when passed as
children to a tag.

here’s a `ListGroup()` component that maps a character vector to `<li>`
children:

``` r
#' A List Group
#'
#' @param items Character vector /// Required.
#'              Items to show in the list group.
#'
#' @return `hypertext.tag`
#'
#' @export
ListGroup <- function(items) {
  tags$ul(
    class = "list-group list-group-numbered my-2",
    lapply(
      X = items,
      FUN = function(item) {
        tags$li(
          class = "list-group-item",
          item
        )
      }
    )
  )
}

todos <- c(
  "Write the component",
  "Render it",
  "Reuse it"
)

todo_list <- ListGroup(todos)
```

``` r
cat(
  render(todo_list)
)
```

- Write the component
- Render it
- Reuse it

[`lapply()`](https://rdrr.io/r/base/lapply.html) returns a plain list of
`tags$li()` nodes. because that list is passed inside `tags$ul()`,
`hypertext` treats each item as a child of the `<ul>`.

use
[`tag_list()`](https://sigflux.github.io/hypertext/reference/tag_list.md)
when you want to group sibling nodes without adding a parent element.
use a plain mapped list when those sibling nodes already have a parent
tag, like the `<ul>` above.

## conclusion

components are functions that return `hypertext.tag` objects. you can
bake styling (css) and behaviour (js) into them, and they’re composable.
