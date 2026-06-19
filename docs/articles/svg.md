# svg with hypertext

``` r
library(hypertext)
```

svg looks like a different world, but it is still just nested tags with
attributes. `tags$svg()`, `tags$circle()`, `tags$rect()`, `tags$path()`,
`tags$text()`, and friends all return `hypertext.tag` objects.

that means everything from the components article still applies: start
with a tag, identify the moving parts, wrap it in a function, then
compose those functions into something more interesting.

the nice part is that the result is inline svg. no image files, no
base64 strings in your source, no `<img>` tag. the graphic is part of
the document.

## first shape

an svg needs a canvas. the `viewBox` says what coordinate system to use.
inside that canvas, every shape is just another tag.

``` r
orb <- tags$svg(
  xmlns = "http://www.w3.org/2000/svg",
  viewBox = "0 0 120 120",
  width = "120",
  height = "120",
  tags$circle(
    cx = "60",
    cy = "60",
    r = "42",
    fill = "#22d3ee",
    stroke = "#0f172a",
    `stroke-width` = "4"
  )
)
```

``` r
cat(
  render(orb)
)
```

![](data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHZpZXdib3g9IjAgMCAxMjAgMTIwIiB3aWR0aD0iMTIwIiBoZWlnaHQ9IjEyMCI+PGNpcmNsZSBjeD0iNjAiIGN5PSI2MCIgcj0iNDIiIGZpbGw9IiMyMmQzZWUiIHN0cm9rZT0iIzBmMTcyYSIgc3Ryb2tlLXdpZHRoPSI0Ij48L2NpcmNsZT48L3N2Zz4=)

named arguments become svg attributes. so `cx`, `cy`, `r`, `fill`, and
`stroke` are all rendered directly onto the `<circle>` element.

## a tray of shapes

once you have one shape, you can place several shapes inside the same
svg. the coordinate system starts at the top-left. `x` increases as you
move right; `y` increases as you move down.

``` r
shape_tray <- tags$svg(
  xmlns = "http://www.w3.org/2000/svg",
  viewBox = "0 0 520 150",
  width = "520",
  height = "150",
  tags$rect(
    x = "0",
    y = "0",
    width = "520",
    height = "150",
    rx = "22",
    fill = "#020617"
  ),
  tags$circle(
    cx = "75",
    cy = "75",
    r = "38",
    fill = "#22d3ee"
  ),
  tags$rect(
    x = "150",
    y = "37",
    width = "76",
    height = "76",
    rx = "18",
    fill = "#a78bfa"
  ),
  tags$line(
    x1 = "282",
    y1 = "110",
    x2 = "358",
    y2 = "40",
    stroke = "#facc15",
    `stroke-width` = "12",
    `stroke-linecap` = "round"
  ),
  tags$path(
    d = "M410 102 C430 20, 478 20, 498 102",
    fill = "none",
    stroke = "#fb7185",
    `stroke-width` = "10",
    `stroke-linecap` = "round"
  )
)
```

``` r
cat(
  render(shape_tray)
)
```

![](data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHZpZXdib3g9IjAgMCA1MjAgMTUwIiB3aWR0aD0iNTIwIiBoZWlnaHQ9IjE1MCI+PHJlY3QgeD0iMCIgeT0iMCIgd2lkdGg9IjUyMCIgaGVpZ2h0PSIxNTAiIHJ4PSIyMiIgZmlsbD0iIzAyMDYxNyIgLz48Y2lyY2xlIGN4PSI3NSIgY3k9Ijc1IiByPSIzOCIgZmlsbD0iIzIyZDNlZSI+PC9jaXJjbGU+PHJlY3QgeD0iMTUwIiB5PSIzNyIgd2lkdGg9Ijc2IiBoZWlnaHQ9Ijc2IiByeD0iMTgiIGZpbGw9IiNhNzhiZmEiIC8+PGxpbmUgeDE9IjI4MiIgeTE9IjExMCIgeDI9IjM1OCIgeTI9IjQwIiBzdHJva2U9IiNmYWNjMTUiIHN0cm9rZS13aWR0aD0iMTIiIHN0cm9rZS1saW5lY2FwPSJyb3VuZCI+PC9saW5lPjxwYXRoIGQ9Ik00MTAgMTAyIEM0MzAgMjAsIDQ3OCAyMCwgNDk4IDEwMiIgZmlsbD0ibm9uZSIgc3Ryb2tlPSIjZmI3MTg1IiBzdHJva2Utd2lkdGg9IjEwIiBzdHJva2UtbGluZWNhcD0icm91bmQiIC8+PC9zdmc+)

you can already see the pattern: svg is not special to `hypertext`. it
is the same tag nesting model, with a different set of elements and
attributes.

## svg as a component

we are going to write more svg, so let’s remove the repeated wrapper. a
small `Svg()` component can own the namespace, dimensions, and
`viewBox`; callers can pass any children through `...`.

``` r
#' An SVG Canvas
#'
#' @param width Number /// Required.
#'              Width of the rendered svg.
#'
#' @param height Number /// Required.
#'               Height of the rendered svg.
#'
#' @param ... Tags, text, or other renderable objects /// Optional.
#'            Children to place inside the svg.
#'
#' @param viewBox String /// Optional.
#'                SVG viewBox. Defaults to `0 0 width height`.
#'
#' @return `hypertext.tag`
#'
#' @export
Svg <- function(
  width,
  height,
  ...,
  viewBox = paste("0 0", width, height)
) {
  tags$svg(
    xmlns = "http://www.w3.org/2000/svg",
    viewBox = viewBox,
    width = width,
    height = height,
    role = "img",
    ...
  )
}
```

now the canvas is reusable:

``` r
night_sky <- Svg(
  width = 420,
  height = 180,
  tags$rect(
    x = "0",
    y = "0",
    width = "420",
    height = "180",
    rx = "24",
    fill = "#111827"
  ),
  tags$circle(cx = "90", cy = "70", r = "34", fill = "#38bdf8"),
  tags$circle(cx = "205", cy = "98", r = "50", fill = "#8b5cf6"),
  tags$circle(cx = "322", cy = "62", r = "26", fill = "#fb7185")
)
```

``` r
cat(
  render(night_sky)
)
```

![](data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHZpZXdib3g9IjAgMCA0MjAgMTgwIiB3aWR0aD0iNDIwIiBoZWlnaHQ9IjE4MCIgcm9sZT0iaW1nIj48cmVjdCB4PSIwIiB5PSIwIiB3aWR0aD0iNDIwIiBoZWlnaHQ9IjE4MCIgcng9IjI0IiBmaWxsPSIjMTExODI3IiAvPjxjaXJjbGUgY3g9IjkwIiBjeT0iNzAiIHI9IjM0IiBmaWxsPSIjMzhiZGY4Ij48L2NpcmNsZT48Y2lyY2xlIGN4PSIyMDUiIGN5PSI5OCIgcj0iNTAiIGZpbGw9IiM4YjVjZjYiPjwvY2lyY2xlPjxjaXJjbGUgY3g9IjMyMiIgY3k9IjYyIiByPSIyNiIgZmlsbD0iI2ZiNzE4NSI+PC9jaXJjbGU+PC9zdmc+)

that is the same `...` technique from html components. the component
owns the outer structure; the caller owns the inner content.

## shape components

we can go one level deeper and make components for the shapes too.

``` r
#' An Orb
#'
#' @param cx Number /// Required.
#'           Horizontal centre.
#'
#' @param cy Number /// Required.
#'           Vertical centre.
#'
#' @param r Number /// Required.
#'          Radius.
#'
#' @param fill String /// Optional.
#'             Fill colour.
#'
#' @param opacity Number /// Optional.
#'                Shape opacity.
#'
#' @return `hypertext.tag`
#'
#' @export
Orb <- function(
  cx,
  cy,
  r,
  fill = "currentColor",
  opacity = 1
) {
  tags$circle(
    cx = cx,
    cy = cy,
    r = r,
    fill = fill,
    opacity = opacity
  )
}

#' A Spark
#'
#' @param x Number /// Required.
#'          Horizontal centre.
#'
#' @param y Number /// Required.
#'          Vertical centre.
#'
#' @param size Number /// Optional.
#'             Spark size.
#'
#' @param color String /// Optional.
#'              Stroke colour.
#'
#' @return `hypertext.tag`
#'
#' @export
Spark <- function(
  x,
  y,
  size = 10,
  color = "#fde68a"
) {
  tags$g(
    stroke = color,
    `stroke-width` = "3",
    `stroke-linecap` = "round",
    tags$line(x1 = x - size, y1 = y, x2 = x + size, y2 = y),
    tags$line(x1 = x, y1 = y - size, x2 = x, y2 = y + size)
  )
}
```

now a scene is just composition:

``` r
scene <- Svg(
  width = 520,
  height = 220,
  tags$rect(
    x = "0",
    y = "0",
    width = "520",
    height = "220",
    rx = "28",
    fill = "#020617"
  ),
  Orb(cx = 115, cy = 112, r = 66, fill = "#22d3ee", opacity = 0.95),
  Orb(cx = 230, cy = 94, r = 88, fill = "#8b5cf6", opacity = 0.72),
  Orb(cx = 348, cy = 130, r = 58, fill = "#fb7185", opacity = 0.86),
  Spark(x = 415, y = 56, size = 13),
  Spark(x = 78, y = 46, size = 9, color = "#bae6fd"),
  Spark(x = 460, y = 150, size = 8, color = "#fecdd3")
)

cat(
  render(scene)
)
```

![](data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHZpZXdib3g9IjAgMCA1MjAgMjIwIiB3aWR0aD0iNTIwIiBoZWlnaHQ9IjIyMCIgcm9sZT0iaW1nIj48cmVjdCB4PSIwIiB5PSIwIiB3aWR0aD0iNTIwIiBoZWlnaHQ9IjIyMCIgcng9IjI4IiBmaWxsPSIjMDIwNjE3IiAvPjxjaXJjbGUgY3g9IjExNSIgY3k9IjExMiIgcj0iNjYiIGZpbGw9IiMyMmQzZWUiIG9wYWNpdHk9IjAuOTUiPjwvY2lyY2xlPjxjaXJjbGUgY3g9IjIzMCIgY3k9Ijk0IiByPSI4OCIgZmlsbD0iIzhiNWNmNiIgb3BhY2l0eT0iMC43MiI+PC9jaXJjbGU+PGNpcmNsZSBjeD0iMzQ4IiBjeT0iMTMwIiByPSI1OCIgZmlsbD0iI2ZiNzE4NSIgb3BhY2l0eT0iMC44NiI+PC9jaXJjbGU+PGcgc3Ryb2tlPSIjZmRlNjhhIiBzdHJva2Utd2lkdGg9IjMiIHN0cm9rZS1saW5lY2FwPSJyb3VuZCI+PGxpbmUgeDE9IjQwMiIgeTE9IjU2IiB4Mj0iNDI4IiB5Mj0iNTYiPjwvbGluZT48bGluZSB4MT0iNDE1IiB5MT0iNDMiIHgyPSI0MTUiIHkyPSI2OSI+PC9saW5lPjwvZz48ZyBzdHJva2U9IiNiYWU2ZmQiIHN0cm9rZS13aWR0aD0iMyIgc3Ryb2tlLWxpbmVjYXA9InJvdW5kIj48bGluZSB4MT0iNjkiIHkxPSI0NiIgeDI9Ijg3IiB5Mj0iNDYiPjwvbGluZT48bGluZSB4MT0iNzgiIHkxPSIzNyIgeDI9Ijc4IiB5Mj0iNTUiPjwvbGluZT48L2c+PGcgc3Ryb2tlPSIjZmVjZGQzIiBzdHJva2Utd2lkdGg9IjMiIHN0cm9rZS1saW5lY2FwPSJyb3VuZCI+PGxpbmUgeDE9IjQ1MiIgeTE9IjE1MCIgeDI9IjQ2OCIgeTI9IjE1MCI+PC9saW5lPjxsaW5lIHgxPSI0NjAiIHkxPSIxNDIiIHgyPSI0NjAiIHkyPSIxNTgiPjwvbGluZT48L2c+PC9zdmc+)

## defs and gradients

some svg elements define things instead of drawing things immediately.
`<defs>` is where those definitions live. gradients are the common
example: define a gradient once, give it an `id`, then reference it with
`url(#id)`.

let’s make a gradient component.

``` r
#' A Linear Gradient
#'
#' @param id String /// Required.
#'           Gradient id.
#'
#' @param colors Character vector /// Required.
#'               Colours in the gradient.
#'
#' @return `hypertext.tag`
#'
#' @export
LinearGradient <- function(id, colors) {
  stops <- lapply(
    X = seq_along(colors),
    FUN = function(i) {
      offset <- if (length(colors) == 1L) {
        0
      } else {
        (i - 1) / (length(colors) - 1) * 100
      }

      tags$stop(
        offset = paste0(round(offset), "%"),
        `stop-color` = colors[[i]]
      )
    }
  )

  tags$linearGradient(
    id = id,
    x1 = "0%",
    y1 = "0%",
    x2 = "100%",
    y2 = "100%",
    stops
  )
}
```

here it is inside a small mark:

``` r
signal_mark <- Svg(
  width = 520,
  height = 220,
  tags$defs(
    LinearGradient(
      id = "aurora",
      colors = c("#22d3ee", "#8b5cf6", "#fb7185")
    )
  ),
  tags$rect(
    x = "0",
    y = "0",
    width = "520",
    height = "220",
    rx = "28",
    fill = "#020617"
  ),
  tags$path(
    d = "M42 145 C112 35, 188 205, 258 96 S395 28, 478 132",
    fill = "none",
    stroke = "url(#aurora)",
    `stroke-width` = "18",
    `stroke-linecap` = "round"
  ),
  tags$text(
    x = "42",
    y = "58",
    fill = "#f8fafc",
    `font-size` = "24",
    `font-family` = "inherit",
    `font-weight` = "700",
    "inline svg, built in r"
  ),
  tags$text(
    x = "42",
    y = "86",
    fill = "#94a3b8",
    `font-size` = "14",
    `font-family` = "inherit",
    "components, gradients, paths, and text"
  )
)

cat(
  render(signal_mark)
)
```

![](data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHZpZXdib3g9IjAgMCA1MjAgMjIwIiB3aWR0aD0iNTIwIiBoZWlnaHQ9IjIyMCIgcm9sZT0iaW1nIj48ZGVmcz48bGluZWFyZ3JhZGllbnQgaWQ9ImF1cm9yYSIgeDE9IjAlIiB5MT0iMCUiIHgyPSIxMDAlIiB5Mj0iMTAwJSI+PHN0b3Agb2Zmc2V0PSIwJSIgc3RvcC1jb2xvcj0iIzIyZDNlZSI+PC9zdG9wPjxzdG9wIG9mZnNldD0iNTAlIiBzdG9wLWNvbG9yPSIjOGI1Y2Y2Ij48L3N0b3A+PHN0b3Agb2Zmc2V0PSIxMDAlIiBzdG9wLWNvbG9yPSIjZmI3MTg1Ij48L3N0b3A+PC9saW5lYXJncmFkaWVudD48L2RlZnM+PHJlY3QgeD0iMCIgeT0iMCIgd2lkdGg9IjUyMCIgaGVpZ2h0PSIyMjAiIHJ4PSIyOCIgZmlsbD0iIzAyMDYxNyIgLz48cGF0aCBkPSJNNDIgMTQ1IEMxMTIgMzUsIDE4OCAyMDUsIDI1OCA5NiBTMzk1IDI4LCA0NzggMTMyIiBmaWxsPSJub25lIiBzdHJva2U9InVybCgjYXVyb3JhKSIgc3Ryb2tlLXdpZHRoPSIxOCIgc3Ryb2tlLWxpbmVjYXA9InJvdW5kIiAvPjx0ZXh0IHg9IjQyIiB5PSI1OCIgZmlsbD0iI2Y4ZmFmYyIgZm9udC1zaXplPSIyNCIgZm9udC1mYW1pbHk9ImluaGVyaXQiIGZvbnQtd2VpZ2h0PSI3MDAiPmlubGluZQpzdmcsIGJ1aWx0IGluCnI8L3RleHQ+PHRleHQgeD0iNDIiIHk9Ijg2IiBmaWxsPSIjOTRhM2I4IiBmb250LXNpemU9IjE0IiBmb250LWZhbWlseT0iaW5oZXJpdCI+Y29tcG9uZW50cywKZ3JhZGllbnRzLCBwYXRocywgYW5kIHRleHQ8L3RleHQ+PC9zdmc+)

the gradient is not drawn by itself. it becomes useful when another
element references it. this is the same idea as defining a function and
calling it later.

## mapping data to svg

svg coordinates are just numbers, so data graphics are normal R
programming. compute `x`, `y`, `width`, and `height`, then pass those
values as attributes.

the only coordinate trick to remember is that svg starts at the
top-left. if a bar should be taller, its `height` gets bigger but its
`y` value gets smaller.

``` r
#' A Bar Chart
#'
#' @param values Named numeric vector /// Required.
#'               Values to plot.
#'
#' @param width Number /// Optional.
#'              SVG width.
#'
#' @param height Number /// Optional.
#'               SVG height.
#'
#' @param title String /// Optional.
#'              Chart title.
#'
#' @return `hypertext.tag`
#'
#' @export
BarChart <- function(
  values,
  width = 560,
  height = 260,
  title = "signal strength"
) {
  labels <- names(values)
  if (is.null(labels)) {
    labels <- as.character(seq_along(values))
  }

  pad_left <- 46
  pad_right <- 28
  pad_top <- 62
  pad_bottom <- 46
  gap <- 12

  plot_w <- width - pad_left - pad_right
  plot_h <- height - pad_top - pad_bottom
  bar_w <- (plot_w - gap * (length(values) - 1)) / length(values)
  max_value <- max(values)
  if (max_value == 0) {
    max_value <- 1
  }

  bars <- lapply(
    X = seq_along(values),
    FUN = function(i) {
      value <- values[[i]]
      bar_h <- value / max_value * plot_h
      bar_x <- pad_left + (i - 1) * (bar_w + gap)
      bar_y <- pad_top + plot_h - bar_h
      label_x <- bar_x + bar_w / 2

      tags$g(
        tags$rect(
          x = round(bar_x, 1),
          y = round(bar_y, 1),
          width = round(bar_w, 1),
          height = round(bar_h, 1),
          rx = "10",
          fill = "url(#bars)",
          opacity = "0.94"
        ),
        tags$text(
          x = round(label_x, 1),
          y = round(bar_y - 10, 1),
          `text-anchor` = "middle",
          `font-size` = "12",
          `font-family` = "inherit",
          `font-weight` = "700",
          fill = "#f8fafc",
          round(value, 1)
        ),
        tags$text(
          x = round(label_x, 1),
          y = height - 18,
          `text-anchor` = "middle",
          `font-size` = "12",
          `font-family` = "inherit",
          fill = "#cbd5e1",
          labels[[i]]
        )
      )
    }
  )

  Svg(
    width = width,
    height = height,
    tags$defs(
      LinearGradient(
        id = "bars",
        colors = c("#22d3ee", "#a78bfa")
      )
    ),
    tags$rect(
      x = "0",
      y = "0",
      width = width,
      height = height,
      rx = "28",
      fill = "#0f172a"
    ),
    tags$text(
      x = "28",
      y = "36",
      fill = "#f8fafc",
      `font-size` = "20",
      `font-family` = "inherit",
      `font-weight` = "700",
      title
    ),
    tags$line(
      x1 = pad_left,
      y1 = pad_top + plot_h,
      x2 = width - pad_right,
      y2 = pad_top + plot_h,
      stroke = "#334155",
      `stroke-width` = "2"
    ),
    bars
  )
}
```

``` r
launch_scores <- c(
  alpha = 42,
  beta = 68,
  gamma = 51,
  delta = 91,
  omega = 74
)

chart_a <- BarChart(launch_scores)

cat(
  render(chart_a)
)
```

![](data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHZpZXdib3g9IjAgMCA1NjAgMjYwIiB3aWR0aD0iNTYwIiBoZWlnaHQ9IjI2MCIgcm9sZT0iaW1nIj48ZGVmcz48bGluZWFyZ3JhZGllbnQgaWQ9ImJhcnMiIHgxPSIwJSIgeTE9IjAlIiB4Mj0iMTAwJSIgeTI9IjEwMCUiPjxzdG9wIG9mZnNldD0iMCUiIHN0b3AtY29sb3I9IiMyMmQzZWUiPjwvc3RvcD48c3RvcCBvZmZzZXQ9IjEwMCUiIHN0b3AtY29sb3I9IiNhNzhiZmEiPjwvc3RvcD48L2xpbmVhcmdyYWRpZW50PjwvZGVmcz48cmVjdCB4PSIwIiB5PSIwIiB3aWR0aD0iNTYwIiBoZWlnaHQ9IjI2MCIgcng9IjI4IiBmaWxsPSIjMGYxNzJhIiAvPjx0ZXh0IHg9IjI4IiB5PSIzNiIgZmlsbD0iI2Y4ZmFmYyIgZm9udC1zaXplPSIyMCIgZm9udC1mYW1pbHk9ImluaGVyaXQiIGZvbnQtd2VpZ2h0PSI3MDAiPnNpZ25hbApzdHJlbmd0aDwvdGV4dD48bGluZSB4MT0iNDYiIHkxPSIyMTQiIHgyPSI1MzIiIHkyPSIyMTQiIHN0cm9rZT0iIzMzNDE1NSIgc3Ryb2tlLXdpZHRoPSIyIj48L2xpbmU+PGc+PHJlY3QgeD0iNDYiIHk9IjE0My44IiB3aWR0aD0iODcuNiIgaGVpZ2h0PSI3MC4yIiByeD0iMTAiIGZpbGw9InVybCgjYmFycykiIG9wYWNpdHk9IjAuOTQiIC8+PHRleHQgeD0iODkuOCIgeT0iMTMzLjgiIHRleHQtYW5jaG9yPSJtaWRkbGUiIGZvbnQtc2l6ZT0iMTIiIGZvbnQtZmFtaWx5PSJpbmhlcml0IiBmb250LXdlaWdodD0iNzAwIiBmaWxsPSIjZjhmYWZjIj40MjwvdGV4dD48dGV4dCB4PSI4OS44IiB5PSIyNDIiIHRleHQtYW5jaG9yPSJtaWRkbGUiIGZvbnQtc2l6ZT0iMTIiIGZvbnQtZmFtaWx5PSJpbmhlcml0IiBmaWxsPSIjY2JkNWUxIj5hbHBoYTwvdGV4dD48L2c+PGc+PHJlY3QgeD0iMTQ1LjYiIHk9IjEwMC40IiB3aWR0aD0iODcuNiIgaGVpZ2h0PSIxMTMuNiIgcng9IjEwIiBmaWxsPSJ1cmwoI2JhcnMpIiBvcGFjaXR5PSIwLjk0IiAvPjx0ZXh0IHg9IjE4OS40IiB5PSI5MC40IiB0ZXh0LWFuY2hvcj0ibWlkZGxlIiBmb250LXNpemU9IjEyIiBmb250LWZhbWlseT0iaW5oZXJpdCIgZm9udC13ZWlnaHQ9IjcwMCIgZmlsbD0iI2Y4ZmFmYyI+Njg8L3RleHQ+PHRleHQgeD0iMTg5LjQiIHk9IjI0MiIgdGV4dC1hbmNob3I9Im1pZGRsZSIgZm9udC1zaXplPSIxMiIgZm9udC1mYW1pbHk9ImluaGVyaXQiIGZpbGw9IiNjYmQ1ZTEiPmJldGE8L3RleHQ+PC9nPjxnPjxyZWN0IHg9IjI0NS4yIiB5PSIxMjguOCIgd2lkdGg9Ijg3LjYiIGhlaWdodD0iODUuMiIgcng9IjEwIiBmaWxsPSJ1cmwoI2JhcnMpIiBvcGFjaXR5PSIwLjk0IiAvPjx0ZXh0IHg9IjI4OSIgeT0iMTE4LjgiIHRleHQtYW5jaG9yPSJtaWRkbGUiIGZvbnQtc2l6ZT0iMTIiIGZvbnQtZmFtaWx5PSJpbmhlcml0IiBmb250LXdlaWdodD0iNzAwIiBmaWxsPSIjZjhmYWZjIj41MTwvdGV4dD48dGV4dCB4PSIyODkiIHk9IjI0MiIgdGV4dC1hbmNob3I9Im1pZGRsZSIgZm9udC1zaXplPSIxMiIgZm9udC1mYW1pbHk9ImluaGVyaXQiIGZpbGw9IiNjYmQ1ZTEiPmdhbW1hPC90ZXh0PjwvZz48Zz48cmVjdCB4PSIzNDQuOCIgeT0iNjIiIHdpZHRoPSI4Ny42IiBoZWlnaHQ9IjE1MiIgcng9IjEwIiBmaWxsPSJ1cmwoI2JhcnMpIiBvcGFjaXR5PSIwLjk0IiAvPjx0ZXh0IHg9IjM4OC42IiB5PSI1MiIgdGV4dC1hbmNob3I9Im1pZGRsZSIgZm9udC1zaXplPSIxMiIgZm9udC1mYW1pbHk9ImluaGVyaXQiIGZvbnQtd2VpZ2h0PSI3MDAiIGZpbGw9IiNmOGZhZmMiPjkxPC90ZXh0Pjx0ZXh0IHg9IjM4OC42IiB5PSIyNDIiIHRleHQtYW5jaG9yPSJtaWRkbGUiIGZvbnQtc2l6ZT0iMTIiIGZvbnQtZmFtaWx5PSJpbmhlcml0IiBmaWxsPSIjY2JkNWUxIj5kZWx0YTwvdGV4dD48L2c+PGc+PHJlY3QgeD0iNDQ0LjQiIHk9IjkwLjQiIHdpZHRoPSI4Ny42IiBoZWlnaHQ9IjEyMy42IiByeD0iMTAiIGZpbGw9InVybCgjYmFycykiIG9wYWNpdHk9IjAuOTQiIC8+PHRleHQgeD0iNDg4LjIiIHk9IjgwLjQiIHRleHQtYW5jaG9yPSJtaWRkbGUiIGZvbnQtc2l6ZT0iMTIiIGZvbnQtZmFtaWx5PSJpbmhlcml0IiBmb250LXdlaWdodD0iNzAwIiBmaWxsPSIjZjhmYWZjIj43NDwvdGV4dD48dGV4dCB4PSI0ODguMiIgeT0iMjQyIiB0ZXh0LWFuY2hvcj0ibWlkZGxlIiBmb250LXNpemU9IjEyIiBmb250LWZhbWlseT0iaW5oZXJpdCIgZmlsbD0iI2NiZDVlMSI+b21lZ2E8L3RleHQ+PC9nPjwvc3ZnPg==)

[`lapply()`](https://rdrr.io/r/base/lapply.html) returns a plain list of
`<g>` nodes. because that list is passed inside `Svg()`, `hypertext`
flattens it into sibling children of the svg.

## same component, real data

now reuse the same chart on a base R dataset. `VADeaths` is a matrix of
mortality rates per 1000. we’ll take the `"Rural Male"` column.

``` r
rural_male <- VADeaths[, "Rural Male"]

chart_b <- BarChart(
  values = rural_male,
  width = 620,
  height = 280,
  title = "VADeaths: rural male"
)

cat(
  render(chart_b)
)
```

![](data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHZpZXdib3g9IjAgMCA2MjAgMjgwIiB3aWR0aD0iNjIwIiBoZWlnaHQ9IjI4MCIgcm9sZT0iaW1nIj48ZGVmcz48bGluZWFyZ3JhZGllbnQgaWQ9ImJhcnMiIHgxPSIwJSIgeTE9IjAlIiB4Mj0iMTAwJSIgeTI9IjEwMCUiPjxzdG9wIG9mZnNldD0iMCUiIHN0b3AtY29sb3I9IiMyMmQzZWUiPjwvc3RvcD48c3RvcCBvZmZzZXQ9IjEwMCUiIHN0b3AtY29sb3I9IiNhNzhiZmEiPjwvc3RvcD48L2xpbmVhcmdyYWRpZW50PjwvZGVmcz48cmVjdCB4PSIwIiB5PSIwIiB3aWR0aD0iNjIwIiBoZWlnaHQ9IjI4MCIgcng9IjI4IiBmaWxsPSIjMGYxNzJhIiAvPjx0ZXh0IHg9IjI4IiB5PSIzNiIgZmlsbD0iI2Y4ZmFmYyIgZm9udC1zaXplPSIyMCIgZm9udC1mYW1pbHk9ImluaGVyaXQiIGZvbnQtd2VpZ2h0PSI3MDAiPlZBRGVhdGhzOgpydXJhbAptYWxlPC90ZXh0PjxsaW5lIHgxPSI0NiIgeTE9IjIzNCIgeDI9IjU5MiIgeTI9IjIzNCIgc3Ryb2tlPSIjMzM0MTU1IiBzdHJva2Utd2lkdGg9IjIiPjwvbGluZT48Zz48cmVjdCB4PSI0NiIgeT0iMjAzLjUiIHdpZHRoPSI5OS42IiBoZWlnaHQ9IjMwLjUiIHJ4PSIxMCIgZmlsbD0idXJsKCNiYXJzKSIgb3BhY2l0eT0iMC45NCIgLz48dGV4dCB4PSI5NS44IiB5PSIxOTMuNSIgdGV4dC1hbmNob3I9Im1pZGRsZSIgZm9udC1zaXplPSIxMiIgZm9udC1mYW1pbHk9ImluaGVyaXQiIGZvbnQtd2VpZ2h0PSI3MDAiIGZpbGw9IiNmOGZhZmMiPjExLjc8L3RleHQ+PHRleHQgeD0iOTUuOCIgeT0iMjYyIiB0ZXh0LWFuY2hvcj0ibWlkZGxlIiBmb250LXNpemU9IjEyIiBmb250LWZhbWlseT0iaW5oZXJpdCIgZmlsbD0iI2NiZDVlMSI+NTAtNTQ8L3RleHQ+PC9nPjxnPjxyZWN0IHg9IjE1Ny42IiB5PSIxODYuOCIgd2lkdGg9Ijk5LjYiIGhlaWdodD0iNDcuMiIgcng9IjEwIiBmaWxsPSJ1cmwoI2JhcnMpIiBvcGFjaXR5PSIwLjk0IiAvPjx0ZXh0IHg9IjIwNy40IiB5PSIxNzYuOCIgdGV4dC1hbmNob3I9Im1pZGRsZSIgZm9udC1zaXplPSIxMiIgZm9udC1mYW1pbHk9ImluaGVyaXQiIGZvbnQtd2VpZ2h0PSI3MDAiIGZpbGw9IiNmOGZhZmMiPjE4LjE8L3RleHQ+PHRleHQgeD0iMjA3LjQiIHk9IjI2MiIgdGV4dC1hbmNob3I9Im1pZGRsZSIgZm9udC1zaXplPSIxMiIgZm9udC1mYW1pbHk9ImluaGVyaXQiIGZpbGw9IiNjYmQ1ZTEiPjU1LTU5PC90ZXh0PjwvZz48Zz48cmVjdCB4PSIyNjkuMiIgeT0iMTYzLjkiIHdpZHRoPSI5OS42IiBoZWlnaHQ9IjcwLjEiIHJ4PSIxMCIgZmlsbD0idXJsKCNiYXJzKSIgb3BhY2l0eT0iMC45NCIgLz48dGV4dCB4PSIzMTkiIHk9IjE1My45IiB0ZXh0LWFuY2hvcj0ibWlkZGxlIiBmb250LXNpemU9IjEyIiBmb250LWZhbWlseT0iaW5oZXJpdCIgZm9udC13ZWlnaHQ9IjcwMCIgZmlsbD0iI2Y4ZmFmYyI+MjYuOTwvdGV4dD48dGV4dCB4PSIzMTkiIHk9IjI2MiIgdGV4dC1hbmNob3I9Im1pZGRsZSIgZm9udC1zaXplPSIxMiIgZm9udC1mYW1pbHk9ImluaGVyaXQiIGZpbGw9IiNjYmQ1ZTEiPjYwLTY0PC90ZXh0PjwvZz48Zz48cmVjdCB4PSIzODAuOCIgeT0iMTI3LjIiIHdpZHRoPSI5OS42IiBoZWlnaHQ9IjEwNi44IiByeD0iMTAiIGZpbGw9InVybCgjYmFycykiIG9wYWNpdHk9IjAuOTQiIC8+PHRleHQgeD0iNDMwLjYiIHk9IjExNy4yIiB0ZXh0LWFuY2hvcj0ibWlkZGxlIiBmb250LXNpemU9IjEyIiBmb250LWZhbWlseT0iaW5oZXJpdCIgZm9udC13ZWlnaHQ9IjcwMCIgZmlsbD0iI2Y4ZmFmYyI+NDE8L3RleHQ+PHRleHQgeD0iNDMwLjYiIHk9IjI2MiIgdGV4dC1hbmNob3I9Im1pZGRsZSIgZm9udC1zaXplPSIxMiIgZm9udC1mYW1pbHk9ImluaGVyaXQiIGZpbGw9IiNjYmQ1ZTEiPjY1LTY5PC90ZXh0PjwvZz48Zz48cmVjdCB4PSI0OTIuNCIgeT0iNjIiIHdpZHRoPSI5OS42IiBoZWlnaHQ9IjE3MiIgcng9IjEwIiBmaWxsPSJ1cmwoI2JhcnMpIiBvcGFjaXR5PSIwLjk0IiAvPjx0ZXh0IHg9IjU0Mi4yIiB5PSI1MiIgdGV4dC1hbmNob3I9Im1pZGRsZSIgZm9udC1zaXplPSIxMiIgZm9udC1mYW1pbHk9ImluaGVyaXQiIGZvbnQtd2VpZ2h0PSI3MDAiIGZpbGw9IiNmOGZhZmMiPjY2PC90ZXh0Pjx0ZXh0IHg9IjU0Mi4yIiB5PSIyNjIiIHRleHQtYW5jaG9yPSJtaWRkbGUiIGZvbnQtc2l6ZT0iMTIiIGZvbnQtZmFtaWx5PSJpbmhlcml0IiBmaWxsPSIjY2JkNWUxIj43MC03NDwvdGV4dD48L2c+PC9zdmc+)

the chart did not need to know where the data came from. it only needed
a named numeric vector. that is the sweet spot for components: accept a
simple input, hide the repetitive markup, return a `hypertext.tag`.

## animation

svg can animate itself. no javascript is needed for simple effects.
animation tags are children of the element they animate.

``` r
#' An Animated Signal Dot
#'
#' @param x Number /// Required.
#'          Horizontal centre.
#'
#' @param y Number /// Required.
#'          Vertical centre.
#'
#' @param color String /// Optional.
#'              Signal colour.
#'
#' @return `hypertext.tag`
#'
#' @export
SignalDot <- function(
  x,
  y,
  color = "#22d3ee"
) {
  tag_list(
    tags$circle(
      cx = x,
      cy = y,
      r = "10",
      fill = color,
      opacity = "0.55",
      tags$animate(
        attributeName = "r",
        values = "10;34",
        dur = "1.4s",
        repeatCount = "indefinite"
      ),
      tags$animate(
        attributeName = "opacity",
        values = "0.55;0",
        dur = "1.4s",
        repeatCount = "indefinite"
      )
    ),
    tags$circle(
      cx = x,
      cy = y,
      r = "7",
      fill = color
    )
  )
}
```

``` r
live_signal <- Svg(
  width = 520,
  height = 180,
  tags$rect(
    x = "0",
    y = "0",
    width = "520",
    height = "180",
    rx = "28",
    fill = "#020617"
  ),
  tags$text(
    x = "42",
    y = "76",
    fill = "#f8fafc",
    `font-size` = "28",
    `font-family` = "inherit",
    `font-weight` = "700",
    "live signal"
  ),
  tags$text(
    x = "42",
    y = "108",
    fill = "#94a3b8",
    `font-size` = "15",
    `font-family` = "inherit",
    "animated with svg tags, rendered from r"
  ),
  SignalDot(x = 440, y = 90)
)

cat(
  render(live_signal)
)
```

![](data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHZpZXdib3g9IjAgMCA1MjAgMTgwIiB3aWR0aD0iNTIwIiBoZWlnaHQ9IjE4MCIgcm9sZT0iaW1nIj48cmVjdCB4PSIwIiB5PSIwIiB3aWR0aD0iNTIwIiBoZWlnaHQ9IjE4MCIgcng9IjI4IiBmaWxsPSIjMDIwNjE3IiAvPjx0ZXh0IHg9IjQyIiB5PSI3NiIgZmlsbD0iI2Y4ZmFmYyIgZm9udC1zaXplPSIyOCIgZm9udC1mYW1pbHk9ImluaGVyaXQiIGZvbnQtd2VpZ2h0PSI3MDAiPmxpdmUKc2lnbmFsPC90ZXh0Pjx0ZXh0IHg9IjQyIiB5PSIxMDgiIGZpbGw9IiM5NGEzYjgiIGZvbnQtc2l6ZT0iMTUiIGZvbnQtZmFtaWx5PSJpbmhlcml0Ij5hbmltYXRlZAp3aXRoIHN2ZyB0YWdzLCByZW5kZXJlZCBmcm9tCnI8L3RleHQ+PGNpcmNsZSBjeD0iNDQwIiBjeT0iOTAiIHI9IjEwIiBmaWxsPSIjMjJkM2VlIiBvcGFjaXR5PSIwLjU1Ij48YW5pbWF0ZSBhdHRyaWJ1dGVuYW1lPSJyIiB2YWx1ZXM9IjEwOzM0IiBkdXI9IjEuNHMiIHJlcGVhdGNvdW50PSJpbmRlZmluaXRlIj48L2FuaW1hdGU+PGFuaW1hdGUgYXR0cmlidXRlbmFtZT0ib3BhY2l0eSIgdmFsdWVzPSIwLjU1OzAiIGR1cj0iMS40cyIgcmVwZWF0Y291bnQ9ImluZGVmaW5pdGUiPjwvYW5pbWF0ZT48L2NpcmNsZT48Y2lyY2xlIGN4PSI0NDAiIGN5PSI5MCIgcj0iNyIgZmlsbD0iIzIyZDNlZSI+PC9jaXJjbGU+PC9zdmc+)

## final composition

let’s end by putting the pieces together. the next component returns a
small metric card: a gradient panel, a data-driven sparkline, and a live
status dot.

``` r
#' A Metric Card
#'
#' @param label String /// Required.
#'              Metric label.
#'
#' @param value String /// Required.
#'              Main value to display.
#'
#' @param points Numeric vector /// Required.
#'               Sparkline points.
#'
#' @return `hypertext.tag`
#'
#' @export
MetricCard <- function(label, value, points) {
  width <- 560
  height <- 220
  x_values <- seq(56, 504, length.out = length(points))
  point_span <- diff(range(points))
  y_values <- if (point_span == 0) {
    rep(115, length(points))
  } else {
    154 - (points - min(points)) / point_span * 78
  }

  path_d <- paste(
    paste0(
      c(
        "M",
        rep(x = "L", times = length(points) - 1)
      ),
      round(x_values, 1),
      " ",
      round(y_values, 1)
    ),
    collapse = " "
  )

  Svg(
    width = width,
    height = height,
    tags$defs(
      LinearGradient(
        id = "metric-bg",
        colors = c("#0f172a", "#312e81", "#701a75")
      ),
      LinearGradient(
        id = "metric-line",
        colors = c("#22d3ee", "#f0abfc")
      )
    ),
    tags$rect(
      x = "0",
      y = "0",
      width = width,
      height = height,
      rx = "30",
      fill = "url(#metric-bg)"
    ),
    tags$text(
      x = "36",
      y = "50",
      fill = "#cbd5e1",
      `font-size` = "14",
      `font-family` = "inherit",
      `letter-spacing` = "1.6",
      label
    ),
    tags$text(
      x = "36",
      y = "94",
      fill = "#ffffff",
      `font-size` = "42",
      `font-family` = "inherit",
      `font-weight` = "800",
      value
    ),
    tags$path(
      d = path_d,
      fill = "none",
      stroke = "url(#metric-line)",
      `stroke-width` = "8",
      `stroke-linecap` = "round",
      `stroke-linejoin` = "round"
    ),
    lapply(
      X = seq_along(points),
      FUN = function(i) {
        Orb(
          cx = round(x_values[[i]], 1),
          cy = round(y_values[[i]], 1),
          r = 4,
          fill = "#f8fafc",
          opacity = 0.95
        )
      }
    ),
    SignalDot(x = 500, y = 50, color = "#f0abfc")
  )
}
```

``` r
metric <- MetricCard(
  label = "RENDER PIPELINE",
  value = "98.7%",
  points = c(28, 35, 31, 48, 44, 62, 58, 79, 86)
)

cat(
  render(metric)
)
```

![](data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHZpZXdib3g9IjAgMCA1NjAgMjIwIiB3aWR0aD0iNTYwIiBoZWlnaHQ9IjIyMCIgcm9sZT0iaW1nIj48ZGVmcz48bGluZWFyZ3JhZGllbnQgaWQ9Im1ldHJpYy1iZyIgeDE9IjAlIiB5MT0iMCUiIHgyPSIxMDAlIiB5Mj0iMTAwJSI+PHN0b3Agb2Zmc2V0PSIwJSIgc3RvcC1jb2xvcj0iIzBmMTcyYSI+PC9zdG9wPjxzdG9wIG9mZnNldD0iNTAlIiBzdG9wLWNvbG9yPSIjMzEyZTgxIj48L3N0b3A+PHN0b3Agb2Zmc2V0PSIxMDAlIiBzdG9wLWNvbG9yPSIjNzAxYTc1Ij48L3N0b3A+PC9saW5lYXJncmFkaWVudD48bGluZWFyZ3JhZGllbnQgaWQ9Im1ldHJpYy1saW5lIiB4MT0iMCUiIHkxPSIwJSIgeDI9IjEwMCUiIHkyPSIxMDAlIj48c3RvcCBvZmZzZXQ9IjAlIiBzdG9wLWNvbG9yPSIjMjJkM2VlIj48L3N0b3A+PHN0b3Agb2Zmc2V0PSIxMDAlIiBzdG9wLWNvbG9yPSIjZjBhYmZjIj48L3N0b3A+PC9saW5lYXJncmFkaWVudD48L2RlZnM+PHJlY3QgeD0iMCIgeT0iMCIgd2lkdGg9IjU2MCIgaGVpZ2h0PSIyMjAiIHJ4PSIzMCIgZmlsbD0idXJsKCNtZXRyaWMtYmcpIiAvPjx0ZXh0IHg9IjM2IiB5PSI1MCIgZmlsbD0iI2NiZDVlMSIgZm9udC1zaXplPSIxNCIgZm9udC1mYW1pbHk9ImluaGVyaXQiIGxldHRlci1zcGFjaW5nPSIxLjYiPlJFTkRFUgpQSVBFTElORTwvdGV4dD48dGV4dCB4PSIzNiIgeT0iOTQiIGZpbGw9IiNmZmZmZmYiIGZvbnQtc2l6ZT0iNDIiIGZvbnQtZmFtaWx5PSJpbmhlcml0IiBmb250LXdlaWdodD0iODAwIj45OC43JTwvdGV4dD48cGF0aCBkPSJNNTYgMTU0IEwxMTIgMTQ0LjYgTDE2OCAxNTAgTDIyNCAxMjcuMSBMMjgwIDEzMi41IEwzMzYgMTA4LjMgTDM5MiAxMTMuNyBMNDQ4IDg1LjQgTDUwNCA3NiIgZmlsbD0ibm9uZSIgc3Ryb2tlPSJ1cmwoI21ldHJpYy1saW5lKSIgc3Ryb2tlLXdpZHRoPSI4IiBzdHJva2UtbGluZWNhcD0icm91bmQiIHN0cm9rZS1saW5lam9pbj0icm91bmQiIC8+PGNpcmNsZSBjeD0iNTYiIGN5PSIxNTQiIHI9IjQiIGZpbGw9IiNmOGZhZmMiIG9wYWNpdHk9IjAuOTUiPjwvY2lyY2xlPjxjaXJjbGUgY3g9IjExMiIgY3k9IjE0NC42IiByPSI0IiBmaWxsPSIjZjhmYWZjIiBvcGFjaXR5PSIwLjk1Ij48L2NpcmNsZT48Y2lyY2xlIGN4PSIxNjgiIGN5PSIxNTAiIHI9IjQiIGZpbGw9IiNmOGZhZmMiIG9wYWNpdHk9IjAuOTUiPjwvY2lyY2xlPjxjaXJjbGUgY3g9IjIyNCIgY3k9IjEyNy4xIiByPSI0IiBmaWxsPSIjZjhmYWZjIiBvcGFjaXR5PSIwLjk1Ij48L2NpcmNsZT48Y2lyY2xlIGN4PSIyODAiIGN5PSIxMzIuNSIgcj0iNCIgZmlsbD0iI2Y4ZmFmYyIgb3BhY2l0eT0iMC45NSI+PC9jaXJjbGU+PGNpcmNsZSBjeD0iMzM2IiBjeT0iMTA4LjMiIHI9IjQiIGZpbGw9IiNmOGZhZmMiIG9wYWNpdHk9IjAuOTUiPjwvY2lyY2xlPjxjaXJjbGUgY3g9IjM5MiIgY3k9IjExMy43IiByPSI0IiBmaWxsPSIjZjhmYWZjIiBvcGFjaXR5PSIwLjk1Ij48L2NpcmNsZT48Y2lyY2xlIGN4PSI0NDgiIGN5PSI4NS40IiByPSI0IiBmaWxsPSIjZjhmYWZjIiBvcGFjaXR5PSIwLjk1Ij48L2NpcmNsZT48Y2lyY2xlIGN4PSI1MDQiIGN5PSI3NiIgcj0iNCIgZmlsbD0iI2Y4ZmFmYyIgb3BhY2l0eT0iMC45NSI+PC9jaXJjbGU+PGNpcmNsZSBjeD0iNTAwIiBjeT0iNTAiIHI9IjEwIiBmaWxsPSIjZjBhYmZjIiBvcGFjaXR5PSIwLjU1Ij48YW5pbWF0ZSBhdHRyaWJ1dGVuYW1lPSJyIiB2YWx1ZXM9IjEwOzM0IiBkdXI9IjEuNHMiIHJlcGVhdGNvdW50PSJpbmRlZmluaXRlIj48L2FuaW1hdGU+PGFuaW1hdGUgYXR0cmlidXRlbmFtZT0ib3BhY2l0eSIgdmFsdWVzPSIwLjU1OzAiIGR1cj0iMS40cyIgcmVwZWF0Y291bnQ9ImluZGVmaW5pdGUiPjwvYW5pbWF0ZT48L2NpcmNsZT48Y2lyY2xlIGN4PSI1MDAiIGN5PSI1MCIgcj0iNyIgZmlsbD0iI2YwYWJmYyI+PC9jaXJjbGU+PC9zdmc+)

the important part is not that this is a charting library. it is not.
the important part is that `hypertext` gives you the raw building
blocks. when you need a graphic that is a little too custom for a
plotting package, you can build it from tags and still keep the code
composable.

## conclusion

svg with `hypertext` follows the same rules as html with `hypertext`:

- named arguments become attributes.
- unnamed arguments become children.
- components are functions that return `hypertext.tag` objects.
- `...` lets callers provide unknown children.
- [`lapply()`](https://rdrr.io/r/base/lapply.html) can turn data into
  repeated svg nodes.
- animation, gradients, and paths are just more tags.

if you know how to build components, you already know the shape of the
svg workflow. the only new thing is the coordinate system.
