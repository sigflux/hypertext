# SVG with hypertext

``` r
library(hypertext)
```

SVG is XML, and XML is just nested elements with attributes. That maps
directly onto hypertext’s model: `tags$svg()`, `tags$circle()`,
`tags$path()`, and so on are all available in the `tags` list.

The result is inline SVG — part of the HTML document, not a separate
file, no base64 encoding, no `<img>` tag. Styles cascade into it, it
scales cleanly, and it can be animated.

## Basic shapes

Every shape is a tag. Named arguments become SVG attributes.

``` r
# circle
circle <- tags$svg(
  viewBox = "0 0 100 100",
  width = "100",
  height = "100",
  tags$circle(
    cx = "50",
    cy = "50",
    r = "40",
    fill = "#a78bfa",
    stroke = "#7c3aed",
    `stroke-width` = "2"
  )
)

# rect with rounded corners
rect <- tags$svg(
  viewBox = "0 0 100 60",
  width = "100",
  height = "60",
  tags$rect(
    x = "5",
    y = "5",
    width = "90",
    height = "50",
    rx = "6",
    fill = "#a78bfa",
    opacity = "0.8"
  )
)

# line
line <- tags$svg(
  viewBox = "0 0 100 100",
  width = "100",
  height = "100",
  tags$line(
    x1 = "10",
    y1 = "90",
    x2 = "90",
    y2 = "10",
    stroke = "#a78bfa",
    `stroke-width` = "3",
    `stroke-linecap` = "round"
  )
)

# quadratic bezier path
path <- tags$svg(
  viewBox = "0 0 100 100",
  width = "100",
  height = "100",
  tags$path(
    d = "M10 80 Q 50 10 90 80",
    stroke = "#a78bfa",
    `stroke-width` = "3",
    fill = "none",
    `stroke-linecap` = "round"
  )
)

cat(
  render(circle),
  render(rect),
  render(line),
  render(path)
)
```

![](data:image/svg+xml;base64,PHN2ZyB2aWV3Ym94PSIwIDAgMTAwIDEwMCIgd2lkdGg9IjEwMCIgaGVpZ2h0PSIxMDAiPjxjaXJjbGUgY3g9IjUwIiBjeT0iNTAiIHI9IjQwIiBmaWxsPSIjYTc4YmZhIiBzdHJva2U9IiM3YzNhZWQiIHN0cm9rZS13aWR0aD0iMiI+PC9jaXJjbGU+PC9zdmc+)

![](data:image/svg+xml;base64,PHN2ZyB2aWV3Ym94PSIwIDAgMTAwIDYwIiB3aWR0aD0iMTAwIiBoZWlnaHQ9IjYwIj48cmVjdCB4PSI1IiB5PSI1IiB3aWR0aD0iOTAiIGhlaWdodD0iNTAiIHJ4PSI2IiBmaWxsPSIjYTc4YmZhIiBvcGFjaXR5PSIwLjgiIC8+PC9zdmc+)![](data:image/svg+xml;base64,PHN2ZyB2aWV3Ym94PSIwIDAgMTAwIDEwMCIgd2lkdGg9IjEwMCIgaGVpZ2h0PSIxMDAiPjxsaW5lIHgxPSIxMCIgeTE9IjkwIiB4Mj0iOTAiIHkyPSIxMCIgc3Ryb2tlPSIjYTc4YmZhIiBzdHJva2Utd2lkdGg9IjMiIHN0cm9rZS1saW5lY2FwPSJyb3VuZCI+PC9saW5lPjwvc3ZnPg==)![](data:image/svg+xml;base64,PHN2ZyB2aWV3Ym94PSIwIDAgMTAwIDEwMCIgd2lkdGg9IjEwMCIgaGVpZ2h0PSIxMDAiPjxwYXRoIGQ9Ik0xMCA4MCBRIDUwIDEwIDkwIDgwIiBzdHJva2U9IiNhNzhiZmEiIHN0cm9rZS13aWR0aD0iMyIgZmlsbD0ibm9uZSIgc3Ryb2tlLWxpbmVjYXA9InJvdW5kIiAvPjwvc3ZnPg==)

## A reusable icon

Wrapping an SVG in a function makes it a component just like any other
hypertext component. Here is a checkmark icon:

``` r
icon_check <- function(size = 24, color = "currentColor") {
  tags$svg(
    xmlns = "http://www.w3.org/2000/svg",
    viewBox = "0 0 24 24",
    width = size,
    height = size,
    fill = "none",
    stroke = color,
    `stroke-width` = "2",
    `stroke-linecap` = "round",
    tags$polyline(points = "20 6 9 17 4 12")
  )
}

a <- icon_check()
b <- icon_check(size = 48, color = "#a78bfa")
```

``` r
cat(
  render(a),
  render(b)
)
```

![](data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHZpZXdib3g9IjAgMCAyNCAyNCIgd2lkdGg9IjI0IiBoZWlnaHQ9IjI0IiBmaWxsPSJub25lIiBzdHJva2U9ImN1cnJlbnRDb2xvciIgc3Ryb2tlLXdpZHRoPSIyIiBzdHJva2UtbGluZWNhcD0icm91bmQiPjxwb2x5bGluZSBwb2ludHM9IjIwIDYgOSAxNyA0IDEyIj48L3BvbHlsaW5lPjwvc3ZnPg==)

![](data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHZpZXdib3g9IjAgMCAyNCAyNCIgd2lkdGg9IjQ4IiBoZWlnaHQ9IjQ4IiBmaWxsPSJub25lIiBzdHJva2U9IiNhNzhiZmEiIHN0cm9rZS13aWR0aD0iMiIgc3Ryb2tlLWxpbmVjYXA9InJvdW5kIj48cG9seWxpbmUgcG9pbnRzPSIyMCA2IDkgMTcgNCAxMiI+PC9wb2x5bGluZT48L3N2Zz4=)

## Gradients

Gradients are defined inside `<defs>` and referenced by `id`. The
`linearGradient` element takes `stop` children that define the colour
stops.

``` r
gradient_rect <- tags$svg(
  viewBox = "0 0 200 80",
  width = "200",
  height = "80",
  tags$defs(
    tags$linearGradient(
      id = "grad1",
      x1 = "0%",
      y1 = "0%",
      x2 = "100%",
      y2 = "0%",
      tags$stop(offset = "0%", style = "stop-color:#7c3aed; stop-opacity:1"),
      tags$stop(offset = "100%", style = "stop-color:#a78bfa; stop-opacity:1")
    )
  ),
  tags$rect(
    x = "0",
    y = "0",
    width = "200",
    height = "80",
    rx = "6",
    fill = "url(#grad1)"
  )
)

cat(render(gradient_rect))
```

![](data:image/svg+xml;base64,PHN2ZyB2aWV3Ym94PSIwIDAgMjAwIDgwIiB3aWR0aD0iMjAwIiBoZWlnaHQ9IjgwIj48ZGVmcz48bGluZWFyZ3JhZGllbnQgaWQ9ImdyYWQxIiB4MT0iMCUiIHkxPSIwJSIgeDI9IjEwMCUiIHkyPSIwJSI+PHN0b3Agb2Zmc2V0PSIwJSIgc3R5bGU9InN0b3AtY29sb3I6IzdjM2FlZDsgc3RvcC1vcGFjaXR5OjEiPjwvc3RvcD48c3RvcCBvZmZzZXQ9IjEwMCUiIHN0eWxlPSJzdG9wLWNvbG9yOiNhNzhiZmE7IHN0b3Atb3BhY2l0eToxIj48L3N0b3A+PC9saW5lYXJncmFkaWVudD48L2RlZnM+PHJlY3QgeD0iMCIgeT0iMCIgd2lkdGg9IjIwMCIgaGVpZ2h0PSI4MCIgcng9IjYiIGZpbGw9InVybCgjZ3JhZDEpIiAvPjwvc3ZnPg==)

## Bar chart — simple

The key insight: SVG coordinates are just numbers. We compute them in R
and pass them as attribute strings.

The SVG coordinate system starts at the top-left. For a bar chart, bars
grow downward from a baseline — so a bar of height `h` in a canvas of
height `H` starts at `y = H - h`.

``` r
bar_chart <- function(
  values,
  width = 300,
  height = 160,
  bar_color = "#a78bfa",
  label_color = "#888"
) {
  n <- length(values)
  pad <- 24 # padding inside canvas
  gap <- 8 # gap between bars
  usable_w <- width - 2 * pad
  bar_w <- (usable_w - gap * (n - 1)) / n
  max_val <- max(values)
  usable_h <- height - pad * 2

  bars <- lapply(seq_along(values), function(i) {
    bar_h <- (values[[i]] / max_val) * usable_h
    bar_x <- pad + (i - 1) * (bar_w + gap)
    bar_y <- pad + (usable_h - bar_h)
    label <- names(values)[[i]]

    tag_list(
      tags$rect(
        x = round(bar_x, 1),
        y = round(bar_y, 1),
        width = round(bar_w, 1),
        height = round(bar_h, 1),
        rx = "3",
        fill = bar_color,
        opacity = "0.85"
      ),
      tags$text(
        x = round(bar_x + bar_w / 2, 1),
        y = height - 6,
        `text-anchor` = "middle",
        `font-size` = "10",
        `font-family` = "inherit",
        fill = label_color,
        label
      )
    )
  })

  tags$svg(
    viewBox = paste("0 0", width, height),
    width = width,
    height = height,
    bars
  )
}

scores <- c(A = 40, B = 65, C = 30, D = 55, E = 80)
chart_a <- bar_chart(scores)

cat(render(chart_a))
```

![](data:image/svg+xml;base64,PHN2ZyB2aWV3Ym94PSIwIDAgMzAwIDE2MCIgd2lkdGg9IjMwMCIgaGVpZ2h0PSIxNjAiPjxyZWN0IHg9IjI0IiB5PSI4MCIgd2lkdGg9IjQ0IiBoZWlnaHQ9IjU2IiByeD0iMyIgZmlsbD0iI2E3OGJmYSIgb3BhY2l0eT0iMC44NSIgLz48dGV4dCB4PSI0NiIgeT0iMTU0IiB0ZXh0LWFuY2hvcj0ibWlkZGxlIiBmb250LXNpemU9IjEwIiBmb250LWZhbWlseT0iaW5oZXJpdCIgZmlsbD0iIzg4OCI+QTwvdGV4dD48cmVjdCB4PSI3NiIgeT0iNDUiIHdpZHRoPSI0NCIgaGVpZ2h0PSI5MSIgcng9IjMiIGZpbGw9IiNhNzhiZmEiIG9wYWNpdHk9IjAuODUiIC8+PHRleHQgeD0iOTgiIHk9IjE1NCIgdGV4dC1hbmNob3I9Im1pZGRsZSIgZm9udC1zaXplPSIxMCIgZm9udC1mYW1pbHk9ImluaGVyaXQiIGZpbGw9IiM4ODgiPkI8L3RleHQ+PHJlY3QgeD0iMTI4IiB5PSI5NCIgd2lkdGg9IjQ0IiBoZWlnaHQ9IjQyIiByeD0iMyIgZmlsbD0iI2E3OGJmYSIgb3BhY2l0eT0iMC44NSIgLz48dGV4dCB4PSIxNTAiIHk9IjE1NCIgdGV4dC1hbmNob3I9Im1pZGRsZSIgZm9udC1zaXplPSIxMCIgZm9udC1mYW1pbHk9ImluaGVyaXQiIGZpbGw9IiM4ODgiPkM8L3RleHQ+PHJlY3QgeD0iMTgwIiB5PSI1OSIgd2lkdGg9IjQ0IiBoZWlnaHQ9Ijc3IiByeD0iMyIgZmlsbD0iI2E3OGJmYSIgb3BhY2l0eT0iMC44NSIgLz48dGV4dCB4PSIyMDIiIHk9IjE1NCIgdGV4dC1hbmNob3I9Im1pZGRsZSIgZm9udC1zaXplPSIxMCIgZm9udC1mYW1pbHk9ImluaGVyaXQiIGZpbGw9IiM4ODgiPkQ8L3RleHQ+PHJlY3QgeD0iMjMyIiB5PSIyNCIgd2lkdGg9IjQ0IiBoZWlnaHQ9IjExMiIgcng9IjMiIGZpbGw9IiNhNzhiZmEiIG9wYWNpdHk9IjAuODUiIC8+PHRleHQgeD0iMjU0IiB5PSIxNTQiIHRleHQtYW5jaG9yPSJtaWRkbGUiIGZvbnQtc2l6ZT0iMTAiIGZvbnQtZmFtaWx5PSJpbmhlcml0IiBmaWxsPSIjODg4Ij5FPC90ZXh0Pjwvc3ZnPg==)

## Bar chart — real data

`VADeaths` is a matrix in base R with mortality rates per 1000 for five
age groups across four demographic groups. We take the `"Rural Male"`
column as our dataset.

``` r
rural_male <- VADeaths[, "Rural Male"]
# rural_male is a named numeric vector:
# 50-54  55-59  60-64  65-69  70-74
#  11.7   18.1   26.9   41.0   66.0

chart_b <- bar_chart(
  values = rural_male,
  width = 360,
  height = 180,
  bar_color = "#7c3aed"
)

cat(render(chart_b))
```

![](data:image/svg+xml;base64,PHN2ZyB2aWV3Ym94PSIwIDAgMzYwIDE4MCIgd2lkdGg9IjM2MCIgaGVpZ2h0PSIxODAiPjxyZWN0IHg9IjI0IiB5PSIxMzIuNiIgd2lkdGg9IjU2IiBoZWlnaHQ9IjIzLjQiIHJ4PSIzIiBmaWxsPSIjN2MzYWVkIiBvcGFjaXR5PSIwLjg1IiAvPjx0ZXh0IHg9IjUyIiB5PSIxNzQiIHRleHQtYW5jaG9yPSJtaWRkbGUiIGZvbnQtc2l6ZT0iMTAiIGZvbnQtZmFtaWx5PSJpbmhlcml0IiBmaWxsPSIjODg4Ij41MC01NDwvdGV4dD48cmVjdCB4PSI4OCIgeT0iMTE5LjgiIHdpZHRoPSI1NiIgaGVpZ2h0PSIzNi4yIiByeD0iMyIgZmlsbD0iIzdjM2FlZCIgb3BhY2l0eT0iMC44NSIgLz48dGV4dCB4PSIxMTYiIHk9IjE3NCIgdGV4dC1hbmNob3I9Im1pZGRsZSIgZm9udC1zaXplPSIxMCIgZm9udC1mYW1pbHk9ImluaGVyaXQiIGZpbGw9IiM4ODgiPjU1LTU5PC90ZXh0PjxyZWN0IHg9IjE1MiIgeT0iMTAyLjIiIHdpZHRoPSI1NiIgaGVpZ2h0PSI1My44IiByeD0iMyIgZmlsbD0iIzdjM2FlZCIgb3BhY2l0eT0iMC44NSIgLz48dGV4dCB4PSIxODAiIHk9IjE3NCIgdGV4dC1hbmNob3I9Im1pZGRsZSIgZm9udC1zaXplPSIxMCIgZm9udC1mYW1pbHk9ImluaGVyaXQiIGZpbGw9IiM4ODgiPjYwLTY0PC90ZXh0PjxyZWN0IHg9IjIxNiIgeT0iNzQiIHdpZHRoPSI1NiIgaGVpZ2h0PSI4MiIgcng9IjMiIGZpbGw9IiM3YzNhZWQiIG9wYWNpdHk9IjAuODUiIC8+PHRleHQgeD0iMjQ0IiB5PSIxNzQiIHRleHQtYW5jaG9yPSJtaWRkbGUiIGZvbnQtc2l6ZT0iMTAiIGZvbnQtZmFtaWx5PSJpbmhlcml0IiBmaWxsPSIjODg4Ij42NS02OTwvdGV4dD48cmVjdCB4PSIyODAiIHk9IjI0IiB3aWR0aD0iNTYiIGhlaWdodD0iMTMyIiByeD0iMyIgZmlsbD0iIzdjM2FlZCIgb3BhY2l0eT0iMC44NSIgLz48dGV4dCB4PSIzMDgiIHk9IjE3NCIgdGV4dC1hbmNob3I9Im1pZGRsZSIgZm9udC1zaXplPSIxMCIgZm9udC1mYW1pbHk9ImluaGVyaXQiIGZpbGw9IiM4ODgiPjcwLTc0PC90ZXh0Pjwvc3ZnPg==)

The same `bar_chart()` function works on both datasets. The chart is a
plain R object — a `hypertext.tag` — that you can embed anywhere
[`render()`](https://sigflux.github.io/hypertext/reference/render.md)
accepts: a full HTML page, an HTTP response, a
[`raw_html()`](https://sigflux.github.io/hypertext/reference/raw_html.md)
call inside a larger document.

## Animation

Animation elements are children of the element they animate. Add
`tags$animate()` as a child of `tags$circle()` to pulse a dot:

``` r
pulse <- tags$svg(
  viewBox = "0 0 100 100",
  width = "100",
  height = "100",
  tags$circle(
    cx = "50",
    cy = "50",
    r = "10",
    fill = "#a78bfa",
    tags$animate(
      attributeName = "r",
      from = "10",
      to = "28",
      dur = "1.2s",
      repeatCount = "indefinite",
      calcMode = "ease-out"
    ),
    tags$animate(
      attributeName = "opacity",
      from = "1",
      to = "0",
      dur = "1.2s",
      repeatCount = "indefinite",
      calcMode = "ease-out"
    )
  ),
  # static inner dot so the centre stays visible
  tags$circle(
    cx = "50",
    cy = "50",
    r = "6",
    fill = "#7c3aed"
  )
)

cat(render(pulse))
```

![](data:image/svg+xml;base64,PHN2ZyB2aWV3Ym94PSIwIDAgMTAwIDEwMCIgd2lkdGg9IjEwMCIgaGVpZ2h0PSIxMDAiPjxjaXJjbGUgY3g9IjUwIiBjeT0iNTAiIHI9IjEwIiBmaWxsPSIjYTc4YmZhIj48YW5pbWF0ZSBhdHRyaWJ1dGVuYW1lPSJyIiBmcm9tPSIxMCIgdG89IjI4IiBkdXI9IjEuMnMiIHJlcGVhdGNvdW50PSJpbmRlZmluaXRlIiBjYWxjbW9kZT0iZWFzZS1vdXQiPjwvYW5pbWF0ZT48YW5pbWF0ZSBhdHRyaWJ1dGVuYW1lPSJvcGFjaXR5IiBmcm9tPSIxIiB0bz0iMCIgZHVyPSIxLjJzIiByZXBlYXRjb3VudD0iaW5kZWZpbml0ZSIgY2FsY21vZGU9ImVhc2Utb3V0Ij48L2FuaW1hdGU+PC9jaXJjbGU+PGNpcmNsZSBjeD0iNTAiIGN5PSI1MCIgcj0iNiIgZmlsbD0iIzdjM2FlZCI+PC9jaXJjbGU+PC9zdmc+)

## Summary

| Element type | Tags |
|----|----|
| Basic shapes | `circle`, `ellipse`, `rect`, `line`, `path`, `polygon`, `polyline` |
| Text | `text`, `tspan`, `textPath` |
| Containers | `g`, `defs`, `symbol`, `use` |
| Gradients | `linearGradient`, `radialGradient`, `stop` |
| Filters | `filter`, `feGaussianBlur`, `feBlend`, and 20+ others |
| Animation | `animate`, `animateMotion`, `animateTransform`, `set` |

All 63 SVG elements from the [MDN
reference](https://developer.mozilla.org/en-US/docs/Web/SVG/Reference/Element)
that are currently part of the living standard are available in `tags`.
