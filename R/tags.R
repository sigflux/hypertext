#' A Variation of `base::list()`
#'
#' @param ... Objects to pass to the list /// Optional.
#'
#' @param .envir An environment /// Optional.
#'               Specifies the environment in which to evaluate
#'               `...` in.
#'
#' @return List with `...` as elements.
#'
#' @keywords internal
#'
#' @noRd
.list2 <- function(...) {
  ellipsis_length <- ...length()
  ellipsis_names <- ...names()

  if (identical(ellipsis_length, 0L)) {
    return(list())
  }

  ellipsis_parse_tree <- substitute(
    expr = list(...)
  )

  has_trailing_comma <- identical(
    ellipsis_parse_tree[[length(ellipsis_parse_tree)]],
    quote(expr = )
  )

  if (has_trailing_comma) {
    ellipsis_length <- ellipsis_length - 1L
    ellipsis_names <- ellipsis_names[seq_len(ellipsis_length)]
  }

  if (identical(ellipsis_length, 0L)) {
    return(list())
  }

  out <- vector(mode = "list", length = ellipsis_length)

  for (idx in seq_len(ellipsis_length)) {
    value <- ...elt(idx)

    if (is.null(value)) {
      out[idx] <- list(NULL)
      next
    }

    out[[idx]] <- value
  }

  names(out) <- ellipsis_names

  out
}

# -- void (self-closing) elements ------------------------------------
.void_elements <- c(
  "area",
  "base",
  "br",
  "col",
  "embed",
  "hr",
  "img",
  "input",
  "link",
  "meta",
  "param",
  "source",
  "track",
  "wbr"
)

# -- html escaping ---------------------------------------------------

#' Escape special HTML characters
#'
#' Replaces `&`, `<`, `>`, `"`, and `'` with their HTML entity equivalents.
#'
#' @param x String /// Required.
#'
#' @return String with HTML special characters escaped.
#'
#' @keywords internal
#'
#' @noRd
.escape_html <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub("\"", "&quot;", x, fixed = TRUE)
  x <- gsub("'", "&#39;", x, fixed = TRUE)
  x
}

# -- attribute rendering ---------------------------------------------

#' Render a named list of attributes to a single HTML attribute string
#'
#' - `NA` values produce boolean / valueless attributes (e.g. `disabled`).
#' - `NULL` values are silently dropped.
#' - `TRUE` produces a boolean attribute; `FALSE` drops it.
#' - Character vectors of length > 1 are collapsed with a space
#'   (handy for `class = c("a", "b")`).
#'
#' @param attrs Named list /// Required.
#'              Attribute values.
#'
#' @return String (leading space included when non-empty).
#'
#' @keywords internal
#'
#' @noRd
.render_attrs <- function(attrs) {
  if (length(attrs) == 0L) {
    return("")
  }

  parts <- character(0L)

  for (nm in names(attrs)) {
    val <- attrs[[nm]]

    # NULL -> skip
    if (is.null(val)) {
      next
    }

    # FALSE -> skip; TRUE -> boolean attribute
    if (isFALSE(val)) {
      next
    }

    if (isTRUE(val)) {
      parts <- c(parts, nm)
      next
    }

    # NA -> boolean / valueless attribute
    if (length(val) == 1L && is.na(val)) {
      parts <- c(parts, nm)
      next
    }

    # character / numeric vectors -> collapse with space, escape
    val <- paste(as.character(val), collapse = " ")
    val <- .escape_html(val)
    parts <- c(parts, paste0(nm, "=\"", val, "\""))
  }

  if (length(parts) == 0L) {
    return("")
  }

  parts <- paste(parts, collapse = " ")

  paste0(" ", parts)
}

# -- node constructors -----------------------------------------------

#' Create an HTML element node
#'
#' Low-level constructor. Named arguments become attributes; unnamed
#' arguments become children (text or nested nodes).
#'
#' @param tag_name String /// Required.
#'                 The HTML element name.
#'
#' @param ... Attributes (named) and children (unnamed) /// Optional.
#'
#' @param tag_type String /// Optional.
#'                 Either `"normal"` (default) for a standard
#'                 element with children, or `"void"` for a self-closing element
#'                 whose children are ignored.
#'
#' @return List of class `"hypertext.tag"` with components `tag`, `attrs`,
#'        `children`, and `tag_type`.
#'
#' @examples
#' # web component
#' tag(tag_name = "calcite-action-bar", layout = "horizontal")
#'
#' # custom element with children
#' tag(
#'   tag_name = "my-card",
#'   class = "shadow",
#'   tag(tag_name = "my-card-header", "Title"),
#'   tag(tag_name = "my-card-body", "Content")
#' )
#'
#' # custom void element
#' tag(tag_name = "my-icon", name = "home", tag_type = "void")
#' @export
tag <- function(
  tag_name,
  ...,
  tag_type = c("normal", "void")
) {
  tag_type <- match.arg(arg = tag_type)
  dots <- list(...)

  nms <- names(dots)
  if (is.null(nms)) {
    nms <- rep(x = "", times = length(dots))
  }

  attrs_lgl <- nzchar(nms)
  attrs <- dots[attrs_lgl]

  children <- list()
  if (identical(tag_type, "normal")) {
    # flatten any bare lists among children so users can splice
    children <- .flatten_children(
      dots[!attrs_lgl]
    )
  }

  structure(
    list(
      tag = tag_name,
      attrs = attrs,
      children = children,
      tag_type = tag_type
    ),
    class = "hypertext.tag"
  )
}

#' Create a tag list (sibling container)
#'
#' Useful when you want to collect sibling nodes into a single
#' object without wrapping them in a parent element.
#'
#' @param ... Tags, text, or other renderable objects /// Optional.
#'
#' @return A list of class `"hypertext.tag.list"`.
#'
#' @examples
#' tl <- tag_list(tags$p("one"), tags$p("two"))
#' render(tl)
#' @export
tag_list <- function(...) {
  dots <- .list2(...)

  children <- Filter(
    f = Negate(is.null),
    x = dots
  )

  structure(
    children,
    class = c("hypertext.tag.list", "list")
  )
}

#' Mark a string as raw HTML
#'
#' Wraps one or more character strings so that [render()] outputs them
#' verbatim, **without** escaping HTML special characters.
#'
#' This is useful for injecting pre-rendered HTML, inline
#' `<script>`/`<style>` blocks, SVG markup, or any content that should
#' not be entity-escaped.
#'
#' @param ... Character strings of raw HTML /// Optional.
#'
#' @return A character vector of class `"hypertext.raw"`.
#'
#' @examples
#' raw_html("<script>alert('hi')</script>")
#' render(tags$div(raw_html("<em>already formatted</em>")))
#' @export
raw_html <- function(...) {
  structure(
    paste(..., collapse = ""),
    class = "hypertext.raw"
  )
}

#' Render the `<!DOCTYPE html>` declaration
#'
#' Convenience wrapper around [raw_html()] that returns the
#' HTML5 document-type declaration. Useful when building a full page.
#'
#' @return A `"hypertext.raw"` object containing `<!DOCTYPE html>`.
#'
#' @examples
#' page <- tag_list(
#'   doctype(),
#'   tags$html(
#'     tags$head(tags$title("Home")),
#'     tags$body(tags$h1("Welcome"))
#'   )
#' )
#' render(page)
#' @export
doctype <- function() {
  raw_html("<!DOCTYPE html>")
}

#' Flatten children
#'
#' Recursively unpack plain lists (but not `hypertext.tag` or
#' `hypertext.tag.list` nodes) so users can pass
#' `list(tags$li("a"), tags$li("b"))` as a single child argument.
#'
#' @param x List of children /// Required.
#'
#' @return A flat list of children.
#'
#' @keywords internal
#'
#' @noRd
.flatten_children <- function(x) {
  out <- list()
  for (el in x) {
    if (is.null(el)) {
      next
    }

    if (
      is.list(el) &&
        !inherits(el, "hypertext.tag") &&
        !inherits(el, "hypertext.tag.list") &&
        !inherits(el, "hypertext.raw")
    ) {
      out <- c(out, .flatten_children(el))
      next
    }

    out <- c(out, list(el))
  }

  out
}

# -- rendering -------------------------------------------------------

.maybe_write <- function(
  html,
  file = "",
  write_mode = c("overwrite", "append")
) {
  if (identical(file, "")) {
    return(html)
  }

  write_mode <- match.arg(arg = write_mode)

  cat(
    html,
    file = file,
    append = identical(write_mode, "append")
  )

  invisible(html)
}

#' Render an HTML node tree to a character string
#'
#' Converts a `hypertext.tag` object (and all its descendants) into an HTML
#' string. Text children are escaped; nested `hypertext.tag` children are
#' rendered recursively.
#'
#' When `file` is provided, the rendered HTML is written to the specified
#' file via [cat()] and the HTML string is returned
#' invisibly.
#'
#' @param x `hypertext.tag` object, a string, or a list of these /// Required.
#'
#' @param file String /// Optional.
#'            Path to file to print to.
#'
#' @param write_mode String /// Optional.
#'                   Either "overwrite" (default) which overwrites the contents
#'                   of `file`, or "append" which appends the HTML string to
#'                   `file`.
#'
#' @param ... Further arguments passed from or to other methods.
#'
#' @return A single character string of HTML.
#'
#' @examples
#' page <- tags$html(
#'   tags$head(
#'     tags$title("Home")
#'   ),
#'   tags$body(
#'     tags$h1("Welcome")
#'   )
#' )
#'
#' # return HTML as a string:
#' render(page)
#'
#' \dontrun{
#'   # write to a file:
#'   render(page, file = "index.html")
#' }
#'
#' @export
render <- function(
  x,
  ...
) {
  UseMethod("render")
}

#' @rdname render
#' @export
render.hypertext.tag <- function(
  x,
  file = "",
  write_mode = c("overwrite", "append"),
  ...
) {
  attr_str <- .render_attrs(x$attrs)
  is_void <- identical(x$tag_type, "void") || x$tag %in% .void_elements

  if (is_void) {
    html <- paste0("<", x$tag, attr_str, " />")

    return(
      .maybe_write(html = html, file = file, write_mode = write_mode)
    )
  }

  inner <- paste(
    vapply(
      X = x$children,
      FUN = render,
      FUN.VALUE = character(1L)
    ),
    collapse = ""
  )

  html <- paste0("<", x$tag, attr_str, ">", inner, "</", x$tag, ">")

  .maybe_write(html = html, file = file, write_mode = write_mode)
}

#' @rdname render
#' @export
render.default <- function(
  x,
  file = "",
  write_mode = c("overwrite", "append"),
  ...
) {
  html <- .escape_html(as.character(x))

  .maybe_write(html = html, file = file, write_mode = write_mode)
}

#' @rdname render
#' @export
render.hypertext.raw <- function(
  x,
  file = "",
  write_mode = c("overwrite", "append"),
  ...
) {
  html <- as.character(x)

  .maybe_write(html = html, file = file, write_mode = write_mode)
}

#' @rdname render
#' @export
render.list <- function(
  x,
  file = "",
  write_mode = c("overwrite", "append"),
  ...
) {
  html <- paste(
    vapply(
      X = x,
      FUN = render,
      FUN.VALUE = character(1L)
    ),
    collapse = ""
  )

  .maybe_write(html = html, file = file, write_mode = write_mode)
}

# -- print method ----------------------------------------------------

#' @export
print.hypertext.tag <- function(
  x,
  ...
) {
  cat(render(x), "\n")
  invisible(x)
}

#' @export
print.hypertext.tag.list <- print.hypertext.tag

#' @export
print.hypertext.raw <- print.hypertext.tag

# -- tag factory helpers ---------------------------------------------

#' Create a tag function
#'
#' @param tag_name String /// Required.
#'                 Name of the tag.
#'
#' @param tag_type String /// Optional.
#'                 Either "normal" (default) for a normal HTML tag
#'                 or "void" for a self-closing HTML tag.
#'
#' @return Function.
#'
#' @keywords internal
#'
#' @noRd
.make_tag <- function(
  tag_name,
  tag_type = c("normal", "void")
) {
  force(tag_name)
  tag_type <- match.arg(arg = tag_type)

  function(...) {
    dots <- .list2(...)

    dots$tag_name <- tag_name
    dots$tag_type <- tag_type

    do.call(what = tag, args = dots)
  }
}

# -- the `tags` list -------------------------------------------------

#' HTML tag functions
#'
#' A named list of functions, one per HTML5 element. Access individual
#' tags with `tags$div(...)`, `tags$p(...)`, etc.
#'
#' Named arguments become HTML attributes; unnamed arguments become
#' child nodes or text content.
#'
#' @section Boolean / valueless attributes:
#' Pass `NA` or `TRUE` as the attribute value to produce a valueless
#' attribute (e.g. `disabled`). `FALSE` or `NULL` suppresses the
#' attribute entirely.
#'
#' @section Multi-value attributes:
#' Character vectors of length > 1 are collapsed with a space, so
#' `class = c("a", "b")` renders as `class="a b"`.
#'
#' @examples
#' tags$p(class = "lead", "Hello, world!")
#' render(tags$div(id = "app", tags$h1("Title")))
#' @export
tags <- list(
  # -- document metadata & root ------------------------------------
  html = .make_tag("html"),
  head = .make_tag("head"),
  body = .make_tag("body"),
  title = .make_tag("title"),
  base = .make_tag("base", tag_type = "void"),
  link = .make_tag("link", tag_type = "void"),
  meta = .make_tag("meta", tag_type = "void"),
  style = .make_tag("style"),
  script = .make_tag("script"),
  noscript = .make_tag("noscript"),

  # -- sectioning & landmarks --------------------------------------
  header = .make_tag("header"),
  footer = .make_tag("footer"),
  nav = .make_tag("nav"),
  main = .make_tag("main"),
  section = .make_tag("section"),
  article = .make_tag("article"),
  aside = .make_tag("aside"),
  address = .make_tag("address"),
  hgroup = .make_tag("hgroup"),
  search = .make_tag("search"),

  # -- headings ----------------------------------------------------
  h1 = .make_tag("h1"),
  h2 = .make_tag("h2"),
  h3 = .make_tag("h3"),
  h4 = .make_tag("h4"),
  h5 = .make_tag("h5"),
  h6 = .make_tag("h6"),

  # -- block text content ------------------------------------------
  div = .make_tag("div"),
  p = .make_tag("p"),
  blockquote = .make_tag("blockquote"),
  pre = .make_tag("pre"),
  figure = .make_tag("figure"),
  figcaption = .make_tag("figcaption"),
  hr = .make_tag("hr", tag_type = "void"),

  # -- inline text semantics ---------------------------------------
  a = .make_tag("a"),
  abbr = .make_tag("abbr"),
  b = .make_tag("b"),
  bdi = .make_tag("bdi"),
  bdo = .make_tag("bdo"),
  br = .make_tag("br", tag_type = "void"),
  cite = .make_tag("cite"),
  code = .make_tag("code"),
  data = .make_tag("data"),
  dfn = .make_tag("dfn"),
  em = .make_tag("em"),
  i = .make_tag("i"),
  kbd = .make_tag("kbd"),
  mark = .make_tag("mark"),
  q = .make_tag("q"),
  rp = .make_tag("rp"),
  rt = .make_tag("rt"),
  ruby = .make_tag("ruby"),
  s = .make_tag("s"),
  samp = .make_tag("samp"),
  small = .make_tag("small"),
  span = .make_tag("span"),
  strong = .make_tag("strong"),
  sub = .make_tag("sub"),
  sup = .make_tag("sup"),
  time = .make_tag("time"),
  u = .make_tag("u"),
  var = .make_tag("var"),
  wbr = .make_tag("wbr", tag_type = "void"),
  del = .make_tag("del"),
  ins = .make_tag("ins"),

  # -- lists -------------------------------------------------------
  ul = .make_tag("ul"),
  ol = .make_tag("ol"),
  li = .make_tag("li"),
  dl = .make_tag("dl"),
  dt = .make_tag("dt"),
  dd = .make_tag("dd"),
  menu = .make_tag("menu"),

  # -- tables ------------------------------------------------------
  table = .make_tag("table"),
  caption = .make_tag("caption"),
  colgroup = .make_tag("colgroup"),
  col = .make_tag("col", tag_type = "void"),
  thead = .make_tag("thead"),
  tbody = .make_tag("tbody"),
  tfoot = .make_tag("tfoot"),
  tr = .make_tag("tr"),
  th = .make_tag("th"),
  td = .make_tag("td"),

  # -- forms -------------------------------------------------------
  form = .make_tag("form"),
  fieldset = .make_tag("fieldset"),
  legend = .make_tag("legend"),
  label = .make_tag("label"),
  input = .make_tag("input", tag_type = "void"),
  button = .make_tag("button"),
  select = .make_tag("select"),
  option = .make_tag("option"),
  optgroup = .make_tag("optgroup"),
  datalist = .make_tag("datalist"),
  textarea = .make_tag("textarea"),
  output = .make_tag("output"),
  progress = .make_tag("progress"),
  meter = .make_tag("meter"),

  # -- media & embedded content ------------------------------------
  img = .make_tag("img", tag_type = "void"),
  picture = .make_tag("picture"),
  source = .make_tag("source", tag_type = "void"),
  audio = .make_tag("audio"),
  video = .make_tag("video"),
  track = .make_tag("track", tag_type = "void"),
  map = .make_tag("map"),
  area = .make_tag("area", tag_type = "void"),
  iframe = .make_tag("iframe"),
  embed = .make_tag("embed", tag_type = "void"),
  object = .make_tag("object"),
  param = .make_tag("param", tag_type = "void"),
  canvas = .make_tag("canvas"),
  svg = .make_tag("svg"),
  math = .make_tag("math"),

  # -- interactive / scripting -------------------------------------
  details = .make_tag("details"),
  summary = .make_tag("summary"),
  dialog = .make_tag("dialog"),
  template = .make_tag("template"),
  slot = .make_tag("slot")
)
