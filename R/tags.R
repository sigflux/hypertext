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
#' @param x A character string.
#' @return A character string with HTML special characters escaped.
#' @keywords internal
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
#' @param attrs A named list of attribute values.
#' @return A single character string (leading space included when non-empty).
#' @keywords internal
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
#' arguments become children (text or nested nodes). This function is
#' used internally by every entry in the [tags] list.
#'
#' @param tag_name Character scalar -- the HTML element name.
#' @param ... Attributes (named) and children (unnamed).
#' @param .void Logical; if `TRUE` the element is self-closing and
#'   children are ignored.
#' @return A list of class `"hypertext.tag"` with components `tag`, `attrs`,
#'   and `children`.
#' @keywords internal
.tag <- function(tag_name, ..., .void = FALSE) {
  dots <- list(...)

  nms <- names(dots)
  if (is.null(nms)) {
    nms <- rep(x = "", times = length(dots))
  }

  attrs_lgl <- nzchar(nms)
  attrs <- dots[attrs_lgl]

  children <- list()
  if (isFALSE(.void)) {
    # flatten any bare lists among children so users can splice
    children <- .flatten_children(
      dots[!attrs_lgl]
    )
  }

  structure(
    list(
      tag = tag_name,
      attrs = attrs,
      children = children
    ),
    class = "hypertext.tag"
  )
}

#' Flatten children
#'
#' Recursively unpack plain lists (but not `hypertext.tag` nodes) so users
#' can pass `list(tags$li("a"), tags$li("b"))` as a single child
#' argument.
#'
#' @param x A list of children.
#' @return A flat list of children.
#' @keywords internal
.flatten_children <- function(x) {
  out <- list()
  for (el in x) {
    if (is.list(el) && !inherits(el, "hypertext.tag")) {
      out <- c(out, .flatten_children(el))
      next
    }

    out <- c(out, list(el))
  }

  out
}

# -- rendering -------------------------------------------------------

#' Render an HTML node tree to a character string
#'
#' Converts an `hypertext.tag` object (and all its descendants) into an HTML
#' string. Text children are escaped; nested `hypertext.tag` children are
#' rendered recursively.
#'
#' @param x An `hypertext.tag` object, a character string, or a list of these.
#' @return A single character string of HTML.
#' @export
render <- function(x) {
  UseMethod("render")
}

#' @rdname render
#' @export
render.hypertext.tag <- function(x) {
  attr_str <- .render_attrs(x$attrs)
  is_void <- x$tag %in% .void_elements

  if (is_void) {
    return(
      paste0("<", x$tag, attr_str, " />")
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

  paste0("<", x$tag, attr_str, ">", inner, "</", x$tag, ">")
}

#' @rdname render
#' @export
render.default <- function(x) {
  .escape_html(as.character(x))
}

#' @rdname render
#' @export
render.list <- function(x) {
  paste(
    vapply(
      X = x,
      FUN = render,
      FUN.VALUE = character(1L)
    ),
    collapse = ""
  )
}

# -- print method ----------------------------------------------------

#' @export
print.hypertext.tag <- function(x, ...) {
  cat(render(x), "\n")
  invisible(x)
}

# -- tag factory helpers ---------------------------------------------

#' Create a tag function for a normal (non-void) element
#' @keywords internal
.make_tag <- function(tag_name) {
  force(tag_name)

  function(...) {
    .tag(
      tag_name,
      ...
    )
  }
}

#' Create a tag function for a void (self-closing) element
#' @keywords internal
.make_void_tag <- function(tag_name) {
  force(tag_name)

  function(...) {
    .tag(
      tag_name,
      ...,
      .void = TRUE
    )
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
  base = .make_void_tag("base"),
  link = .make_void_tag("link"),
  meta = .make_void_tag("meta"),
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
  hr = .make_void_tag("hr"),

  # -- inline text semantics ---------------------------------------
  a = .make_tag("a"),
  abbr = .make_tag("abbr"),
  b = .make_tag("b"),
  bdi = .make_tag("bdi"),
  bdo = .make_tag("bdo"),
  br = .make_void_tag("br"),
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
  wbr = .make_void_tag("wbr"),
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
  col = .make_void_tag("col"),
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
  input = .make_void_tag("input"),
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
  img = .make_void_tag("img"),
  picture = .make_tag("picture"),
  source = .make_void_tag("source"),
  audio = .make_tag("audio"),
  video = .make_tag("video"),
  track = .make_void_tag("track"),
  map = .make_tag("map"),
  area = .make_void_tag("area"),
  iframe = .make_tag("iframe"),
  embed = .make_void_tag("embed"),
  object = .make_tag("object"),
  param = .make_void_tag("param"),
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
