#' Specify the components of an article
#'
#' The `info_card()` function is used exclusively within `block_cards()`,
#' and having one, two, or three calls will arrange the articles in a row (or as
#' a column of articles at lower screen widths).
#'
#' @param title An optional title for the article.
#' @param value ...
#' @param caption ...
#' @param color ...
#' @param background_color ...
#' @param icon An optional icon ...
#' @param fill ...
#' @param height ...
#' @param background_position ...
#' @param link ...
#'
#' @examples
#' # We can define an info card with an icon
#' # title text, some content,
#' # and a link to relevant content
#' card <-
#'      info_card(value = 45, icon = "phone", fill = "white",
#'                caption = "Articles per Day", color = "white",
#'                background_color = "rgba(39, 128, 227, 0.7)",
#'                link = "http://www.google.de"),
#'
#' if (interactive()) article
#' @export
info_card <- function(title = NULL, value = NULL, caption = NULL, color = "black",
                      background_color = "white", icon = NULL, fill = "white",
                      height = 60, background_position = "90% 50%", link = NULL)  {

  maybe_link <- function(...) {
    if (is.null(link)) {
      tags$div(...)
    }
    else {
      tags$a(href = link,
             ...)
    }}


  if (!is.null(icon) && requireNamespace("fontawesome", quietly = TRUE)) {

    temp <- tempfile(pattern = "", fileext = ".png")

    fontawesome::fa_png(name = icon, file = temp, fill = fill, height = height)

    img <- paste0("url(",blastula:::get_image_uri(file = temp),")")

    custom_css <- htmltools::css(background_image = img,
                                 background_repeat = "no-repeat",
                                 background_position = background_position
    )
  } else {
    custom_css <- NULL
  }

  htmltools::tagList(
    maybe_link(class = "value-box",
               style = htmltools::css(text_decoration = "none",
                                      color = color,
                                      background_color = background_color,
                                      border_radius =  "3px",
                                      # position = "relative",
                                      display = "block",
                                      box_shadow = "2px 2px 2px rgba(0, 0, 0, 0.2)"),
               htmltools::tags$div(class = "inner",
                                   style = custom_css,
                                   htmltools::tags$p(class = "value", value),
                                   htmltools::tags$p(class = "caption", caption))))

}

# To allow info cards to be snapshot tested using testthat::verify_output
print.info_card <- function(x, ...) {
  print(x(NULL))
}

#' A block of one, two, or three cards with a multicolumn layout
#'
#' With `block_cards()`, we can create a single- or multi-column layout of
#' info and plot cards. The cards are responsive to the screen width, so side-by-side
#' articles will collapse and any of the optional images will resize
#' accordingly. The function can accept one to three `*_card()` calls, each
#' with varying amounts of text and imagery. Like all `block_*()` functions,
#' `block_cards()` must be placed inside of `blocks()` and the resultant
#' `blocks` object can be provided to the `body`, `header`, or `footer`
#' arguments of `compose_email()`.
#'
#' @param ... One, two, or three calls to `*_card()`.
#'
#' @examples
#' # Create a block of three, side-by-side
#' # articles with three `article()`
#' # calls inside of `block_articles()`,
#' # itself placed in `blocks()`
#' email <-
#'   compose_email(
#'  body =
#'  blocks(
#'    block_title("Report"),
#'    block_text(md("Below you will find this weeks KPIs")),
#'    block_cards(
#'      info_card(value = 45, icon = "phone", fill = "white",
#'                caption = "Articles per Day", color = "white",
#'                background_color = "rgba(39, 128, 227, 0.7)", link = "http://www.google.de"),
#'      info_card(value = "2.302", caption = "Emails",
#'                color = "white", icon = "envelope", fill = "white",
#'                background_color = "rgba(39, 128, 227, 0.7)"),
#'      info_card(value = 32, caption = "Flights", color = "white", background_color = "rgba(255, 117, 24, 0.7)")
#'    )
#'      )
#'      )
#'     )
#'
#' if (interactive()) email
#'
#' @export
block_cards <- function (...) {

  x <- list(...)

  pct <- round(100/length(x))

  htmltools::div(class = "message-block block_cards",
     htmltools::tags$table(class = "cards",
                           cellspacing = "0",
                           cellpadding = "0",
                           width = "100%",
                           htmltools::tags$tr(lapply(seq_along(x), function(i) {
                           cards <- x[[i]]
                           htmltools::tagList(if (i != 1) {
                           htmltools::tags$td(width = 1,
                           htmltools::tags$img(src = blastula:::get_image_uri(system.file(package = "blastula","img/blank.png")),
                                               width = 12,
                                               height = 12,
                                               style = htmltools::css(width = "12px !important",
                                               height = "12px",
                                               max_width = "12px !important",
                                               min_width = "12px !important")))
                                         },
                                         htmltools::tags$td(class = "cards",
                                                            width = paste0(pct, "%"), cards))
                                       })
                                       )
                 )
  )
}
