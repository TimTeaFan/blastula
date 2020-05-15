#' Specify the components of an info card
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
info_card <- function(title = NULL,
                      value = NULL,
                      value_size = "38px",
                      icon = NULL,
                      icon_fill = "white",
                      icon_width = 40,
                      delta = NULL,
                      caption = NULL,
                      arrow = NULL,
                      arrow_fill = "auto",
                      color = "#000000",
                      background_color = "#FFFFFF",
                      link = NULL)  {

  maybe_link <- function(...) {
    if (is.null(link)) {
      tags$div(...)
    }
    else {
      tags$a(href = link,
             ...)
    }}


  if (!is.null(icon)) {

    if(requireNamespace("fontawesome", quietly = TRUE)) {

      temp <- tempfile(pattern = "icon", fileext = ".png")

      fontawesome::fa_png(name = icon, file = temp, fill = icon_fill, width = icon_width)

      uri <- get_image_uri(file = temp)

      img <- tags$img(class = "icon",
                      src = uri,
                      alt = paste0("icon_", icon),
                      width = icon_width,
                      align = "right")

      on.exit(file.remove(temp), add = TRUE)

    } else {
      stop("Please ensure that the `fontawesome` package is installed before using the `icon` argument.",
           call. = FALSE)
    }}


  if (!is.null(arrow)) {

    if (arrow_fill == "auto") {

      arrow_fill <- switch(as.character(sign(delta)),
                           `-1` = "#E30000",
                           `1` = "#00B916",
                           `0` = "#A7A7A7")
    }

    add_sign <- switch(arrow_fill,
                       "#E30000" = "-",
                       "#00B916" = "+",
                       NULL)

    tmparrw <- tempfile(pattern = "arrow", fileext = ".png")

    arrw <- fontawesome::fa_png(name = paste0("arrow-", arrow), file = tmparrw, fill = arrow_fill, width = 15)

    uri <- get_image_uri(file = tmparrw)

    arr_icon <- tags$img(class = "arrow_cap",
                         src = uri,
                         alt = paste0("arrow-", arrow),
                         width = 15,
                         align = "left",
                         vertical_align = "middle")
  }

  # 2 add caption icon (arrow-up, arrow-down, arrow-right)
  # 3 font size and padding
  # 4 add link (use maybe link)

      tags$table(class = "value-box",
                 bgcolor = background_color,
                 width = "100%",
                 style = htmltools::css(text_decoration = "none",
                   border_radius =  "3px",
                   box_shadow = "2px 2px 2px rgba(0, 0, 0, 0.2)",
                   cellspacing = "0",
                   cellpadding = "0",
                   color = color),

                 # empty row to use colspan in outlook
                 tags$tr(
                   tags$td(
                   if (!is.null(icon)) {
                     tags$td(width = "50px")}
                   )
                 ),
                 if (!is.null(title)) {
                   tags$tr(tags$th(width = "100%",
                                   colspan = "2",
                                   align = "left",
                                   title)
                           )
                 },
                 tags$tr(
                   tags$td(class = "value",
                           tags$p(class = "value",
                                  style = htmltools::css(font_size = value_size),
                                  value)),
                   if (!is.null(icon)) {
                   tags$td(width = "50px",
                           img)}
                 ),
                 if (!(is.null(arrow) && is.null(delta) && is.null(caption))) {
                   tags$tr(
                     tags$td(class = "caption",
                             width = "100%",
                             colspan = "2",
                             if (!is.null(arrow)) {
                                arr_icon},
                             if (!is.null(delta)) {
                               tags$b(paste0(add_sign, delta))},
                             if (!is.null(caption)) {
                               caption}
                                   ))
                 })


}


info_card2 <- function(title = NULL,
                      value = NULL,
                      value_size = "38px",
                      icon = NULL,
                      icon_fill = "white",
                      icon_width = 40,
                      icon_height = 50,
                      icon_position = "90% 50%",
                      delta = NULL,
                      caption = NULL,
                      arrow = NULL,
                      arrow_fill = "auto",
                      color = "#000000",
                      background_color = "#FFFFFF",
                      link = NULL)  {

  maybe_link <- function(...) {
    if (is.null(link)) {
      tags$div(...)
    }
    else {
      tags$a(href = link,
             ...)
    }}


  if (!is.null(icon)) {

    if(requireNamespace("fontawesome", quietly = TRUE)) {

      temp <- tempfile(pattern = "icon", fileext = ".png")

      fontawesome::fa_png(name = icon, file = temp, fill = icon_fill, height = icon_height)

      img <- paste0("url(",get_image_uri(file = temp),")")

      custom_css <- htmltools::css(background_image = img,
                                   background_repeat = "no-repeat",
                                   background_position = icon_position
      )

      on.exit(file.remove(temp), add = TRUE)

    } else {
      stop("Please ensure that the `fontawesome` package is installed before using the `icon` argument.",
           call. = FALSE)
    }} else {

      custom_css <- NULL
    }


  if (!is.null(arrow)) {

    if (arrow_fill == "auto") {

      arrow_fill <- switch(as.character(sign(delta)),
                           `-1` = "#E30000",
                           `1` = "#00B916",
                           `0` = "#A7A7A7")
    }

    add_sign <- switch(arrow_fill,
                       "#E30000" = "-",
                       "#00B916" = "+",
                       NULL)

    tmparrw <- tempfile(pattern = "arrow", fileext = ".png")

    arrw <- fontawesome::fa_png(name = paste0("arrow-", arrow), file = tmparrw, fill = arrow_fill, width = 15)

    uri <- get_image_uri(file = tmparrw)

    arr_icon <- tags$img(class = "arrow_cap",
                         src = uri,
                         alt = paste0("arrow-", arrow),
                         width = 15,
                         align = "left",
                         vertical_align = "middle")
  }

  # 2 add caption icon (arrow-up, arrow-down, arrow-right)
  # 3 font size and padding
  # 4 add link (use maybe link)

  tags$table(class = "value-box",
             bgcolor = background_color,
             width = "100%",
             style = paste0(htmltools::css(text_decoration = "none",
                                           border_radius =  "3px",
                                           box_shadow = "2px 2px 2px rgba(0, 0, 0, 0.2)",
                                           cellspacing = "0",
                                           cellpadding = "0",
                                           color = color),
                                           custom_css),

             if (!is.null(title)) {
               tags$tr(tags$th(width = "100%",
                               colspan = "2",
                               align = "left",
                               title)
               )
             },
             tags$tr(
               tags$td(class = "value",
                       tags$p(class = "value",
                              style = htmltools::css(font_size = value_size),
                              value))
             ),
             if (!(is.null(arrow) && is.null(delta) && is.null(caption))) {
               tags$tr(
                 tags$td(class = "caption",
                         width = "100%",
                         colspan = "2",
                         if (!is.null(arrow)) {
                           arr_icon},
                         if (!is.null(delta)) {
                           tags$b(paste0(add_sign, delta))},
                         if (!is.null(caption)) {
                           caption}
                 ))
             })


}


info_card_old <- function(title = NULL, value = NULL, caption = NULL, color = "black",
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


  if (!is.null(icon)) {

    if(requireNamespace("fontawesome", quietly = TRUE)) {

      temp <- tempfile(pattern = "icon", fileext = ".png")

      fontawesome::fa_png(name = icon, file = temp, fill = fill, height = height)

      img <- paste0("url(",get_image_uri(file = temp),")")

      custom_css <- htmltools::css(background_image = img,
                                   background_repeat = "no-repeat",
                                   background_position = background_position
      )

      on.exit(file.remove(temp), add = TRUE)

    } else {
      stop("Please ensure that the `fontawesome` package is installed before using the `icon` argument.",
           call. = FALSE)
    }} else {

      custom_css <- NULL
    }

  htmltools::tagList(
    maybe_link(class = "value-box",
               style = htmltools::css(text_decoration = "none",
                                      color = color,
                                      background_color = background_color,
                                      border_radius =  "3px",
                                      display = "block",
                                      box_shadow = "2px 2px 2px rgba(0, 0, 0, 0.2)"),
               htmltools::tags$div(class = "inner",
                                   style = custom_css,
                                   htmltools::tags$p(class = "value", value),
                                   htmltools::tags$p(class = "caption", caption)))
    )
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
block_cards <- function(...) {

  x <- list(...)

  if (any(unlist(lapply(x, function(.x) grepl('class="caption"', .x))))) {
    min_height <- 90
  } else { min_height <- 75 }

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
                                                            width = paste0(pct, "%"),
                                                            cards))
                                       })
                                       )
                 )
  )
}
