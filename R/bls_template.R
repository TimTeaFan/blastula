#' Default template for `compose_email`
#'
#' A template function that is suitable for using as the `template` argument of
#' [compose_email()]. Template functions should generally not be called
#' directly. When implementing your own template function, you must include
#' parameters for `html_body`, `html_header`, `html_footer`, and `title`; you
#' may also optionally add your own parameters, which callers to
#' `compose_email()` can provide through the `...` argument.
#'
#' @param html_body,html_header,html_footer htmltools tag objects (e.g.
#'   [htmltools::tags()] or [htmltools::HTML()]), or `NULL` to omit.
#' @param title Plain text title to be used for the `<title>` element; may be
#'   displayed in mobile phone notifications.
#' @param content_width The width that should be used for the content area. This
#'   should NOT be specified CSS units like `"600px"`, but as either an integer
#'   (for pixels, e.g. `600`) or a percent string like `"85%"`.
#' @param font_family The CSS value to use for `font-family`.
#'
#' @return A string containing a complete HTML document.
#'
#' @export
blastula_template <- function(html_body, html_header, html_footer, title, content_width = "1000px",
                              font_family = "Helvetica, sans-serif", body_background_color = "#f6f6f6", border_radius = "0px",
                              content_background_color = "white", body_color = "#222", header_color = "#999999",
                              header_font_size = "12px", header_font_weight = "normal", header_margin = "0 0 24px 0",
                              header_text_align = "center", footer_color = "#999999", footer_font_size = "12px",
                              footer_font_weight = "normal", footer_margin = "24px 0 0 0", footer_text_align = "center") {
  result <- htmltools::renderTags(
    htmltools::tagList(
      htmltools::tags$head(
        htmltools::includeHTML(system.file(package = "blastula", "cerberus-meta.html")),
        htmltools::tags$title(title),
        htmltools::tags$style(htmltools::HTML(paste0(
          "body {
            font-family: ", font_family, ";
            font-size: 14px;
          }
          .content {
            background-color: white;
            border-radius:", border_radius,";
          }", "
          .content .message-block {
            margin-bottom: 24px;
          }
          .header .message-block, .footer message-block {
            margin-bottom: 12px;
          }
          img {
            max-width: 100%;
          }
          .value-box > .inner{
            padding: 10px;
            padding-top: 15px;
            padding-left: 20px;
            padding-right: 20px;
          }
          .value-box .value {
            font-size: 38px;
            font-weight: bold;
            margin: 0 0 3px 0;
            white-space: nowrap;
            padding: 0;
          }
          p {
            margin: 0 0 10.5px;
          }
          @media only screen and (max-width: 767px) {
            .container {
               width: 100%;
             }
            .articles, .articles tr, .articles td {
              display: block;
              width: 100%;
            }
            .cards, .cards tr, .cards td, .value-box, .inner {
              display: block !important;
              width: 100% !important;
            }
          .article {
            margin-bottom: 24px;
            }
          }
          ")))),
      htmltools::tags$body(style = htmltools::css(background_color = body_background_color,
                                                  font_family = font_family,
                                                  color = body_color,
                                                  margin = "0",
                                                  padding = "0"),
                           blastula:::panel(outer_class = "container",
                                            outer_align = "center",
                                            padding = "24px",
                                            width = "85%",
                                            max_width = htmltools::validateCssUnit(content_width),
                                            if (!is.null(html_header)) {
                                              htmltools::div(class = "header",
                                                             style = htmltools::css(font_family = font_family,
                                                                                    color = header_color,
                                                                                    font_size = header_font_size,
                                                                                    font_weight = header_font_weight,
                                                                                    margin = header_margin,
                                                                                    text_align = header_text_align),
                                                             html_header)
                                            }, blastula:::panel(outer_class = "content",
                                                                padding = "12px",
                                                                background_color = content_background_color,
                                                                html_body),
                                            if (!is.null(html_footer)) {
                                              htmltools::div(class = "footer",
                                                             style = htmltools::css(font_family = font_family,
                                                                                    color = footer_color,
                                                                                    font_size = footer_font_size,
                                                                                    font_weight = footer_font_weight,
                                                                                    margin = footer_margin,
                                                                                    text_align = footer_text_align),
                                                             html_footer)
                                            }
                           )
      )))
  htmltools::HTML(sprintf("<!doctype html>\n<html>\n  <head>\n%s\n  </head>\n%s\n</html>",
                          result$head, result$html))
}
