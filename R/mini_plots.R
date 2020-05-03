# mini plots to be used with plot_card

donut <- function(value, color = "white", thick = "s", label = "percent", label_color = NULL, label_size = 12, label_round = 0) {

  if (is.character(thick)) {
    thick <- switch(thick,
           xs = -2,
           s  = 0,
           m  = 1,
           l  = 2,
           NULL)
  }

  if(is.null(thick) || thick > 2) {
    stop("`thick` takes either a character vector of length == 1 specifying the size ('xs', 's', 'm', 'l') or a numeric value under 2")
  }

  if (is.null(label_color)) {
    label_color <- color
  }

  # Data
  if (length(value) == 1) {

    if (value > 1 || value < 0)
      stop("`value` has to be either a number between 0 and 1 (in terms of percent) or a vector of two numbers (in terms of x out of y)")


    data <- data.frame(
      category = c("Done","Left"),
      count = c(value, 1 - value)
    )

  } else if (length(value) == 2) {

    left <- value[[2]] - value[[1]]

    data <- data.frame(
      category = c("Done","Left"),
      count = c(value[[1]], left)
    )

  } else {
    stop("`value` has to be either a numeric vector with one element between 0 and 1 or a numeric vector of two elements in terms of x out of y")
  }

  data$fraction = data$count / sum(data$count)

  data$ymax = cumsum(data$fraction)

  data$ymin = c(0, head(data$ymax, n = -1))

  # label
  if (label == "percent") {

    label <- paste0(round(data$fraction[[1]] * 100, label_round), "%")

  } else if (label == "absolute") {
    if (length(value) == 1) {
      stop("Setting `label` to `absolute` is only possible when `value` is a numeric vector of two elements in terms of x out of y")
    }

    label <- paste0(round(data$count[[1]], label_round))

  } else {
    stop("`label` takes only `percent` or `absolute` as arguments")
    }

  # Plot

  if (requireNamespace("ggplot2", quietly = TRUE)) {

    ggplot2::ggplot(data, ggplot2::aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = category)) +
      ggplot2::geom_rect() +
      ggplot2::scale_fill_manual(values = c(color, "transparent"), guide = FALSE) +
      ggplot2::coord_polar(theta = "y") +
      ggplot2::xlim(c(thick, 4)) +
      ggplot2::annotate("text", x = thick, y = 0, label = label, size = label_size, colour = label_color) +
      ggplot2::theme_void() +
      ggplot2::theme(plot.margin = grid::unit(c(0,0,0,0), "mm"))

  } else {
    stop("Please ensure that the `ggplot2` package is installed before using `donut()`.",
         call. = FALSE)
  }

}

