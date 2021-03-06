% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bls_template.R
\name{blastula_template}
\alias{blastula_template}
\title{Default template for \code{compose_email}}
\usage{
blastula_template(
  html_body,
  html_header,
  html_footer,
  title,
  content_width = "1000px",
  font_family = "Helvetica, sans-serif"
)
}
\arguments{
\item{html_body, html_header, html_footer}{htmltools tag objects (e.g.
\code{\link[htmltools:tags]{htmltools::tags()}} or \code{\link[htmltools:HTML]{htmltools::HTML()}}), or \code{NULL} to omit.}

\item{title}{Plain text title to be used for the \verb{<title>} element; may be
displayed in mobile phone notifications.}

\item{content_width}{The width that should be used for the content area. This
should NOT be specified CSS units like \code{"600px"}, but as either an integer
(for pixels, e.g. \code{600}) or a percent string like \code{"85\%"}.}

\item{font_family}{The CSS value to use for \code{font-family}.}
}
\value{
A string containing a complete HTML document.
}
\description{
A template function that is suitable for using as the \code{template} argument of
\code{\link[=compose_email]{compose_email()}}. Template functions should generally not be called
directly. When implementing your own template function, you must include
parameters for \code{html_body}, \code{html_header}, \code{html_footer}, and \code{title}; you
may also optionally add your own parameters, which callers to
\code{compose_email()} can provide through the \code{...} argument.
}
