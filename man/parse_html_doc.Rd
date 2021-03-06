% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/parse.r
\name{parse_html_doc}
\alias{parse_html_doc}
\title{Parse documentation html using predefined css selectors.}
\usage{
parse_html_doc(method_html, css_selectors = c(method = "h1", fields = "h2",
  des_auth_tim = "p", arg_err = "dt", arg_err_desc = "dd", example =
  ".boxhighlight"), method_regex = "rtm\\\\..*", arg_err_regex = "[0-9]+:")
}
\arguments{
\item{method_html}{html object returned by \code{rvest::html()}}

\item{css_selectors}{named vector of fields and css selectors}

\item{method_regex}{regex to parse the method field}

\item{arg_err_regex}{regex to distinguish between argument and
error fields}
}
\value{
the parsed list of values documenting the method
}
\description{
Parse documentation html using predefined css selectors.
}

