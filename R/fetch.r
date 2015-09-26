##' Get a list of html documentation for all RTM API method calls from
##' the RTM website.
##'
##' @param rtm_url the base url of the RTM website
##' @param api_doc_path the path to the list of links to documentation on
##'     each method call
##' @param css the css selector for the links
##' @param progress whether to show a progress bar
##' @param sample select a random sample of this size, for testing,
##'     by default -1 means scrape all documentation
##' @return a list of html pages
##' @export
fetch_html_docs <- function(rtm_url = "https://www.rememberthemilk.com/",
                               api_doc_path = "services/api/methods/",
                            css = "#api a", progress = TRUE, sample = -1) {
  ## note: we get these warnings from use rvest::html(), which is
  ## deprecated, but the suggested alternative, rvest::read_html(),
  ## is not in fact exported in the current CRAN version, so
  ## we'll live with the warnings for now
  ## 
  ## Warning messages:
  ## 1: 'f' is deprecated.
  ## Use 'read_html' instead.
  ## See help("Deprecated")
  ## 2: 'f' is deprecated.
  ## ...
  doc_links <- html_d(paste0(rtm_url, api_doc_path)) %>%
    rvest::html_nodes(css) %>%
    lapply(rvest::html_attr, "href")
  links <- seq_along(doc_links)
  if (sample != -1)
    links <- sample.int(length(doc_links), sample)
  plyr::llply(doc_links[links], function (path) html_d(paste0(rtm_url, path)),
              .progress = if (progress) "text" else "none")
}
