##' Parse documentation html using predefined css selectors. 
##'
##' @param method_html html object returned by \code{rvest::html()}
##' @param css_selectors named vector of fields and css selectors
##' @param method_regex regex to parse the method field
##' @param arg_err_regex regex to distinguish between argument and
##'     error fields
##' @return the parsed list of values documenting the method
##' @export
parse_html_doc <- function(method_html, css_selectors =
                         c(method = "h1", fields = "h2",
                           des_auth_tim = "p", arg_err = "dt",    
                           arg_err_desc = "dd",
                           example = ".boxhighlight"),
                           method_regex = "rtm\\..*",
                           arg_err_regex = "[0-9]+:") {
  
  res <- lapply(css_selectors,
                function (field) rvest::html_text(
                  html_nodes(method_html, field)))
  
  res$method <- stringr::str_extract(res$method, method_regex)
  res$description <- res$des_auth_tim[1]
  res$authentication <- res$des_auth_tim[2]
  
  l <- length(res$des_auth_tim)
  if (l >= 3)
    res$timeline <- res$des_auth_tim[3]
  if (l == 4)
    res$example <- res$des_auth_tim[4]
  if (l == 5) {
    res$example <- res$des_auth_tim[4]
    res$revertable <- res$des_auth_tim[5]
  }
  
  ers <- grepl(arg_err_regex, res$arg_err)
  res$arguments <- res$arg_err[!ers]
  res$error_codes <- res$arg_err[ers]
  res$arguments_desc <- res$arg_err_desc[!ers]
  res$error_codes_desc <- res$arg_err_desc[ers]
  
  res[c("des_auth_tim", "arg_err", "arg_err_desc")] <- NULL
  class(res) <- "rtm_doc"
  res
}

##' Convert HTML to documentation
##'
##' @param html_pages a list of html documentation objects
##' @return a named list of rtm_doc objects
##' @export
create_rtm_docs <- function(html_pages) {
  docs <- lapply(html_pages, parse_html_doc)
  names(docs) <- vapply(docs, function (x) x[["method"]], character(1))
  docs
}

##' Print a description.
##'
##' @param x the RTM object
##' @param ... extra
##' @export
print.rtm_doc <- function(x, ...) {
  field_c <- function (field) {
    sprintf("%s:\n\n    %s\n", stringr::str_to_title(field), x[[field]])
  }
  
  multi_field_c <- function (field) {
    heading <- sprintf("%s:\n", stringr::str_to_title(field))
    elem_list <- lapply(seq_along(x[[field]]), function(i) {
                          c(sprintf("    %s", x[[field]][i]),
                            sprintf("        %s\n",
                                    x[[paste0(field, "_desc")]][i]))
                        })
    c(heading, unlist(elem_list))
  }
  
  cat("\n",
      field_c("method"), field_c("description"), field_c("authentication"),
      if ("timeline" %in% names(x))
        field_c("timeline"),
      multi_field_c("arguments"), field_c("example"),
      if ("revertable" %in% names(x))
        field_c("revertable"),
      multi_field_c("error_codes"), ..., sep = "\n")
}

