#' \pkg{rtmapidoc} scrapes the Remember the Milk API documentation website
#' for the purposes of creating and maintaining the rtmr package
#'
#' @name rtmapidoc
#' @docType package
#' @import rvest
NULL


#' A list of documented API methods for the RTM REST API previously
#' scraped from the RTM website using functions in this package.
#'
#' @format A list of rtm_doc objects, each of which contains the
#'   following fields
#' \describe{
#'   \item{method}{the API method name}
#'   \item{description}{a description}
#'   \item{authentication}{whether the API method requires authentication}
#'   \item{arguments}{a list of its arguments}
#'   \item{arguments_desc}{a list of argument descriptions}
#'   \item{example}{an example response}
#'   \item{error_codes}{a list of possible error codes}
#'   \item{error_codes_desc}{a list of descriptions of the error codes}
#'   \item{timeline}{whether the API call requires a timeline (not all have this)}
#'   \item{revertible}{whether the API call is revertible}
#' }
#' @source \url{http://www.diamondse.info/}
"rtm_docs"
