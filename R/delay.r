##' Ensures that a certain amount of time has elapsed before
##' a function can be called.
##'
##' This is a function operator, so you specify the delay
##' and an existing function and it returns a new function
##' that is only callable once every \code{delay} seconds.
##' Suggested in an exercise in \href{http://adv-r.had.co.nz/}{"Advanced R"}
##' @param delay the delay in seconds
##' @param f the existing function
##' @return the new function, which behaves exactly as the
##'   existing function except it calls Sys.sleep when necessary
delay_by <- function(delay, f) {
  force(f)
  last_called <- NA
  function(...) {
    now <- Sys.time()
    if (is.na(last_called))
      elapsed <- 0
    else
      elapsed <- as.numeric(now - last_called, units = "secs")
    last_called <<- now
    if (delay > elapsed)
      Sys.sleep(delay - elapsed)
    f(...)
  }
}

##' An instance of the \code{rvest::html()} function that can be
##' run at most once every 1 second.
##'
##' @param ... same parameters as \code{rvest::html()}
##' @return same as \code{rvest::html()}
html_d <- delay_by(1, rvest::html)
