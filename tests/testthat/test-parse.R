context("Parse HTML")

pkg_file <- function(folder, file) {
  file.path(system.file(folder, package = "rtmapidoc"), file)
}

test_that(
  "parse raw html", {
    fnames <- paste0("test", c(1, 4, 5, 8, 22, 42, 44, 46, 50), ".html")
    test_docs <- lapply(fnames,
                        function (file) rvest::html(pkg_file("extdata", file)))
    parsed_docs <- create_rtm_docs(test_docs)
    is_parsed_doc_info_complete <- function(doc) {
      vapply(doc, function (x) length(x) != 0, logical(1))
    }
    
    expect_is(parsed_docs, "list")
    expect_true(all(lapply(parsed_docs, function (x) class(x) == "rtm_doc")))
    expect_true(all(unlist(lapply(parsed_docs, is_parsed_doc_info_complete))))
    expect_true(all(unlist(lapply(parsed_docs, length)) >= 9))
    expect_true(all(unlist(lapply(parsed_docs, length)) <= 11))
  })

test_that(
  "print rtm_doc object", {
    expect_output(print(rtm_docs[["rtm.auth.checkToken"]]),
                  "    Returns the credentials")
    expect_output(print(rtm_docs[["rtm.tasks.setURL"]]),
                  "    114: Invalid SOAP envelope")
  })
