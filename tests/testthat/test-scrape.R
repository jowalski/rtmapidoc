context("Scrape RTM website")

test_that(
  "scrape website", {
    
    skip_on_cran()
    skip_on_travis()
    skip("figuring out bug in R CMD check")
    
    html_docs <- fetch_html_docs(sample = 4)
    
    expect_equal(length(html_docs), 4)
    expect_is(html_docs[[1]], "xml_document")
  })

test_that(
  "determine if documentation has changed", {

    skip_on_cran()
    skip_on_travis()
    skip("figuring out bug in R CMD check")
    
    test_html_docs <- fetch_html_docs(sample = 8)
    test_parsed_docs <- create_rtm_docs(test_html_docs)
    rtm_docs_file <- file.path(system.file("data", package="rtmapidoc"),
                               "rtm_docs.rda")
    load(rtm_docs_file)
    expect_equal(rtm_docs[names(test_parsed_docs)], test_parsed_docs)
  })
