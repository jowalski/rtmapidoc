library(rtmapidoc)
library(xml2)
library(devtools)
  
docs <- fetch_html_docs()

for (i in c(1, 4, 5, 8, 22, 42, 44, 46, 50))
  write_xml(docs[[i]], paste0("inst/extdata/test", i, ".html"))
}
    
rtm_docs <- create_rtm_docs(rtm_html_docs)
use_data(rtm_docs, overwrite = TRUE)

