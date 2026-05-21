# set up vcr
library("vcr")
invisible(vcr::vcr_configure(
    dir = "../fixtures",
    filter_sensitive_data = list(
        "<crossref_email>" = Sys.getenv("crossref_email"),
        "<crossref_plus_token>" = Sys.getenv("crossref_plus_token")
    )
))

pj <- function(x) jsonlite::fromJSON(x)
