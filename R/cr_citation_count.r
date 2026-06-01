#' Get a citation count via CrossRef
#'
#' @export
#'
#' @param doi (character) One or more digital object identifiers.
#' @param url (character) the url for the OpenURL function (used when no Plus
#'   token is configured)
#' @param key your Crossref OpenURL email address. Used as the `pid` parameter
#'   for the legacy OpenURL API when `plus_token` is not set.
#' @param async (logical) use async HTTP requests for the legacy OpenURL path.
#'   Default: `FALSE`. Ignored when `plus_token` is set.
#' @param plus_token (character) Crossref Metadata Plus API token. When set,
#'   uses the REST API (`https://api.crossref.org/works/{doi}`) with the
#'   `Crossref-Plus-API-Token: Bearer <token>` header and sends requests in
#'   async batches. Defaults to the `crossref_plus_token` environment variable.
#' @param batch_size (integer) number of concurrent async requests per batch
#'   when using the Plus REST path. Defaults to the `crossref_batch_size`
#'   environment variable, or 50 if unset.
#' @param ... Curl options passed on to [crul::HttpClient()] or
#'   [crul::HttpRequest()]
#'
#' @return a data.frame with columns `doi` and `count`. `count` is numeric or
#'   `NA` when not found.
#'
#' @details
#' When `plus_token` is provided (or set via the `crossref_plus_token` env
#' var), the function uses the Crossref REST API with your Metadata Plus
#' subscription, which provides higher rate limits. Requests are sent in
#' async batches of `batch_size` concurrent connections.
#'
#' Without a Plus token the legacy OpenURL API is used
#' (`https://doi.crossref.org/openurl/`). Set `async = TRUE` to fire all
#' OpenURL requests concurrently (no batching).
#'
#' @seealso [cr_search()], [cr_r()]
#' @author Carl Boettiger \email{cboettig@@gmail.com}, Scott Chamberlain
#' @examples \dontrun{
#' cr_citation_count(doi = "10.1371/journal.pone.0042793")
#'
#' # Many DOIs, legacy async
#' dois <- c("10.1016/j.fbr.2012.01.001", "10.1371/journal.pone.0042793")
#' cr_citation_count(doi = dois, async = TRUE)
#'
#' # Metadata Plus path (token read from crossref_plus_token env var)
#' Sys.setenv(crossref_plus_token = "my-token")
#' cr_citation_count(doi = dois)
#'
#' # Explicit token and batch size
#' cr_citation_count(doi = dois, plus_token = "my-token", batch_size = 25L)
#' }

cr_citation_count <- function(doi,
    url = "http://www.crossref.org/openurl/",
    key = "cboettig@ropensci.org",
    async = FALSE,
    plus_token = Sys.getenv("crossref_plus_token"),
    batch_size = as.integer(
      Sys.getenv("crossref_batch_size", unset = "50")),
    ...) {

  if (nchar(plus_token) > 0) {
    cr_cc_rest_batched(doi, plus_token = plus_token, batch_size = batch_size, ...)
  } else if (async) {
    cr_cc_async(doi, url, key, ...)
  } else {
    out <- lapply(doi, cr_cc, ur = url, key = key, ...)
    data.frame(doi = doi, count = unlist(out), stringsAsFactors = FALSE)
  }
}

# REST API path: send one batch of DOIs concurrently, return data.frame
cr_cc_rest_async <- function(doi, plus_token, ...) {
  headers <- list(
    `User-Agent` = rcrossref_ua(),
    `X-USER-AGENT` = rcrossref_ua(),
    `Crossref-Plus-API-Token` = paste0("Bearer ", plus_token)
  )
  reqs <- lapply(doi, function(d) {
    endpoint_url <- sprintf(
      "https://api.crossref.org/works/%s",
      utils::URLencode(d, reserved = TRUE)
    )
    crul::HttpRequest$new(
      url = endpoint_url,
      headers = headers,
      opts = list(...)
    )$get()
  })
  cli <- crul::AsyncVaried$new(.list = reqs)
  cli$request()
  cli$status()
  cli$responses()
  bodies <- cli$parse()
  results <- mapply(function(txt, d) {
    tryCatch({
      r <- jsonlite::fromJSON(txt)
      data.frame(
        doi = d,
        count = as.numeric(r$message$`is-referenced-by-count`),
        stringsAsFactors = FALSE
      )
    }, error = function(e) {
      warning("Failed to get citation count for DOI: ", d, call. = FALSE)
      data.frame(doi = d, count = NA_integer_, stringsAsFactors = FALSE)
    })
  }, bodies, doi, SIMPLIFY = FALSE, USE.NAMES = FALSE)
  do.call(rbind, results)
}

# Splits DOIs into batches and calls cr_cc_rest_async for each
cr_cc_rest_batched <- function(doi, plus_token, batch_size = 50L, ...) {
  if (is.na(batch_size) || batch_size < 1L) batch_size <- 50L
  batches <- split(doi, ceiling(seq_along(doi) / batch_size))
  results <- lapply(batches, cr_cc_rest_async, plus_token = plus_token, ...)
  do.call(rbind, results)
}

cr_cc <- function(doi, url, key, ...) {
  args <- list(id = paste("doi:", doi, sep = ""), pid = as.character(key),
                 noredirect = as.logical(TRUE))
  cli <- crul::HttpClient$new(
    url = url,
    headers = list(
      `User-Agent` = rcrossref_ua(), `X-USER-AGENT` = rcrossref_ua()
    )
  )
  cite_count <- cli$get(query = args, ...)
  cite_count$raise_for_status()
  txt <- cite_count$parse("UTF-8")
  if (grepl("malformed doi", txt, ignore.case = TRUE)) {
    warning("Malformed DOI: ", doi, call. = FALSE)
    return(NA_integer_)
  }
  ans <- xml2::read_xml(cite_count$parse("UTF-8"))
  if (get_attr(ans, "status") == "unresolved") {
    NA_integer_
  } else {
    as.numeric(get_attr(ans, "fl_count"))
  }
}

get_attr <- function(xml, attr){
  xml2::xml_attr(xml2::xml_find_all(xml, sprintf("//*[@%s]", attr)), attr)
}
