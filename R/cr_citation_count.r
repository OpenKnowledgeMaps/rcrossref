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
#'   environment variable, or 50 if unset. Should not exceed the
#'   `x-concurrency-limit` returned by the Crossref API.
#' @param max_retries (integer) maximum number of retry attempts for failed
#'   requests (HTTP 429 or 5xx) on the Plus REST path. Default: 3.
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
    max_retries = 3L,
    ...) {

  if (nchar(plus_token) > 0) {
    cr_cc_rest_batched(doi, plus_token = plus_token, batch_size = batch_size,
                       max_retries = max_retries, ...)
  } else if (async) {
    cr_cc_async(doi, url, key, ...)
  } else {
    out <- lapply(doi, cr_cc, ur = url, key = key, ...)
    data.frame(doi = doi, count = unlist(out), stringsAsFactors = FALSE)
  }
}

# Parse Crossref interval strings ("1s", "200ms") -> numeric seconds
parse_rl_interval <- function(s) {
  if (is.null(s) || is.na(s) || !nzchar(trimws(s))) return(1)
  s <- trimws(s)
  if (grepl("ms$", s)) as.numeric(sub("ms$", "", s)) / 1000
  else if (grepl("s$", s)) as.numeric(sub("s$", "", s))
  else 1
}

# Extract x-rate-limit-* and x-concurrency-limit from the first response
# that carries them; returns a named list or NULL.
extract_rl_headers <- function(resps) {
  for (r in resps) {
    h   <- r$response_headers
    lim <- h[["x-rate-limit-limit"]]
    if (!is.null(lim)) {
      return(list(
        limit       = as.integer(lim),
        interval    = parse_rl_interval(h[["x-rate-limit-interval"]]),
        concurrency = suppressWarnings(as.integer(h[["x-concurrency-limit"]]))
      ))
    }
  }
  NULL
}

# Send one concurrent batch of DOIs.
# Returns list(data = <data.frame>, rl = <rate-limit info or NULL>).
cr_cc_rest_async <- function(doi, plus_token, max_retries = 3L, ...) {
  headers <- list(
    `User-Agent` = rcrossref_ua(),
    `X-USER-AGENT` = rcrossref_ua(),
    `Crossref-Plus-API-Token` = paste0("Bearer ", plus_token)
  )

  make_reqs <- function(dois) lapply(dois, function(d) {
    crul::HttpRequest$new(
      url     = sprintf("https://api.crossref.org/works/%s",
                        utils::URLencode(d, reserved = TRUE)),
      headers = headers,
      opts    = list(...)
    )$get(query = list(select = "DOI,is-referenced-by-count"))
  })

  pending  <- doi
  resolved <- list()   # named by doi -> one-row data.frame
  rl       <- NULL
  backoff  <- 1        # seconds, doubles on each retry

  for (attempt in seq_len(max_retries + 1L)) {
    if (attempt > 1L) Sys.sleep(backoff)

    cli <- crul::AsyncVaried$new(.list = make_reqs(pending))
    cli$request()
    statuses <- cli$status_code()
    resps    <- cli$responses()
    bodies   <- cli$parse()

    if (is.null(rl)) rl <- extract_rl_headers(resps)

    retry_dois  <- character(0)
    retry_after <- NULL

    for (i in seq_along(pending)) {
      d  <- pending[[i]]
      sc <- statuses[[i]]

      if (sc == 200L) {
        resolved[[d]] <- tryCatch({
          r <- jsonlite::fromJSON(bodies[[i]])
          data.frame(doi = d,
                     count = as.numeric(r$message$`is-referenced-by-count`),
                     stringsAsFactors = FALSE)
        }, error = function(e) {
          warning("Failed to parse response for DOI: ", d, call. = FALSE)
          data.frame(doi = d, count = NA_integer_, stringsAsFactors = FALSE)
        })
      } else if (sc == 429L || sc >= 500L) {
        retry_dois <- c(retry_dois, d)
        if (sc == 429L && is.null(retry_after)) {
          ra <- resps[[i]]$response_headers[["retry-after"]]
          if (!is.null(ra)) retry_after <- as.numeric(ra)
        }
      } else {
        warning("HTTP ", sc, " for DOI: ", d, call. = FALSE)
        resolved[[d]] <- data.frame(doi = d, count = NA_integer_,
                                    stringsAsFactors = FALSE)
      }
    }

    pending <- retry_dois
    if (length(pending) == 0L) break

    backoff <- if (!is.null(retry_after)) retry_after
               else min(backoff * 2, 60)
    retry_after <- NULL
  }

  # Any DOIs that exhausted retries
  for (d in pending) {
    warning("Max retries exceeded for DOI: ", d, call. = FALSE)
    resolved[[d]] <- data.frame(doi = d, count = NA_integer_,
                                stringsAsFactors = FALSE)
  }

  list(data = do.call(rbind, resolved[doi]), rl = rl)
}

# Splits DOIs into batches and calls cr_cc_rest_async for each.
# Uses rate-limit headers from each batch to compute inter-batch sleep.
cr_cc_rest_batched <- function(doi, plus_token, batch_size = 50L,
                                max_retries = 3L, ...) {
  if (is.na(batch_size) || batch_size < 1L) batch_size <- 50L
  batches     <- split(doi, ceiling(seq_along(doi) / batch_size))
  all_results <- vector("list", length(batches))
  rl          <- NULL

  for (i in seq_along(batches)) {
    # Sleep before this batch based on rate-limit headers from the previous one
    if (i > 1L && !is.null(rl) && !is.na(rl$limit) && rl$limit > 0L) {
      sleep_secs <- (length(batches[[i - 1L]]) / rl$limit) * rl$interval
      if (sleep_secs > 0) Sys.sleep(sleep_secs)
    }

    out              <- cr_cc_rest_async(batches[[i]], plus_token = plus_token,
                                         max_retries = max_retries, ...)
    all_results[[i]] <- out$data
    if (is.null(rl) && !is.null(out$rl)) rl <- out$rl
  }

  do.call(rbind, all_results)
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
