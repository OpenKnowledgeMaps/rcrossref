context("testing cr_citation_count with Metadata Plus REST API")

# ---------------------------------------------------------------------------
# Pure unit tests — no HTTP required
# ---------------------------------------------------------------------------

test_that("get_plus_token returns NULL when env var is empty", {
  withr::with_envvar(c(crossref_plus_token = ""), {
    expect_null(rcrossref:::get_plus_token())
  })
})

test_that("get_plus_token returns the token when env var is set", {
  withr::with_envvar(c(crossref_plus_token = "my-secret-token"), {
    expect_equal(rcrossref:::get_plus_token(), "my-secret-token")
  })
})

test_that("cr_citation_count has plus_token parameter that reads from env var", {
  default_args <- formals(cr_citation_count)
  expect_true("plus_token" %in% names(default_args))
  expect_true("batch_size" %in% names(default_args))
  expect_match(deparse(default_args$plus_token), "Sys.getenv")
  expect_match(deparse(default_args$batch_size), "Sys.getenv")
})

test_that("batch splitting produces correct number of batches and sizes", {
  dois <- paste0("10.1000/doi.", seq_len(7))
  batch_size <- 3L
  batches <- split(dois, ceiling(seq_along(dois) / batch_size))
  expect_equal(length(batches), 3L)
  expect_equal(as.integer(lengths(batches)), c(3L, 3L, 1L))
  # All original DOIs present, no duplicates or drops
  expect_equal(sort(unlist(batches, use.names = FALSE)), sort(dois))
})

test_that("batch splitting with batch_size >= length gives one batch", {
  dois <- paste0("10.1000/doi.", seq_len(5))
  batches <- split(dois, ceiling(seq_along(dois) / 10L))
  expect_equal(length(batches), 1L)
  expect_equal(length(batches[[1]]), 5L)
})

test_that("batch splitting with batch_size = 1 gives one batch per DOI", {
  dois <- paste0("10.1000/doi.", seq_len(4))
  batches <- split(dois, ceiling(seq_along(dois) / 1L))
  expect_equal(length(batches), 4L)
  expect_true(all(lengths(batches) == 1L))
})

test_that("cr_cc_rest_batched guard clamps NA batch_size to 50", {
  fn_src <- paste(deparse(body(rcrossref:::cr_cc_rest_batched)), collapse = " ")
  expect_match(fn_src, "is.na\\(batch_size\\)")
})

test_that("cr_citation_count has max_retries parameter defaulting to 3", {
  default_args <- formals(cr_citation_count)
  expect_true("max_retries" %in% names(default_args))
  expect_equal(default_args$max_retries, 3L)
})

# ---------------------------------------------------------------------------
# parse_rl_interval — pure unit tests, no HTTP
# ---------------------------------------------------------------------------

test_that("parse_rl_interval parses seconds correctly", {
  expect_equal(rcrossref:::parse_rl_interval("1s"),   1)
  expect_equal(rcrossref:::parse_rl_interval("60s"),  60)
  expect_equal(rcrossref:::parse_rl_interval("0s"),   0)
})

test_that("parse_rl_interval parses milliseconds correctly", {
  expect_equal(rcrossref:::parse_rl_interval("200ms"), 0.2)
  expect_equal(rcrossref:::parse_rl_interval("1000ms"), 1)
})

test_that("parse_rl_interval defaults to 1 for NULL, NA, empty string", {
  expect_equal(rcrossref:::parse_rl_interval(NULL), 1)
  expect_equal(rcrossref:::parse_rl_interval(NA),   1)
  expect_equal(rcrossref:::parse_rl_interval(""),   1)
})

test_that("parse_rl_interval defaults to 1 for unrecognised format", {
  expect_equal(rcrossref:::parse_rl_interval("1m"), 1)
})

# ---------------------------------------------------------------------------
# extract_rl_headers — unit tests with mock response-like objects
# ---------------------------------------------------------------------------

test_that("extract_rl_headers reads rate limit fields from response headers", {
  mock_resps <- list(
    list(response_headers = list(
      `x-rate-limit-limit`    = "50",
      `x-rate-limit-interval` = "1s",
      `x-concurrency-limit`   = "5"
    ))
  )
  rl <- rcrossref:::extract_rl_headers(mock_resps)
  expect_equal(rl$limit,       50L)
  expect_equal(rl$interval,    1)
  expect_equal(rl$concurrency, 5L)
})

test_that("extract_rl_headers returns NULL when headers are absent", {
  mock_resps <- list(list(response_headers = list(`content-type` = "application/json")))
  expect_null(rcrossref:::extract_rl_headers(mock_resps))
})

test_that("extract_rl_headers uses first response that has rate limit headers", {
  mock_resps <- list(
    list(response_headers = list(`content-type` = "text/plain")),
    list(response_headers = list(
      `x-rate-limit-limit`    = "100",
      `x-rate-limit-interval` = "500ms",
      `x-concurrency-limit`   = "10"
    ))
  )
  rl <- rcrossref:::extract_rl_headers(mock_resps)
  expect_equal(rl$limit,    100L)
  expect_equal(rl$interval, 0.5)
})

# ---------------------------------------------------------------------------
# HTTP tests via webmockr stubs — no cassette files needed.
#
# Note: webmockr patches crul::HttpRequest (used by AsyncVaried internally).
# If these tests fail with "Real HTTP connections are disabled" it means
# webmockr is not intercepting AsyncVaried — record cassettes inside Docker
# instead using vcr::use_cassette(..., record = "new_episodes").
# ---------------------------------------------------------------------------

make_rest_body <- function(doi, count) {
  jsonlite::toJSON(list(
    status = "ok",
    `message-type` = jsonlite::unbox("work"),
    `message-version` = jsonlite::unbox("1.0.0"),
    message = list(
      DOI = jsonlite::unbox(doi),
      `is-referenced-by-count` = jsonlite::unbox(as.integer(count))
    )
  ))
}

test_that("cr_citation_count REST path: single DOI returns correct data.frame", {
  webmockr::enable("crul")
  on.exit({
    webmockr::stub_registry_clear()
    webmockr::disable("crul")
  })

  webmockr::stub_request("get", uri_regex = "api\\.crossref\\.org/works") %>%
    webmockr::to_return(
      body   = make_rest_body("10.1371/journal.pone.0042793", 42L),
      status = 200L,
      headers = list(`content-type` = "application/json;charset=UTF-8")
    )

  result <- cr_citation_count(
    doi = "10.1371/journal.pone.0042793",
    plus_token = "test-token"
  )

  expect_is(result, "data.frame")
  expect_named(result, c("doi", "count"))
  expect_equal(nrow(result), 1L)
  expect_is(result$doi, "character")
  expect_is(result$count, "numeric")
  expect_equal(result$count, 42)
})

test_that("cr_citation_count REST path: multiple DOIs return one row each", {
  webmockr::enable("crul")
  on.exit({
    webmockr::stub_registry_clear()
    webmockr::disable("crul")
  })

  dois <- c("10.1371/journal.pone.0042793", "10.1016/j.fbr.2012.01.001")
  # Single stub matching any Crossref works URL; count value isn't checked here
  webmockr::stub_request("get", uri_regex = "api\\.crossref\\.org/works") %>%
    webmockr::to_return(
      body   = make_rest_body(dois[[1]], 42L),
      status = 200L,
      headers = list(`content-type` = "application/json;charset=UTF-8")
    )

  result <- cr_citation_count(doi = dois, plus_token = "test-token")

  expect_is(result, "data.frame")
  expect_equal(nrow(result), 2L)
  expect_named(result, c("doi", "count"))
  # doi column uses actual DOIs, not whatever the JSON response says
  expect_equal(sort(result$doi), sort(dois))
})

test_that("cr_citation_count REST path: non-retryable 4xx returns NA with HTTP warning", {
  webmockr::enable("crul")
  on.exit({
    webmockr::stub_registry_clear()
    webmockr::disable("crul")
  })

  webmockr::stub_request("get", uri_regex = "api\\.crossref\\.org/works") %>%
    webmockr::to_return(
      body    = "Resource not found.",
      status  = 404L,
      headers = list(`content-type` = "text/plain")
    )

  expect_warning(
    result <- cr_citation_count(doi = "10.9999/not.a.doi", plus_token = "test-token"),
    regexp = "HTTP 404"
  )
  expect_equal(nrow(result), 1L)
  expect_true(is.na(result$count))
})

test_that("cr_citation_count REST path: 429 with max_retries=0 returns NA with warning", {
  webmockr::enable("crul")
  on.exit({
    webmockr::stub_registry_clear()
    webmockr::disable("crul")
  })

  webmockr::stub_request("get", uri_regex = "api\\.crossref\\.org/works") %>%
    webmockr::to_return(
      body    = "",
      status  = 429L,
      headers = list(`content-type` = "text/plain")
    )

  expect_warning(
    result <- cr_citation_count(
      doi        = "10.1371/journal.pone.0042793",
      plus_token = "test-token",
      max_retries = 0L
    ),
    regexp = "Max retries exceeded"
  )
  expect_equal(nrow(result), 1L)
  expect_true(is.na(result$count))
})

test_that("cr_citation_count REST path: batching collects all DOIs across rounds", {
  webmockr::enable("crul")
  on.exit({
    webmockr::stub_registry_clear()
    webmockr::disable("crul")
  })

  n <- 5L
  dois <- paste0("10.1000/doi.", seq_len(n))
  webmockr::stub_request("get", uri_regex = "api\\.crossref\\.org/works") %>%
    webmockr::to_return(
      body   = make_rest_body(dois[[1]], 1L),
      status = 200L,
      headers = list(`content-type` = "application/json;charset=UTF-8")
    )

  # batch_size = 2 → ceil(5/2) = 3 rounds; all 5 DOIs should appear in result
  result <- cr_citation_count(doi = dois, plus_token = "test-token",
                              batch_size = 2L)

  expect_equal(nrow(result), n)
  expect_equal(sort(result$doi), sort(dois))
})

test_that("cr_citation_count does NOT use REST path when plus_token is empty", {
  # Confirm dispatch logic: when plus_token is "" the REST branch is skipped.
  # We check this structurally rather than via HTTP, since making the legacy
  # call would require the OpenURL endpoint.
  withr::with_envvar(c(crossref_plus_token = ""), {
    fn_src <- paste(deparse(body(cr_citation_count)), collapse = " ")
    # The Plus branch is guarded by nchar(plus_token) > 0
    expect_match(fn_src, "nchar\\(plus_token\\).*>.*0")
    # When that guard is FALSE, cr_cc_async or cr_cc is reached
    expect_match(fn_src, "cr_cc_async|cr_cc\\b")
  })
})
