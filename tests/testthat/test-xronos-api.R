test_that("basic request succeeds", {
  skip_if_offline("xronos.ch")
  expect_error(xronos_request("query_labnr=AAR-1847"), NA)
})

test_that("HTTP status codes are converted into R errors", {
  skip_if_offline("httpbin.org")

  mockery::stub(xronos_request, "xronos_api_url", "http://httpbin.org/status/404")
  expect_error(xronos_request(), class = "http_404")

  mockery::stub(xronos_request, "xronos_api_url", "http://httpbin.org/status/500")
  expect_error(xronos_request(), class = "http_500")
})

test_that("non-JSON response from XRONOS is an error", {
  skip_if_offline("xronos.ch")
  mockery::stub(xronos_request, "httr::http_type", "text/plain")
  expect_error(xronos_request("query_labnr=AAR-1847"), class = "xronos_api_error")
})

test_that("we can distinguish between valid and invalid filters", {
  expect_error(xronos_assert_valid_filter("site"), NA)
  expect_error(xronos_assert_valid_filter("an invalid criterion"),
               class = "xronos_invalid_request")
})
