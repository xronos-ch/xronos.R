test_that("basic request succeeds", {
  skip_if_offline("xronos.ch")
  expect_error(xronos_request("query_labnr=AAR-1847"), NA)
})

test_that("we can distinguish between valid and invalid filters", {
  expect_error(xronos_assert_valid_filter("site"), NA)
  expect_error(xronos_assert_valid_filter("an invalid criterion"),
               class = "xronos_invalid_request")
})
