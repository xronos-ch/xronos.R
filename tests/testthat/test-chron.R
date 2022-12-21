test_that("chron_data(...) correctly translates parameters to xronos_query()", {
  # Just return the query, don't actually make it
  mockery::stub(xronos_query, "xronos_request", return, depth = 2)

  x <- chron_data(country = "CH", material = c("bone", "charcoal"))
  y <- xronos_query(c("country", "material"), list("CH", c("bone", "charcoal")))
  expect_equal(x, y)
})

test_that("chron_data() prompts for confirmation if `.everything` isn't set", {
  # Don't actually make requests here
  mockery::stub(chron_data, "xronos_request", NULL)

  expect_warning(chron_data(.everything = TRUE), regexp = NA)

  # Simulate interactive mode and prompt
  mockery::stub(chron_data, "interactive", TRUE)
  mockery::stub(chron_data, "utils::askYesNo", TRUE)
  expect_message(chron_data(), regexp = "everything")

  mockery::stub(chron_data, "utils::askYesNo", FALSE)
  expect_null(chron_data())
})

test_that("chron_data() can interpret country names", {
  mockery::stub(xronos_query, "xronos_request", return, depth = 2)

  expect_equal(chron_data(country = c("CH", "DE")),
               chron_data(country = c("Switzerland", "Germany")))
  expect_equal(chron_data(country = c("CH", "Germany")),
               chron_data(country = c("Switzerland", "DE")))
  expect_error(chron_data(country = c("CH", "DE", "Nonsuch Palace")),
               class = "xronos_invalid_request")
})
