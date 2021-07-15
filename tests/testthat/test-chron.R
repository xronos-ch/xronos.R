
# chron_data() ------------------------------------------------------------

test_that("chron_data(...) correctly translates parameters to xronos_query()", {
  skip_if_offline("xronos.ch")

  x <- chron_data(country = "Switzerland", material = c("bone", "charcoal"))
  y <- xronos_query(c("country", "material"), list("Switzerland", c("bone", "charcoal")))
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

# chron_as_sf() -----------------------------------------------------------

test_that("chron_as_sf() returns a valid sf object", {
  load("../testdata/xronos_ch.RData")
  x <- xronos_ch[!is.na(xronos_ch$lat) & !is.na(xronos_ch$lng),]
  x <- chron_as_sf(x)

  expect_s3_class(x, "sf")
  expect_true(all(sf::st_is_valid(x)))
})

test_that("chron_as_sf() returns object with desired CRS", {
  load("../testdata/xronos_ch.RData")
  x <- xronos_ch[!is.na(xronos_ch$lat) & !is.na(xronos_ch$lng),]

  y <- chron_as_sf(x)
  expect_equal(sf::st_crs(y), sf::st_crs(4326))

  y <- chron_as_sf(x, crs = 2056)
  expect_equal(sf::st_crs(y), sf::st_crs(2056))
})

test_that("chron_as_sf() warns about dropped rows", {
  load("../testdata/xronos_ch.RData")
  expect_warning(chron_as_sf(xronos_ch), class = "xronos_lossy_conversion")
})

test_that("chron_as_sf() fails gracefully if {sf} is not installed", {
  load("../testdata/xronos_ch.RData")

  mockery::stub(chron_as_sf, "requireNamespace", FALSE)
  expect_error(chron_as_sf(xronos_ch), class = "xronos_missing_package")
})
