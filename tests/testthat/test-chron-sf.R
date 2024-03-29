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
  expect_warning(chron_as_sf(xronos_ch), class = "xronos_lossy_operation")
})

test_that("chron_as_sf() fails gracefully if {sf} is not installed", {
  load("../testdata/xronos_ch.RData")

  mockery::stub(chron_as_sf, "requireNamespace", FALSE)
  expect_error(chron_as_sf(xronos_ch), class = "xronos_missing_package")
})

test_that("chron_drop_na_coords() finds NAs in both columns", {
  missing_lng <- data.frame(lng = c(0, 1, NA), lat = c(0, 1, 2))
  missing_lat <- data.frame(lng = c(0, 1, 2), lat = c(0, 1, NA))
  missing_both <- data.frame(lng = c(0, 1, NA), lat = c(0, 1, NA))
  complete <- data.frame(lng = c(0, 1), lat = c(0, 1))

  expect_equal(chron_drop_na_coords(missing_lng), complete)
  expect_equal(chron_drop_na_coords(missing_lat), complete)
  expect_equal(chron_drop_na_coords(missing_both), complete)
})
