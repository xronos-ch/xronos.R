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
