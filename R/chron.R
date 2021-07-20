# chron.R
# chron_*; high-level functions for retrieving and working with chronological
# data from XRONOS

#' Get chronological data from XRONOS
#'
#' Retrieves chronological data from XRONOS. `chron_data()` retrieves all
#' records. Additional parameters can be used to filter the results.
#'
#' @param ... (Optional) One or more named arguments specifying filter variables
#'  and values to include. See [xronos_query()] for a list of filters supported
#'  by the XRONOS API.
#' @param .everything If `TRUE`, suppresses interactive mode prompt when
#'  retrieving all records from XRONOS (see details).
#'
#' @details
#'
#' To reduce unnecessary server load, `get_xronos()` (without any filters) will
#' prompt for confirmation in interactive mode interactive mode. Set
#' `.everything = TRUE` to suppress this.
#'
#' @export
#'
#' @examples
#' # Dates on charcoal or bone from Switzerland
#' chron_data(country = "Switzerland", material = c("charcoal", "bone"))
chron_data <- function(..., .everything = NA) {
  params <- rlang::list2(...)

  if (rlang::is_empty(params)) {
    if (!isTRUE(.everything) && interactive()) {
      answer <- utils::askYesNo("Download all records from XRONOS? (This may take some time.)",
                                default = FALSE)
      if (isTRUE(answer)) {
        cli::cli_alert_info("Use `.everything = TRUE` to suppress this prompt in future.")
      }
      else {
        return(invisible(NULL))
      }
    }

    xronos_request()
  }
  else {
    xronos_query(names(params), params)
  }
}

#' Convert a table of chronological data to an sf object
#'
#' `chron_as_sf()` adds a simple features geometry column (see [sf::st_sf]) to
#' chronological data from XRONOS, using the latitude and longitude columns.
#'
#' `chron_drop_na_coords()` excludes rows with missing or invalid coordinates;
#' it is implicitly called by `chron_as_sf()`, issuing a warning if any rows
#' were removed.
#'
#' @param x data.frame retrieved with [chron_data()].
#' @param crs Desired coordinate reference system in a format understood by
#'  [sf::st_crs()]. Defaults to latitude/longitude on the WGS84 ellipsoid
#'  (EPSG:4326).
#' @param .warn If `TRUE`, issues a warning when rows are removed.
#'
#' @return
#' `chron_as_sf()` returns `x` converted into an `sf` object. The original
#' coordinate columns are retained.
#'
#' `chron_drop_na_coords()` returns `x` without rows with missing or invalid
#' coordinates.
#'
#' @export
#'
#' @examples
#' x <- chron_data(country = "Switzerland")
#' chron_as_sf(x)
chron_as_sf <- function(x, crs = sf::st_crs(4326)) {
  if (requireNamespace("sf", quietly = TRUE)) {
    y <- chron_drop_na_coords(x, .warn = TRUE)
    y <- sf::st_as_sf(y, coords = c("lng", "lat"), crs = 4326, remove = FALSE)

    if (crs != sf::st_crs(4326)) {
      y <- sf::st_transform(y, crs)
    }

    y
  }
  else {
    rlang::abort("`chron_as_sf()` requires package `sf`.",
                 class = "xronos_missing_package")
  }
}

#' @rdname chron_as_sf
#' @export
chron_drop_na_coords <- function(x, .warn = FALSE) {
  y <- x[!is.na(x$lng) & !is.na(x$lat),]

  d <- nrow(x) - nrow(y)
  if (d > 0 & isTRUE(message)) {
    rlang::warn(paste(d, "rows with missing or invalid coordinates were removed."),
                class = "xronos_lossy_operation")
  }

  y
}
