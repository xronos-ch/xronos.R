# chron.R
# chron_*; high-level functions for retrieving and working with chronological
# data from XRONOS

#' Get chronological data from XRONOS
#'
#' Retrieves chronological data from XRONOS. `chron_data()` retrieves all
#' records. Additional parameters can be used to filter the results.
#'
#' @param ... (Optional) One or more named arguments specifying filter variables
#'  and values to include. Supported filters are `"labnr"`, `"site"`,
#'  `"site_type"`, `"country"`, `"feature"`, `"material"`, and `"species"`.
#' @param .everything If `TRUE`, suppresses interactive mode prompt when
#'  retrieving all records from XRONOS (see details).
#'
#' @details
#'
#' To reduce unnecessary server load, `get_xronos()` (without any filters) will
#' prompt for confirmation in interactive mode interactive mode. Set
#' `.everything = TRUE` to suppress this.
#'
#' `country` should be a valid
#' [ISO two-letter country code](https://en.wikipedia.org/wiki/ISO_3166-1_alpha-21).
#' Otherwise, the function will try to interpret as such using
#' [countrycode::countryname()], or return an error if any values of `country`
#' cannot be matched.
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
    if ("country" %in% names(params)) {
      unknown_countries <- !is_country_code(params[["country"]])
      if (any(unknown_countries)) {
        params[["country"]][unknown_countries] <- normalise_country_code(params[["country"]][unknown_countries])
      }
    }
    xronos_query(names(params), params)
  }
}

#' Normalise country codes
#'
#' Attempts to interpret a vector as ISO two-character country codes. Error if
#' not all can be matched.
#'
#' @noRd
#' @keywords internal
normalise_country_code <- function(x) {
  x <- countrycode::countryname(x, destination = "iso2c", warn = FALSE)
  if (any(rlang::are_na(x))) {
    rlang::abort(
      c("`country` must be a valid ISO two-character country code, or something interpretable as one.",
        i = "See ?countrycode::countryname for interpretable values."),
      class = "xronos_invalid_request"
    )
  }
  else {
    return(x)
  }
}

#' Checks if a vector is a valid ISO two-character country code
#'
#' @noRd
#' @keywords internal
is_country_code <- function(x) {
  x %in% countrycode::codelist$iso2c
}
