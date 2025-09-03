# xronos-api.R
# xronos_*, low-level functions for interacting with the XRONOS API

# Request -----------------------------------------------------------------

#' Make a request to the XRONOS API
#'
#' Request data directly from the XRONOS API. See <https://xronos.ch/api> for
#' supported filters. Use `xronos_request()` to retrieve all data.
#'
#' @param query Valid query string.
#' @param api_url Root address of the XRONOS API. `xronos_api_url()` provides
#'  the default value.
#'
#' @return
#' Parsed JSON response.
#'
#' @export
#'
#' @examples
#' xronos_request("query_labnr=AAR-1847")
xronos_request <- function(query = NA, api_url = xronos_api_url()) {
  if (!is.na(query)) url <- paste0(api_url, "?", query)
  else url <- api_url
  url <- utils::URLencode(url)

  response <- httr::GET(url, xronos_user_agent())

  # Convert HTTP errors to R errors
  # TODO: Check if acceptable to CRAN
  httr::stop_for_status(response, task = "query XRONOS API")

  # Parse & return
  if (httr::http_type(response) == "application/json") {
    xronos_parse(response)
  }
  else {
    rlang::abort(paste0("Unexpected content type of response from XRONOS API:",
                        httr::http_type(response)),
                 class = "xronos_api_error")
  }
}

#' Query the XRONOS API
#'
#' Constructs a query request to the XRONOS API by combining filter variables
#' and values.
#'
#' @param filter Name of a variable or vector of variables to filter by. See
#'  [chron_data()] for a list of supported filters.
#' @param values A vector of values to include. If more than one `filter` is
#'  used, must be a list of the same length.
#'
#' @return
#' Parsed JSON response.
#'
#' @noRd
#' @keywords internal
#'
#' @examples
#' xronos_query(c("site", "material"), list("Stonehenge", c("bone", "charcoal")))
xronos_query <- function(filter, values) {
  purrr::map(filter, xronos_assert_valid_filter)

  value_strings <- purrr::map_chr(values, paste, collapse = "|")
  filter_strings <- paste0("query_", filter, "=", value_strings)
  query <- paste(filter_strings, collapse = "&")

  xronos_request(query)
}



# Parse -------------------------------------------------------------------

#' Parse response from XRONOS API
#'
#' Extracts the content of a [httr::response] object and transforms it into a
#' tidy data frame, replacing a variety of 'empty' values with `NA`.
#'
#' @details
#' Currently makes no assumptions about data structure other than that data is
#' nested within an object named "measurement".
#' Might want to be more strict in future, but would prefer not to have to
#' hard-code expectations from the API.
#'
#' @keywords internal
#' @noRd
xronos_parse <- function(response) {
  content <- httr::content(response, as = "text")
  result <- rjson::fromJSON(content)

  measurements <- lapply(seq_along(result), function(i) {
    process_measurement(result[[i]]$measurement)
  })

  # Combine list of named lists into a data frame and then convert to tibble
  result_df <- dplyr::bind_rows(measurements)

  result_df <- lapply(result_df, normalise_empty)

  return(dplyr::as_tibble(result_df))
}


# Helpers -----------------------------------------------------------------

#' XRONOS web address
#'
#' Helper function to process each measurement
#'
#' @param measurement An individual measurement from the response as JSON
#'
#' @return
#' A row of parsed JSON for an individual measurement.
#'
#' @keywords internal
#' @noRd
process_measurement <- function(measurement) {

  periods <- unlist(
    lapply(
      measurement$periods,
      function(x) x$periode
      ),
    use.names = FALSE)

  typochronological_units <- unlist(
    lapply(
      measurement$typochronological_units,
      function(x) x$typochronological_unit
      ),
    use.names = FALSE
    )

  ecochronological_units <- unlist(
    lapply(
      measurement$ecochronological_units,
      function(x) x$ecochronological_unit
      ),
    use.names = FALSE
    )

  reference <- lapply(
    measurement$reference,
    function(x) list(reference = x$reference)
    )

  measurement$periods <- list(periods)
  measurement$typochronological_units <- list(typochronological_units)
  measurement$ecochronological_units <- list(ecochronological_units)
  measurement$reference <- reference

  return(measurement)
}

#' XRONOS web address
#'
#' Returns the current base URL of XRONOS, or the address of the API version
#' specified.
#'
#' @param version Version of the API to use. Currently has no effect.
#' @param base_url Base URL of XRONOS.
#'
#' @export
#'
#' @examples
#' xronos_url()
#'
#' xronos_api_url()
xronos_url <- function() {
  "https://xronos.ch"
}

#' @rdname xronos_url
#' @export
xronos_api_url <- function(version = "v1", base_url = xronos_url()) {
  if (version == "v1") paste0(base_url, "/api/v1/data")
  else {
    rlang::abort(
      paste0("API version '", version, "' is not supported by this version of xronos."),
      class = "xronos_api_error"
    )
  }
}

#' User agent string for http requests
#'
#' @keywords internal
#' @noRd
xronos_user_agent <- function() {
  httr::user_agent("https://github.com/xronos-ch/xronos.R")
}

#' Check if a variable is in the list of filters supported by the XRONOS API
#'
#' @keywords internal
#' @noRd
xronos_assert_valid_filter <- function(x) {
  valid_filters <- c("labnr", "site", "site_type", "country", "feature",
                     "material", "species")

  if (!x %in% valid_filters) {
    rlang::abort(c(paste0("\"", x, "\" is not a valid filter criterion."),
                   i = "See <https://xronos.ch/api> for a list of valid filters."),
                 class = "xronos_invalid_request")
  }

  invisible(x)
}

#' Normalise empty values in parsed JSON
#'
#' Recurse through the output of jsonlite::parse_json(), replacing empty values
#' (`NULL`, empty lists) with `NA`.
#'
#' @keywords internal
#' @noRd
normalise_empty <- function(x) {
  if (is.list(x)) {
    lapply(x, normalise_empty)
  } else if (is.null(x)) {
    NA
  } else {
    x
  }
}
