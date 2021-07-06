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
#' @param version Version of the API to use. Currently has no effect.
#'
#' @return
#' Parsed JSON response.
#'
#' @export
#'
#' @examples
#' xronos_request("query_labnr=AAR-1847")
xronos_request <- function(query = NA, api_url = xronos_api_url()) {
  if (!is.na(query)) url <- paste0(xronos_api_url(), "?", query)
  else url <- xronos_api_url()
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
#' @param filter Name of a variable or vector of variables to filter by.
#'  Supported filters are `"labnr"`, `"site"`, `"site_type"`, `"country"`,
#'  `"feature"`, `"material"`, and `"species"`.
#' @param values A vector of values to include. If more than one `filter` is
#'  used, must be a list of the same length.
#'
#' @return
#' Parsed JSON response.
#'
#' @export
#'
#' @examples
#' xronos_query(c("site", "material"), list("Stonehenge", c("bone", "charcoal")))
xronos_query <- function(filter, values) {
  purrr::map(filter, xronos_assert_valid_filter)

  # Argument checking
  if (length(filter) > 1) {
    if (!is.list(values)) {
      rlang::abort("If more than one filter is used, `values` must be a list.",
                   class = "xronos_arg_error")
    }
    else if (length(filter) != length(values)) {
      rlang::abort("`filter` and `values` must have the same length.",
                   class = "xronos_arg_error")
    }
  }
  else {
    if (!is.list(values)) values <- list(values)
  }

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
  result <- jsonlite::parse_json(content)
  result <- purrr::map(result, normalise_empty)
  result <- purrr::map_dfr(result, "measurement")

  result
}


# Helpers -----------------------------------------------------------------

#' @rdname xronos_request
#' @export
xronos_api_url <- function(version = "v1") {
  "https://xronos.ch/api/v1/data"
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
    if (length(x) == 0) NA
    else purrr::map(x, normalise_empty)
  }
  else if (is.null(x)) NA
  else x
}
