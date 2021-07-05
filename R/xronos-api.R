# xronos-api.R
# Low-level functions for interacting with the XRONOS API

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

  response <- httr::stop_for_status(httr::GET(url),
                                    task = "query XRONOS API")

  if (httr::http_type(response) == "application/json") {
    result <- jsonlite::fromJSON(httr::content(response, as = "text"))
  }
  else {
    rlang::abort(paste0("Unexpected content type of response from XRONOS API:",
                        httr::http_type(response)),
                 class = "xronos_api_error")
  }

  result
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



# Helpers -----------------------------------------------------------------

#' @rdname xronos_request
#' @export
xronos_api_url <- function(version = "v1") {
  "https://xronos.ch/api/v1/data"
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
