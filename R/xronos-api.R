# xronos-api.R
# Low-level functions for interacting with the XRONOS API

#' @rdname xronos_query
#' @export
xronos_api_url <- function(version = "v1") {
  "https://xronos.ch/api/v1/data"
}

#' Query the XRONOS API
#'
#' Directly query the XRONOS API. See <https://xronos.ch/api> for supported
#' formats.
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
#' xronos_query("query_labnr=AAR-1847")
xronos_query <- function(query, api_url = xronos_api_url()) {
  url <- paste0(xronos_api_url(), "?", query)
  response <- httr::GET(url)

  jsonlite::parse_json(response)
}
