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

