
#
#' Title
#'
#' @param title
#'
#' @return
#' @export
#'
#' @examples
create_page <- function(title, ...) {
  data <- list(
    action = "create-page",
    page = list(
      title = as.character(title) # TODO: checks!
    )
  )

  ## Handling
  # Bad Request if page name is non-unique!!

  roamR::req_roam(data = data, route = "write", ...)
}

