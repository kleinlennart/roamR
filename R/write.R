# Pages -------------------------------------------------------------------

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

#' Title
#'
#' @param title
#'
#' @return
#' @export
#'
#' @examples
update_page <- function(uid, title, ...) {
  data <- list(
    action = "update-page",
    page = list(
      uid = as.character(uid), # TODO: checks!
      title = as.character(title)
    )
  )

  ## Handling
  # Bad Request if page name is non-unique!!

  roamR::req_roam(data = data, route = "write", ...)
}


#' Title
#'
#' @param uid
#'
#' @return
#' @export
#'
#' @examples
delete_page <- function(uid, ...) {
  data <- list(
    action = "delete-page",
    page = list(
      uid = as.character(uid) # TODO: checks!
    )
  )

  ## Handling
  # Bad Request if page name is non-unique!!

  roamR::req_roam(data = data, route = "write", ...)
}
