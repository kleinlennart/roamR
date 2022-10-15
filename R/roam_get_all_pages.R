#' Get all Pages in the Roam Graph
#'
#' @param header
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
roam_get_all_pages <- function(header = NULL, ...) {
  # TODO: add a field selector here for query customization
  # TODO: use for renaming vars in tibble results

  pages <- roamR::roam_q(
    query = "[:find ?p ?title :where [?p :node/title ?title]]", header = header,
    # format = "tibble", # NOTE: should defautl to "tibble" for easier handling
    ...
  )

  return(pages)
}
