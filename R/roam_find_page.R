#' Find page by name
#'
#' @param header
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
roam_find_page <- function(pattern) {
  # TODO: add a field selector here for query customization
  # TODO: use for renaming vars in tibble results  NAs introduced by coercion to integer range

  page <- roamR::roam_q(
    query = stringr::str_interp('[:find ?p ?title ?create_time :where [?p :node/title "${pattern}"][?p :node/title ?title][?p :create/time ?create_time]]'),
    set_names = TRUE
  )

  page <- page %>% dplyr::mutate(
    create_time = as.numeric(create_time),
    created_time = anytime::anytime(create_time / 1000),
    created_date = as.Date(created_time)
  )

  return(page)
}
