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
  # TODO: use for renaming vars in tibble results  NAs introduced by coercion to integer range

  pages <- roamR::roam_q(
    query = "[:find ?id ?uid ?title ?create_time :where [?id :node/title ?title] [?id :block/uid ?uid] [?id :create/time ?create_time]]",
    header = r, set_names = TRUE
  )

  pages <- pages %>% mutate(
    create_time = as.numeric(create_time),
    time_created = anytime::anytime(create_time / 1000),
    date_created = as.Date(time_created)
  )

  return(pages)
}
