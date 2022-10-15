# object for quicker API use


#' Create a Roam API Headers Object
#'
#' @param graph
#' @param key
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
roam_set_headers <- function(graph, key, ...) {
  #### check header integrity ####
  if (is.null(key)) {
    usethis::ui_stop("`key` is empty. You need to supply a Roam API Key!")
  } else if (!is.character(key)) {
    usethis::ui_stop("`key` is not a character string.")
  }

  if (is.null(graph)) {
    usethis::ui_stop("`graph` is empty. You need to specify the graph you want to query!")
  } else if (!is.character(graph)) {
    usethis::ui_stop("`graph` is not a character string.")
  }

  # generate header object, with other settings as well
  roam_headers <- list(
    GRAPH = graph,
    KEY = key
  )

  # TODO: add dot-dot-dot

  # TODO: give class
  # class(roam_headers) <- "RoamHeaders"

  return(roam_headers)
}



