# Basic q function wrapper cURL --------------------------------------------

#' Roam Backend API 'q' route
#'
#' @description Query the graph
#'
#' @param query Query string
#' @param graph Name of Roam graph
#' @param key Graph API Key
#' @param verbose Show query output
#' @param format Output format
#'
#' @return a results tibble
#' @export
#'
#' @examples
roam_q <- function(query, graph = NULL, key = NULL, verbose = FALSE, format = "tibble") {
  #### Check params ####
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

  if (is.null(query)) {
    usethis::ui_stop("`query` is empty. What would you like to query?")
  } else if (!is.character(graph)) {
    usethis::ui_stop("`query` is not a character string.")
  }


  #### Build the query ####
  base_curl <- "curl -X POST \"https://api.roamresearch.com/api/graph/<GRAPH>/q\" --location-trusted \\\n  -H  \"accept: application/json\" \\\n  -H  \"Authorization: Bearer <API-KEY>\" \\\n  -H  \"Content-Type: application/json\" \\\n  -H  \"User-Agent: roamR\" \\\n  -d \"{\\\"query\\\" : \\\"<QUERY>\\\"}\""

  # TODO: adjust query text (escape ", etc.)
  # query <- query %>%

  # insert params
  curl <- base_curl %>%
    stringr::str_replace("<GRAPH>", graph) %>%
    stringr::str_replace("<API-KEY>", key) %>%
    stringr::str_replace("<QUERY>", query)

  # print cURL
  if (verbose) {
    usethis::ui_info("Running API Call:")
    cat(curl, "\n\n")
  }

  #### Run the query ####
  req <- system(curl, intern = TRUE) # capture output
  # TODO: check libcurl capabilities later...

  ## Check API response
  if (stringr::str_detect(req, '\\{\"message\":')) {
    message(req)
    usethis::ui_stop("API error!")

    # if ((stringr::str_detect(req, '{\"message\":'))) # TODO: specify
  }

  #### Data wrangling ####

  content <- jsonlite::fromJSON(req, flatten = FALSE)

  ## Tibble
  results <- content$result %>%
    tibble::as_tibble(.name_repair = "unique") # FIXME: .name_repair = "unique"
  # TODO: rename!

  # Look at JSON if (verbose == TRUE)
  # jsonlite::prettify(req)

  if (format == "json") {
    usethis::ui_done("Successful!")
    return(req)
  } else if (format == "parsed") {
    usethis::ui_done("Successful!")
    return(content)
  } else if (format == "tibble") {
    usethis::ui_done("Successful!")
    return(results)
  } else {
    usethis::ui_stop("Please set a valid output format!")
  }
}

# roam_q(
#   query = "[:find ?p ?title :where [?p :node/title ?title]]",
#   graph = "roamr",
#   key = Sys.getenv("ROAMR"),
#   verbose = TRUE,
#   format = "tibble"
# )
