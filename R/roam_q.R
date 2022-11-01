# With header object ------------------------------------------------------

#' Low-level Roam Backend API Wrapper function for q route
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
roam_q <- function(query, header = NULL, verbose = TRUE, format = "tibble", set_names = FALSE) {
  # FIXME: consider changing param order (header before query?!)
  # -> what is easier to type?!

  #### Check params ####
  if (is.null(header)) {
    usethis::ui_stop("{ui_field('header')} is empty. You need to supply a valid roam_headers object!
                      See {ui_code('?roam_set_headers')} for more information.")
  }


  key <- header$KEY
  graph <- header$GRAPH

  #### check header integrity ####
  if (is.null(key)) {
    usethis::ui_stop("The `key` header is empty. You need to supply a Roam API Key!")
  } else if (!is.character(key)) {
    usethis::ui_stop("The `key` header is not a character string.")
  }

  if (is.null(graph)) {
    usethis::ui_stop("The `graph` header is empty. You need to specify the graph you want to query!")
  } else if (!is.character(graph)) {
    usethis::ui_stop("The `graph` header is not a character string.")
  }

  #### check query ####
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


  # EXPERIMENTAL, potentially a lot of edge cases here...
  if (set_names) {
    ui_info("The {ui_field('set_names')} option is experimental. Use with caution!")

    # TODO: add tryCatch
    query_names <- query %>%
      str_extract("(?<=:find).+(?=:where)") %>%
      str_extract_all("(?<=\\?)\\S+") %>%
      unlist()

    usethis::ui_info("Using extracted names:")
    ui_value(query_names) # FIXME: doesn't show!!

    results <- content$result %>%
      as_tibble()
    names(results) <- query_names

  } else {
    ## Tibble
    # muffles the new names message!!
    results <- suppressMessages(content$result %>%
      tibble::as_tibble(.name_repair = "unique"))
    # FIXME: .name_repair = "unique"
  }

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
    # print(results) # see print.tbl ?
    return(results)
  } else {
    usethis::ui_stop("Please set a valid output format!")
  }
}



# Manual headers ----------------------------------------------------------

#' Low-level Roam Backend API Wrapper function for q route
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
roam_q_manual <- function(query, ..., graph = NULL, key = NULL, verbose = TRUE, format = "tibble") {
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

  # parsed
  content <- jsonlite::fromJSON(req, flatten = FALSE)

  ## Tibble
  results <- content$result %>%
    suppressMessages( # muffles the new names message!!
      tibble::as_tibble(.name_repair = "unique")
    ) # FIXME: .name_repair = "unique"
  # TODO: proper rename!

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
