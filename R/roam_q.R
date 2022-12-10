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
roam_q <- function(query, graph = Sys.getenv("ROAM_GRAPH"), key = Sys.getenv("ROAM_API_KEY"), verbose = FALSE, format = c("tibble", "parsed", "json"), set_names = FALSE) {
  # FIXME: consider changing param order (header before query?!)
  # -> what is easier to type?! Switch so Sys.env?

  # FIXME: outsource param checks to helper function!

  ### Checks
  format <- match.arg(format)

  # if not format type
  # if set_names not logical
  # if verbose not logical

  ## check header integrity
  if (is.null(key)) {
    usethis::ui_stop("The {ui_field('key')} header field is empty. You need to supply a Roam API Key!")
  } else if (!is.character(key)) {
    usethis::ui_stop("The {ui_field('key')} header field is not a character string.")
  }

  if (is.null(graph)) {
    usethis::ui_stop("The {ui_field('graph')} header field is empty. You need to specify the graph you want to query!")
  } else if (!is.character(graph)) {
    usethis::ui_stop("The {ui_field('graph')} header field is not a character string.")
  }

  ## check query
  if (is.null(query)) {
    usethis::ui_stop("{ui_field('query')} is empty. What would you like to query?")
  } else if (!is.character(graph)) {
    usethis::ui_stop("{ui_field('query')} is not a character string.")
  }


  #### Build the query ####

  # TODO: generalize for "write" endpoint
  url <- stringr::str_interp("https://api.roamresearch.com/api/graph/${graph}/q")

  # allow different query formattings and deal with escape characters
  query <- query %>%
    stringr::str_replace_all("\\s", " ") %>%
    stringr::str_squish() %>%
    stringr::str_replace_all('"', '\\\\\\\"')

  query_curl <- stringr::str_interp("{\"query\": \"${query}\"}")

  if (verbose) {
    cat(query_curl, "\n")
    write(query_curl, file = "notes/log.txt", append = TRUE)
  }

  #### Run the query ####
  usethis::ui_done("Running query...")
  h <- curl::new_handle() %>%
    curl::handle_setopt(
      followlocation = 1,
      unrestricted_auth = 1,
      verbose = verbose,
      copypostfields = query_curl
    ) %>%
    curl::handle_setheaders(
      "Accept" = "application/json",
      "Content-Type" = "application/json",
      "Authorization" = sprintf("Bearer %s", key),
      "User-Agent" = "roamR package"
    )

  req <- curl::curl_fetch_memory(url, handle = h)
  json <- rawToChar(req$content)

  ## Check API response

  if (req$status_code != "200") {
    usethis::ui_stop("An API error occurred:\n {json}")
  }




  # if (httr::http_type(req) != "application/json") {
  #   stop("API did not return json", call. = FALSE)
  # }

  # FIXME: parse content or just check headers and curl verbose!
  # if (stringr::str_detect(req, '\\{\"message\":')) {
  #   message(req)
  #   usethis::ui_stop("API error!")
  #
  #   # if ((stringr::str_detect(req, '{\"message\":'))) # TODO: specify
  # }


  #### Data wrangling ####
  # Parsing curl JSON response

  json <- rawToChar(req$content)
  parsed <- jsonlite::fromJSON(json, simplifyDataFrame = TRUE, flatten = TRUE)


  # if (is.null(parsed)) {
  #   stop("No results!")
  # }

  # print cURL
  if (verbose) {
    cat(curl::parse_headers(req$headers))

    # TODO: Look at JSON snippet if (verbose == TRUE)
    # jsonlite::prettify(json)
  }

  # Renaming ----------------------------------------------------------------

  # EXPERIMENTAL, potentially a lot of edge cases here...
  # FIXME: try catch, if error default to normal and warn

  if (set_names) {
    usethis::ui_info("The {ui_field('set_names')} option is experimental. Use with caution!")

    # TODO: add tryCatch
    query_names <- query %>%
      stringr::str_extract("(?<=:find).+(?=:where)") %>%
      stringr::str_extract_all("(?<=\\?)\\S+") %>%
      unlist()

    usethis::ui_info("Using extracted names:")
    usethis::ui_info(ui_value(query_names)) # FIXME: doesn't show!!

    results <- parsed %>%
      as.data.frame() %>%
      setNames(query_names) %>%
      tibble::as_tibble()
  } else {
    usethis::ui_info("Using default names.")
    results <- parsed %>%
      as.data.frame() %>%
      setNames(paste0("V", 1:length(.))) %>%
      tibble::as_tibble()
  }


  # Data Return -------------------------------------------------------------
  # TODO: use switch instead?

  if (format == "json") {
    usethis::ui_done("Successful!")
    return(json)
  } else if (format == "parsed") {
    usethis::ui_done("Successful!")
    return(parsed)
  } else if (format == "tibble") {
    usethis::ui_done("Successful!")
    # print(results) # see print.tbl
    return(results)
  } else {
    usethis::ui_stop("Please set a valid output format!")
    # FIXME: check before running the query!!
  }
}


# Manual headers version ----------------------------------------------------------

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
