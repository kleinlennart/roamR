#' Low-level Roam Backend API Wrapper function for q route
#'
#' @description Query the graph
#'
#' @param query Query string
#' @param verbose Show query output
#' @param format Output format
#'
#' @return a results tibble
#' @export
#'
#' @examples
req_roam <- function(query = NULL, data = NULL, verbose = FALSE, format = c("tibble", "parsed", "json"), set_names = FALSE, ...) {
  ### Checks
  params <- list(
    ...
  )
  format <- match.arg(format)


  #### Build the query ####

  # allow different query formattings and deal with escape characters
  # query <- query %>%
  #   stringr::str_replace_all("\\s", " ") %>%
  #   stringr::str_squish() %>%
  #   stringr::str_replace_all('"', '\\\\\\\"')

  # if (verbose) {
  #   cat(query_curl, "\n")
  #   write(query_curl, file = "notes/log.txt", append = TRUE)
  # }


  if(is.null(data)) {
    data <- list(query = query)
  } else if (is.null(query)) {
    query <- data[["query"]] # needed for rename
  }

  #### Run the query ####
  req <- httr2::request("https://api.roamresearch.com/api/") %>%
    httr2::req_url_path_append("graph") %>%
    httr2::req_url_path_append(roamR:::get_api_graph()) %>% #
    httr2::req_url_path_append("q") %>% # write
    httr2::req_user_agent("roamR (https://github.com/kleinlennart/roamR)") %>%
    httr2::req_headers(Authorization = paste("Bearer", roamR:::get_api_key())) %>%
    httr2::req_headers(`Content-Type` = "application/json") %>%
    httr2::req_headers(Accept = "application/json") %>%
    httr2::req_options(
      # copypostfields = query_curl,
      # verbose = verbose,
      ## most important options:
      followlocation = 1, # should write a blog post on this!
      unrestricted_auth = 1
    ) %>%
    httr2::req_body_json(data = data)

  # httr2::req_url_query(!!!params) %>%
  # httr2::req_error(is_error = zoter::zoter_errors, body = zoter::zoter_error_body) %>%

  usethis::ui_done("Running query...")
  req %>% httr2::req_dry_run()
  resp <- req %>% httr2::req_perform(verbosity = as.integer(verbose))

  ## Check API response

  # if (req$status_code != "200") {
  #   usethis::ui_stop("An API error occurred:\n {json}")
  # }

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

  ### HEADERS

  # print cURL
  # if (verbose) {
  #   cat(curl::parse_headers(req$headers))
  #
  #   # TODO: Look at JSON snippet if (verbose == TRUE)
  #   # jsonlite::prettify(json)
  # }


  ## Parsing
  parsed <- resp %>% httr2::resp_body_json(simplifyVector = TRUE, simplifyDataFrame = TRUE, flatten = TRUE)

  # if (is.null(parsed)) {
  #   stop("No results!")
  # }

  # Renaming ----------------------------------------------------------------
  if (set_names) {
    usethis::ui_info("The {ui_field('set_names')} option is experimental. Use with caution!")

    # TODO: add tryCatch
    query_names <- query %>%
      stringr::str_extract("(?<=:find).+(?=:)") %>% # :where or :in
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

  return(results)

  #   # Data Return -------------------------------------------------------------
  #   # TODO: use switch instead?
  #
  #   if (format == "json") {
  #     usethis::ui_done("Successful!")
  #     return(json)
  #   } else if (format == "parsed") {
  #     usethis::ui_done("Successful!")
  #     return(parsed)
  #   } else if (format == "tibble") {
  #     usethis::ui_done("Successful!")
  #     # print(results) # see print.tbl
  #     return(results)
  #   } else {
  #     usethis::ui_stop("Please set a valid output format!")
  #     # FIXME: check before running the query!!
  #   }
}
