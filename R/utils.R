# Authentication --------------------------------------------------------

#' Authentication Setup
#'
#' @return
#' @export
#'
#' @examples
set_roam_creds <- function() {
  if (!rlang::is_interactive()) {
    stop("`set_roam_creds()` only works in interactive sessions.")
  }

  choice <- utils::menu(c("Yes", "No"), title = "Do you already have your Roam graphname and API key?")
  if (identical(choice, 2L)) { # if No
    usethis::ui_todo("Then get a new private key here: https://relemma-git-feat-frontdesk.roamresearch.com/#/app")
    Sys.sleep(1)
    utils::browseURL("https://relemma-git-feat-frontdesk.roamresearch.com/#/app")
    Sys.sleep(1)

    if (!usethis::ui_yeah("Would you like to continue with the setup?")) {
      usethis::ui_oops("Setup aborted...")
      rlang::interrupt()
    }
  }

  graph <- askpass::askpass("Please enter the name of your Roam Research Graph.")
  Sys.setenv("ROAM_GRAPH" = graph)

  key <- askpass::askpass("Please enter your Roam Backend API key.")
  Sys.setenv("ROAM_API_KEY" = key)

  usethis::ui_done("Setup successful!")
}


# API Helper --------------------------------------------------------------

#' Title
#' @return
#' @NoRd
get_api_graph <- function() {
  user_id <- Sys.getenv("ROAM_GRAPH")
  if (identical(user_id, "")) {
    stop("No API Graph found. Use set_roam_creds.")
  }
  return(user_id)
}

#' Title
#' @return
#' @NoRd
get_api_key <- function() {
  key <- Sys.getenv("ROAM_API_KEY")
  if (identical(key, "")) {
    stop("No API key found. Use set_roam_creds.")
  }
  return(key)
}

### Old
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


# Other -------------------------------------------------------------------

# func for tibble::as_tibble(.name_repair)
basic_names <- function(names) {
  return(paste0("V", 1:length(names)))
}



#
roam_validate_params <- function(dat) {
  return()
}

#' Title
#' @return
#' @NoRd
roamr_export <- function(resp, output = c("parsed", "raw"), paginated = FALSE) {
  output <- match.arg(output)

  if (paginated) {
    if (identical(output, "raw")) {
      return(resp) # all responses
    } else if (identical(output, "parsed")) {
      parsed <- resp %>%
        purrr::map(httr2::resp_body_json, simplifyVector = TRUE, simplifyDataFrame = TRUE, flatten = TRUE) %>%
        purrr::map(dplyr::bind_rows) %>%
        plyr::rbind.fill()
      # TODO: as_tibble() ??

      return(parsed)
    } else {
      stop("Output format not valid.") # FIXME: run checks on input earlier
    }
  } # not paginated
  else {
    if (identical(output, "raw")) {
      usethis::ui_done("Returning raw API response.")
      return(resp) # single resp
    } else if (identical(output, "parsed")) {
      parsed <- resp %>% httr2::resp_body_json(simplifyVector = TRUE, simplifyDataFrame = TRUE, flatten = TRUE)
      # TODO: as_tibble() ??

      return(parsed)
    }
    # TODO: add S3 class?
  }
}


# Helper ------------------------------------------------------------------

# request needs to fail for it to run!
zoter_error_body <- function(resp) {
  # body <- resp_body_json(resp)
  status <- httr2::resp_status(resp) # integer with L

  if (identical(status, 304L)) {
    message <- "No changes to the library since last request!"
  }

  return(message)
}

# request needs to fail for it to run!
zoter_errors <- function(resp) {
  return(FALSE)
}
