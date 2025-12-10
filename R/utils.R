#' Setup Canvas for API use
#'
#' Wrapper for two `rcanvas` functions: [rcanvas::set_canvas_token()] and
#' [rcanvas::set_canvas_domain()]. Calls [Sys.getenv()] for both,
#' so this must be set up ahead of time (see Details).
#'
#' To use this function, you must have a Canvas access token stored in your
#' Renviron file as `CANVAS_TOKEN`, and the domain URL for your institution
#' stored as `CANVAS_DOMAIN`. See [cnvs::set_renv_variables()] for more.
#'
#' @returns Sets the Canvas token and domain.
#' @export

canvas_setup <- function(){
  if (Sys.getenv("CANVAS_TOKEN") == "")
    stop("You have not successfully created an environmental CANVAS_TOKEN variable. Check your Renviron or run cnvs::set_renv_variables()")
  if (Sys.getenv("CANVAS_DOMAIN") == "")
    stop("You have not successfully created an environmental CANVAS_DOMAIN variable.")
  rcanvas::set_canvas_token(Sys.getenv("CANVAS_TOKEN"))
  rcanvas::set_canvas_domain(Sys.getenv("CANVAS_DOMAIN"))
}

#' Create a string for the current academic year
#'
#' Checks the current year and produces an academic year abbreviation, e.g.
#' "22/23" for the academic year spanning 2022 to 2023.
#'
#' @param switch_date A date object containing the date/time marking the
#' boundary between academic years. Default is September 1st in user's locale
#'
#' @returns A string e.g. "22/23"
#' @export

get_ac_year <- function(switch_date = as.Date(paste0(format(Sys.Date(), "%Y"), "-09-01"))){
  current_year_short <- format(Sys.Date(), "%y")
  ifelse(Sys.Date() > switch_date, # checks if the current date is after switch_date
         paste(current_year_short, as.numeric(current_year_short) + 1, sep = "/"), # "next" academic year
         paste(as.numeric(current_year_short) - 1, current_year_short, sep = "/")) # "previous" academic year
}

#' Get Canvas module ID
#'
#' A nicer way to get the ID number for a module on Canvas. Essentially lets you
#' look up https://canvas.youruni.ac.uk/courses/THIS_NUMBER for use in other
#' Canvas API operations
#'
#' @param search_term A string containing word or phrase (exact) in the module
#'   title to search for, OR a module code (not both!)
#' @param academic_year A string containing the academic year for the desired
#'   module as e.g. "22/23"; defaults to the current year as calculated by
#'   [cnvs::get_ac_year()]. Does not need to be specified for ongoing/non-academic
#'   modules
#' @param quietly Should the message about academic year be suppressed?
#'
#' @returns Canvas module ID as a numeric vector.
#' @export

get_module_id <- function(search_term, academic_year, quietly = FALSE){

  if(missing(academic_year)){
    academic_year <- cnvs::get_ac_year()
    if(!quietly) {message(paste("Using current year", academic_year, "if necessary"))}
  }

  module_list <- cnvs::get_module_list(academic_modules = FALSE)

  if(## Check if search term looks like a module code
    all(stringr::str_detect(search_term, "^[A-Z]?[0-9]+[A-Z]?[0-9]+"))){

    if (!academic_year %in% "current" & !grepl("^[1-2][0-9]\\/[1-2][0-9]$", academic_year))
      stop("Academic year is not specified correctly. Please enter either 'current', 'continuous', or the last two digits of each year separated by a slash, e.g. '21/22'",
           call. = F)

    ## Get the current academic year
    # if(academic_year == "current"){
    #   academic_year <- get_ac_year()
    #   message(paste("Using current year", academic_year))
    # }

    module_id <- module_list |>
      dplyr::filter(
        module_code == search_term &
          ac_year == academic_year
      ) |>
      dplyr::pull(id)

  } else {

    module_id <- module_list |>
      dplyr::filter(
        grepl(search_term, name, ignore.case = TRUE)
      ) |>
      dplyr::pull(id)

    if (length(module_id) > 1){
      stop("There is more than one module that matches your search term. Please try again with a unique search term.",
           call. = FALSE)
    }
    if (length(module_id) == 0){
      stop("Your search did not return any results. Please try again with a unique search term.",
           call. = FALSE)
    }
  }

  # Still doesn't bloody work for some reason!
  # grepl(paste("(?<=", search_term, ")", sep = "", collapse = ""),
  #       name, ignore.case = TRUE, perl = TRUE)

    return(module_id)

  ## Also still can't get the multiple lookahead matches to work!

}

#' Check module code input and attempt to return the Canvas module ID
#'
#' @param search_term A string containing word or phrase (exact) in the module
#'   title to search for, OR a module code (not both!)
#' @param academic_year A string containing the academic year for the desired
#'   module as e.g. "22/23"; defaults to the current year as calculated by
#'   [cnvs::get_ac_year()]. Does not need to be specified for ongoing/non-academic
#'   modules
#' @param quietly Should the message about academic year be suppressed?
#'
#' @returns Canvas module ID
#' @export

set_module_id <- function(search_term, academic_year, quietly = FALSE){

  if(missing(academic_year)){
    academic_year <- cnvs::get_ac_year()
    if(!quietly) {message(paste("Using current year", academic_year, "if necessary"))}
  }

  # Try to get the Canvas module id
  module_id <- try(cnvs::get_module_id(search_term, academic_year), silent = TRUE)

  # If that didn't work, try running canvas_setup() to set the domain and token
  if(inherits(module_id, "try-error")){
    cnvs::canvas_setup()
    module_id <- cnvs::get_module_id(search_term, academic_year)
  }

  return(module_id)
}

#' Set up local folder for writing Quarto/Canvas pages
#'
#' Create a folder to organise Quarto documents that can be turned into Canvas
#' pages using [quarto_page()], or quickly set the correct folder as the working
#' directory.
#'
#' @param local_path File path to the folder to contain the new site folder.
#' @param search_term String to search for on Canvas.
#' @param set_wd Boolean. Whether the new file path should be set as the working
#'   directory.
#' @param template Optional file path to a Quarto file to use as a template.
#'   Will be copied into the new file folder as a new document called
#'   "template.qmd".
#' @param module_list Provide an object containing an existing module list from
#'   [get_module_list()]. If none provided, runs [get_module_list()].
#' @returns A new file folder as a subfolder of `local_path`, named "module_id
#'   Full Name Of Canvas Module", and the module path as a string
#'
#' @export

cnvs_folder <- function(local_path, search_term, module_list, set_wd = FALSE, template){

  if(missing(module_list)){
    module_list <- get_module_list()
  }

  if(missing(template)){
    template <- c(
      "---",
      "title: 'Title'",
      "format:",
      "  html:",
      "    theme: default",
      "    toc: true",
      "    toc-location: body",
      "    toc-title: 'Contents'",
      "editor:",
      "  markdown:",
      "    wrap: none",
      "execute:",
      "  echo: false",
      "self-contained: true",
      "---",
      "",
      "```{r}",
      "source(here::here('render_helpers', 'canvas_setup.R')",
      "```"
    )
  } else{
    template <- readLines(template)
  }

  module_id <- get_module_id(search_term)

  # Create a file path to a new folder
  module_path <- file.path(local_path, paste(module_id, module_list$name[module_list$id == module_id]))

  canvas_setup <- c(
    "library(cnvs)",
    "cnvs::canvas_setup()"
  )

  if(!dir.exists(module_path)){
    dir.create(module_path)
    writeLines(template, file.path(module_path, "template.qmd"))
  }

  if(!dir.exists(file.path(module_path, "render_helpers"))){
    dir.create(file.path(module_path, "render_helpers"))
    writeLines(canvas_setup, file.path(module_path, "render_helpers", "canvas_setup.R"))
  }

  if(set_wd == TRUE){
    set_wd(module_path)
  }

  return(module_path)
}

#' Publish (offer) a Canvas module
#'
#' @param search_term Passed to [get_module_id()] to return the module ID
#'
#' @return Reply from Canvas
#' @export

publish_module <- function(search_term){

  module_id <- cnvs::get_module_id(search_term)

  rcanvas:::canvas_query(
    url = paste0(rcanvas:::canvas_url(), file.path("/courses", module_id)),
    args = list(
      `offer` = TRUE
    ),
    "PUT"
  )
}

#' Get a response from Canvas
#'
#' @param url A Canvas API URL
#' @param args Arguments to add to the query
#'
#' @returns A tibble containing the requested information
#' @export

get_resp <- function(url, args){

  args <- c(list(access_token = rcanvas:::check_token(), per_page = 100),
            args)

  resp <- rcanvas:::process_response(url, args) |>
  dplyr::mutate(course_id = module_id)

  return(resp)
}

#' Generate a Canvas URL
#'
#' Copied from the unexported `canvas_url()` function from [the {rcanvas} package](https://github.com/daranzolin/rcanvas).
#'
#' @returns A Canvas API URL
#' @export

rcanvas_canvas_url <- function(){
  paste0(get("rcanvas_CANVAS_DOMAIN", envir = cdenv),
         "/api/v1")
}

#' Generate a Canvas URL
#'
#' @returns A Canvas API URL
#' @export

canvas_url <- function(){
  paste0(Sys.getenv("CANVAS_DOMAIN"), "/api/v1")
}

#' Set Up Canvas variables
#'
#' Edits the user's .Renviron file with new or updated environ variables and sets these for use with Canvas/`cnvs`.
#'
#' This interactive function guides the user through the creation (or updating, if they already exist) of two variables, CANVAS_TOKEN and CANVAS_DOMAIN. The function first checks if there are already variables defined in the local .Renviron file with these names, and gives the option to update them or not.
#' Once both values are put in, the function then sets these for use with Canvas using [rcanvas::set_canvas_token()] and [rcanvas::set_canvas_domain()] respectively.
#'
#' To obtain a Canvas access token:
#'
#' * Log into Canvas as normal.
#' * Click on your profile picture, then on Settings.
#' * Click on the "+ New access token" button and follow the instructions.
#'
#' Pro-tip: Save your Canvas token somewhere safe, where you can find it again. For example, paste it into a simple text document and save it as `canvas_token.txt` in a sensible place in your folders. Once you leave the page where it's displayed, you cannot retrieve it again!
#'
#' To obtain the Canvas domain:
#'
#' * Navigate in your browser to your Canvas dashboard
#' * The Canvas domain is the whole uRL, typically `https://canvas.your_uni_domain`, e.g. `https://canvas.sussex.ac.uk`
#'
#' @return Sets .Renviron variables and runs [cnvs::canvas_setup()] to communicate with Canvas
#' @export
#'
set_renv_variables <- function(){
  ## Intro message
  message("This function will set up your access to Canvas via R.\nYou will need your Canvas access token and your uni's Canvas URL domain to proceed.")

  ## Get current .Renviron file
  scope = c("user", "project")
  path <- usethis:::scoped_path_r(scope, ".Renviron", envvar = "R_ENVIRON_USER")
  renv_lines <- readLines(path)

  ## Needs updating - throws an error if there is no .Renviron file
    # tryCatch(
    # {readLines(path)},
    # error = usethis::edit_r_environ()

  ## Check whether variable CANVAS_TOKEN already exists
  if(any(grepl("CANVAS_TOKEN", renv_lines) == TRUE)){
    message("You already have a 'CANVAS_TOKEN' in your .Renviron.")
    token_resp <- utils::menu(title = "Overwrite with new token?",
                              c("Yes, overwrite", "No, proceed without overwriting", "Abort"))
  } else {
    token_resp <- NA
  }

  ## If there IS a response but it's not 1 or 2, stop
  if(!(token_resp %in% 1:2) & !is.na(token_resp)) {
    stop("Canvas setup aborted", call. = FALSE)
    ## If response was overwrite, replace CANVAS_TOKEN line with new one
  } else if(token_resp %in% 1){
    renv_lines[grepl("CANVAS_TOKEN", renv_lines)] <- paste0("CANVAS_TOKEN = ",
                                                            readline(prompt = "Canvas token: "),
                                                            "\n")

    writeLines(renv_lines, path)
    ## If there was no previous CANVAS_TOKEN, create one
  } else if(is.na(token_resp)){
    renv_lines <- c(renv_lines,
                    paste0("CANVAS_TOKEN = ",
                           readline(prompt = "Canvas token: ")),
                    "\n")
    writeLines(renv_lines, path)
  }

  ## Check whether variable CANVAS_DOMAIN already exists
  if(any(grepl("CANVAS_DOMAIN", renv_lines) == TRUE)){
    message("You already have a 'CANVAS_DOMAIN' in your .Renviron.")
    domain_resp <- utils::menu(title = "Overwrite with new domain?",
                               c("Yes, overwrite", "No, proceed without overwriting", "Abort"))
  } else {
    domain_resp <- NA
  }

  ## If there IS a response but it's not 1 or 2, stop
  if(!(domain_resp %in% 1:2) & !is.na(domain_resp)) {
    stop("Canvas setup aborted", call. = FALSE)
    ## If response was overwrite, replace CANVAS_DOMAIN line with new one
  } else if(domain_resp %in% 1){
    renv_lines[grepl("CANVAS_DOMAIN", renv_lines)] <- paste0("CANVAS_DOMAIN = ",
                                                             readline(prompt = "Canvas domain URL: "),
                                                             "\n")

    writeLines(renv_lines, path)
    ## If there was no previous CANVAS_TOKEN, create one
  } else if(is.na(domain_resp)){
    renv_lines <- c(renv_lines,
                    paste0("CANVAS_DOMAIN = ",
                           readline(prompt = "Canvas domain URL: "),
                           "\n")
    )
    writeLines(renv_lines, path)
  }

  message("Renviron variables set successfully!
          To complete setup, restart your R session and run `cnvs::canvas_setup().")

}

#' Get Sussex term dates
#'
#' @param which Which table to retrieve, of the upcoming or current academic year with `current_year`, minimum services dates (Christmas, Easter, bank hols) with `min_service`, or the next academic year after the current/upcoming one with `next_year`.
#' @param teaching_only Whether to retrieve ONLY teaching start and end dates.
#'
#' @returns A tibble adapted from the Sussex Term Dates webpage
#' @export

get_sussex_dates <- function(which = c("current_year", "min_service", "next_year"),
                             teaching_only = FALSE){

  which <- switch(
    which,
    current_year = 1,
    min_service = 2,
    next_year = 3
  )

  session <- rvest::session("https://www.sussex.ac.uk/about/term-dates")

  page <- xml2::read_html(session)

  dates <- page |>
    rvest::html_nodes("table") |>
    rvest::html_table() |>
    magrittr::extract2(which)

  dates <- dplyr::rename_with(dates, ~c("term", "event", "date")) |>
    dplyr::mutate(event = tolower(event))

  if(teaching_only == TRUE){
    dates <- dates |>
      dplyr::filter(grepl("teaching", event) | grepl("(spring|summer) term", event)) |>
      dplyr::filter(event != "spring term begins")
  }

  return(dates)
}

#' Parse dates
#'
#' @param date Date to parse. Expects Weekday Day Month Year e.g. Fri 3 Oct 2025
#'
#' @returns Dates as date.
#' @export

parse_sussex_dates <- function(date){lubridate::parse_date_time(date, orders = "%a %d %b %Y")}


#' Create teaching calendar info
#'
#' @param which Which year to return, of the upcoming or current academic year with `current_year`, or the following one with `next_year`.
#'
#' @returns A list, two tibbles for Autumn and Spring terms, and a short text string stating after which week Easter holiday begins.
#' @export

create_teaching_calendar <- function(which){

  if(missing(which)){
    stop("You must indicate either current_year or next_year")
  }

  dates <- get_sussex_dates(which, teaching_only = TRUE)

  ## Autumn Term (no break)
  autumn_start <- dates |>
    dplyr::filter(grepl("One", term) & grepl("begins", event)) |>
    dplyr::pull(date) |>
    parse_sussex_dates() |>
    as.Date()

  autumn_weeks <- 11

  autumn_calendar <- tibble::tibble(
    term = "autumn",
    week = 1:autumn_weeks,
    week_start = seq.Date(from = autumn_start, by = "week", length.out = autumn_weeks),
    week_end = week_start + lubridate::days(4)
  )

  ## Spring Term (with break)

  spring_start <- dates |>
    dplyr::filter(grepl("Two", term) & grepl("teaching begins", event)) |>
    dplyr::pull(date) |>
    parse_sussex_dates() |>
    as.Date()

  easter_start <- dates |>
    dplyr::filter(grepl("Two", term) & grepl("spring term ends", event)) |>
    dplyr::pull(date) |>
    parse_sussex_dates() |>
    as.Date()

  spring_weeks <- 14

  spring_calendar <- tibble::tibble(
    term = "spring",
    week = 1:spring_weeks,
    week_start = seq.Date(from = spring_start, by = "week", length.out = spring_weeks),
    week_end = week_start + lubridate::days(4)
  )

  easter_week <- spring_calendar |>
    dplyr::filter(week_end == easter_start) |>
    dplyr::pull(week)

  break_weeks <- seq(from = easter_week + 1, length.out = 3)

  spring_calendar <- spring_calendar |>
    dplyr::filter(!week %in% break_weeks) |>
    dplyr::mutate(week = 1:autumn_weeks)

  term_calendars <- list(
    "autumn" = autumn_calendar,
    "spring" = spring_calendar,
    "spring_break" = paste0("This year, the spring holiday begins after Week ", easter_week, ".")
  )

  return(term_calendars)
}


