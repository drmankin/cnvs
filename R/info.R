#' Get details of all modules
#'
#' Wrapper for `rcanvas` function [rcanvas::get_course_list()] with some extra
#' cleaning for convenience.
#'
#' @param academic_modules Boolean. Return only academic modules (removing those
#'   without a course ID code)? Defaults to `FALSE`.
#' @param academic_year Optional string to filter only specific academic
#'   year(s). Accepts the format e.g. "22/23" or a vector of the same.
#'
#' @returns A tibble of information about all the Canvas modules that are
#'   currently associated with the Canvas token set by [canvas_setup()].
#' @export

get_module_list <- function(academic_modules = FALSE, academic_year = NA_character_){
  module_list <- rcanvas::get_course_list() |>
    tibble::as_tibble()

  if(academic_modules == TRUE){
    module_list <- module_list |>
      dplyr::filter(!is.na(sis_course_id))
  }

  module_list <- module_list |>
    tidyr::separate_wider_delim(cols = sis_course_id,
                                names = c("module_code", "term", "start_year", "end_year"),
                                delim = "_",
                                too_few = "align_start",
                                too_many = "drop") |>
    dplyr::mutate(
      ## This is because some (small number of) sis_course_ids are not actually composed of the elements above
      ## So this turns their module_code into NA to prevent Problems(TM)
      module_code = ifelse(grepl(pattern = "^[A-Z]?[0-9]+[A-Z]?[0-9]+", x = module_code),
                           yes = module_code, no = NA),
      ac_year = dplyr::case_when(
        !is.na(module_code) ~ paste0(start_year, "/", end_year),
        TRUE ~ "continuous")
    )

  if(!all(is.na(academic_year))){
    module_list <- module_list |>
      dplyr::filter(ac_year %in% academic_year)
  }

  return(module_list)
}

#' Get a tibble of all users on a module
#'
#' Note that this is different from [cnvs::get_students()] as this gets all
#' USERS including convenors, observers, DTs etc
#'
#' @param module_id A Canvas module ID number (NOT university module code).
#'
#' @return A tibble of user information
#' @export

get_users <- function(module_id){
  url <- paste0(rcanvas:::canvas_url(), "/", paste("courses", module_id,
                                                   "users", sep = "/"))
  args <- list(per_page = 100)
  include <- rcanvas:::iter_args_list(NULL, "include[]")
  args <- c(args, include)
  dat <- rcanvas:::process_response(url, args)
  return(tibble::as_tibble(dat))
}


#' Get information about all students on a module
#'
#' @param module_id A Canvas module ID number (NOT university module code).
#' @param cand_no_pattern Regex pattern to filter valid candidate numbers
#'
#' @returns A tibble of student information
#' @export

get_students <- function(module_id, cand_no_pattern = "[0-9]{6}"){
  url <- paste0(rcanvas:::canvas_url(), "/", paste("courses", module_id,
                                                   "students", sep = "/"))
  args <- list(per_page = 100)
  include <- rcanvas:::iter_args_list(NULL, "include[]")
  args <- c(args, include)
  students <- rcanvas:::process_response(url, args)

  ## Old version - recover if otehr doesn't work
# get_students <- function(module_id){
#   students <- rcanvas:::canvas_query(
#     paste0("https://canvas.sussex.ac.uk/api/v1/courses/", module_id, "/students"),
#     list(per_page = 100)) |>
#     rcanvas:::paginate() |>
#     purrr::map(httr::content, "text") |>
#     purrr::map(jsonlite::fromJSON, flatten = TRUE) |>
#     dplyr::bind_rows()

  students <- students[!duplicated(students), ] |>
    dplyr::mutate(cand_no = gsub("Candidate No : ", "", sortable_name))

  students <- students |>
    dplyr::select(-created_at, -sortable_name, -short_name, -integration_id) |>
    ## Check cand_no is a 6-digit number
    ## Also removes alarming NA warning!
    dplyr::filter(grepl(cand_no_pattern, cand_no)) |>
    dplyr::mutate(cand_no = as.numeric(cand_no))

  return(tibble::as_tibble(students))
}

## Old version - "GET" just stopped working one day :(
# get_students <- function(module_id){
#   students <- cnvs::rcanvas_canvas_query(
#     paste0("https://canvas.sussex.ac.uk/api/v1/courses/", module_id, "/students"),
#     list(per_page = 100), "GET") |>
#     cnvs::rcanvas_paginate() |>
#     purrr::map(httr::content, "text") |>
#     purrr::map(jsonlite::fromJSON, flatten = TRUE) |>
#     dplyr::bind_rows()
#
#   students <- students[!duplicated(students), ] |>
#     dplyr::mutate(cand_no = gsub("Candidate No : ", "", sortable_name))
#
#   students <- students |>
#     dplyr::select(-created_at, -sortable_name, -short_name, -integration_id, -pronouns) |>
#     dplyr::mutate(cand_no = as.numeric(cand_no)) |>
#     dplyr::filter(!is.na(cand_no))
# }

#' Create term information
#'
#' Make a tibble of information about term dates
#'
#' @param term_start_date First day of term (the first Monday)
#' @param term_end_date Last day of term (the last Friday)
#' @param has_break_weeks Logical. Does the module have break weeks (e.g. spring break)?
#' @param break_weeks Numerical vector indicating weeks out of the total number
#'   of weeks that the term spans are break weeks. E.g. for an 11-week term, with three weeks of spring break, the
#'   total number of weeks is 14: 10 weeks of term, three weeks break, and one
#'   final week of term. In this case the input would be 11:13.
#'
#' @returns A tibble of weeks and start and end dates
#' @export

create_term_info <- function(term_start_date,
                             term_end_date,
                             has_break_weeks = TRUE, break_weeks = 11:13){

  # Calculate the total weeks spanned by the term
  total_weeks <- suppressMessages(difftime(term_end_date, term_start_date, units = "weeks") |> ceiling())

  # Generate an initial tibble
  term_times <- tibble::tibble(
    week = 1:total_weeks,
    week_start = seq.Date(as.Date(term_start_date), by = "week", length.out = total_weeks),
    ## This nonsense is to get the sequence of days right counting from the last lock date!
    week_end = seq.Date(as.Date(term_end_date) - lubridate::weeks(total_weeks - 1),
                        by = "week", length.out = total_weeks)

  )

  # Make sure datetimes are right
  term_times <- term_times |>
    dplyr::mutate(
      dplyr::across(
        dplyr::contains("term"),
        ## keeps the timezone right but converts to character
        ~ lubridate::format_ISO8601(lubridate::as_datetime(.x), usetz = FALSE, precision = "ymdhms"))
    )

  # Remove break weeks (if there are any)
  if (has_break_weeks){
    term_times <- term_times |>
      dplyr::filter(!week %in% break_weeks) |>
      dplyr::mutate(week = 1:dplyr::n())
  }

  return(term_times)

}
