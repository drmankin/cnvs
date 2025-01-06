#' Setup Canvas for API use
#'
#' Wrapper for two `rcanvas` functions: [rcanvas::set_canvas_token()] and
#' [rcanvas::set_canvas_domain()]. Calls [Sys.getenv()] for setting the token,
#' so this must be set up ahead of time (see Details).
#'
#' To use this function, you must obtain a Canvas token and set it as an
#' environmental variable.
#'
#' To obtain a Canvas token:
#'   * Log into Canvas.
#'   * On the left bar, click Account, then Settings.
#'   * Click New Access Token and follow the instructions.
#'
#' Save your Canvas token somewhere safe, where you can find it again. Then,
#' open your Renviron file (by using e.g. [usethis::edit_r_environ()]) and
#' create a new variable called  called `CANVAS_TOKEN` with your Canvas token in
#' quotes, making sure to keep the last line of the file empty. Save the file,
#' close it, and restart R (Session > Restart R) for the changes to take effect.
#'
#' @param domain A text string URL of the homepage of your university's Canvas
#'   site. Default is for University of Sussex.
#'
#' @returns Sets the Canvas token and domain.
#' @export

canvas_setup <- function(domain = "https://canvas.sussex.ac.uk"){
  if (Sys.getenv("CANVAS_TOKEN") == "")
    stop("You have not successfully created an environmental CANVAS_TOKEN variable.")
  rcanvas::set_canvas_token(Sys.getenv("CANVAS_TOKEN"))
  rcanvas::set_canvas_domain(domain)
}

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
#'
#' @returns Canvas module ID as a numeric vector.
#' @export

get_module_id <- function(search_term, academic_year){

  if(missing(academic_year)){
    academic_year <- cnvs::get_ac_year()
    message(paste("Using current year", academic_year, "if necessary"))
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
#' @examples
#' create_cnvs_folder("C:/Users/Canvas", "My Canvas Site")
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


#' Create a markdown-style in-text link to various Canvas destinations
#'
#' Convenience function to use as inline code to quickly insert embedded
#' hyperlinks
#'
#' @param link_text The text to be hyperlinked
#' @param dest The desination, one of:
#'  - "website", which will link to `web_url`
#'  - One of the normal Canvas elements (e.g. "Assignments", "Quizzes")
#'  - "Zoom" or "Panopto Recordings"
#'  - A text string that matches the machine-readable page name in the Canvas
#'  site returned by `search_term`
#'   If missing, `url` must be provided for a custom link.
#' @param search_term Passed to [get_module_id()] to return the module ID
#' @param web_url URL to use for "website"
#' @param url The end of a Canvas URL - the first bit will be added
#'   automatically up to ".../courses/module_id/".
#' @param type What kind of link to produce - "markdown" or "html"
#'
#' @return An embedded link text string
#' @export
#'
#' @examples
#'

embed_link <- function(link_text, dest, search_term, type, url = NULL, web_url = "https://r-training.netlify.app"){

  if(!missing(search_term)){
    module_id <- get_module_id(search_term)
  }

  cnvs_dest <- c("Home", "Announcements", "Assignments", "Discussions", "Grades", "People", "Pages", "Files", "Syllabus", "Quizzes", "Units", "Collaborations")
  cnvs_links <- c("", "announcements", "assignments", "discussion_topics", "grades", "users", "wiki", "files", "assignments/syllabus", "quizzes", "modules", "collaborations")

  all_dest <- purrr::set_names(cnvs_links, nm = cnvs_dest)

  if (type == "markdown"){
    if(missing(dest) & !is.null(url)){
      paste0("[", link_text, "](", paste0("https://canvas.sussex.ac.uk/courses/", module_id, "/", url),")")
    } else if (dest == "website"){
      paste0("[", link_text, "](", web_url, ")")
    } else if(dest %in% cnvs_dest){
      paste0("[", link_text, "](", paste0("https://canvas.sussex.ac.uk/courses/",
                                          module_id, "/", all_dest[names(all_dest) == dest], ")"))
    } else if(dest %in% c("Zoom", "Panopto Recordings")) {
      paste0("[", link_text, "](", paste0("https://canvas.sussex.ac.uk/courses/",
                                          module_id, "/external_tools/", switch(dest,
                                                                                Zoom = 5351,
                                                                                `Panopto Recordings` = 3491)), ")")
    } else {
      paste0("[", link_text, "](", paste0("https://canvas.sussex.ac.uk/courses/",
                                          module_id, "/pages/", dest), ")")
    }
  }

  if (type == "html"){
    if(missing(dest) & !is.null(url)){
      paste0("<a href='", paste0("https://canvas.sussex.ac.uk/courses/", module_id, "/", url), "'>", link_text, "</a>")
    } else if (dest == "website"){
      paste0("<a href='", web_url, "'>", link_text, "</a>")
    } else if(dest %in% cnvs_dest){
      paste0("<a href='", paste0("https://canvas.sussex.ac.uk/courses/",
                                 module_id, "/", all_dest[names(all_dest) == dest], ")"),
             "'>", link_text, "</a>")
    } else if(dest %in% c("Zoom", "Panopto Recordings")) {
      paste0("<a href='", paste0("https://canvas.sussex.ac.uk/courses/",
                                 module_id, "/external_tools/", switch(dest,
                                                                       Zoom = 5351,
                                                                       `Panopto Recordings` = 3491)),
             "'>", link_text, "</a>")
    } else {
      paste0("<a href='", paste0("https://canvas.sussex.ac.uk/courses/",
                                 module_id, "/pages/", dest), "'>", link_text, "</a>")
    }
  }
}



#' Publish (offer) a Canvas module
#'
#' @param search_term Passed to [get_module_id()] to return the module ID
#'
#' @return Reply from Canvas
#' @export
#'
#' @examples
#'

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

#' Get a tibble of all users on a module
#'
#' Note that this is different from [cnvs::get_students()] as this gets all
#' USERS including convenors, observers, DTs etc
#'
#' @param module_id A Canvas module ID number (NOT university module code).
#'
#' @return A tibble of user information
#' @export
#'
#' @examples
#'
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
#'
#' @returns A tibble of student information
#' @export
#'
#' @examples
#'

get_students <- function(module_id){
  students <- rcanvas:::canvas_query(
    paste0("https://canvas.sussex.ac.uk/api/v1/courses/", module_id, "/students"),
    list(per_page = 100), "GET") |>
    rcanvas:::paginate() |>
    purrr::map(httr::content, "text") |>
    purrr::map(jsonlite::fromJSON, flatten = TRUE) |>
    dplyr::bind_rows()

  students <- students[!duplicated(students), ] |>
    dplyr::mutate(cand_no = gsub("Candidate No : ", "", sortable_name))

  students <- students |>
    dplyr::select(-created_at, -sortable_name, -short_name, -integration_id, -pronouns) |>
    dplyr::mutate(cand_no = as.numeric(cand_no)) |>
    dplyr::filter(!is.na(cand_no))
}



#' Create a new section in a module
#'
#' @param module_id
#' @param section_name
#'
#' @returns A response from Canvas.
#' @export
#'
#' @examples
create_section <- function(module_id, section_name){
rcanvas:::canvas_query(
  url = paste0(rcanvas:::canvas_url(), file.path("/courses", module_id, "sections")),
  args = list(
    `course_section[name]` = section_name
  ),
  "POST"
)
}

#' Get all sections on a module
#'
#' @param module_id
#' @param search_term Optional string to search for in section names.
#'
#' @returns
#' @export
#'
#' @examples
get_sections <- function(module_id, search_term = ""){
  rcanvas:::canvas_query(
  url = paste0(rcanvas:::canvas_url(), file.path("/courses", module_id, "/sections")),
  args = list(
    `include[]` = "students",
    search_term = search_term,
    per_page = 100
  ),
  "GET"
  )|>
  rcanvas:::paginate() |>
  purrr::map(httr::content, "text") |>
  purrr::map(jsonlite::fromJSON, flatten = TRUE) |>
  dplyr::bind_rows()
}


#' Enroll a user in an (existing) section
#'
#' @param section_id ID of the section to enroll the user in
#' @param user_id ID of the user
#'
#' @returns
#' @export
#'
#' @examples
enroll_in_section <- function(section_id, user_id){
  rcanvas:::canvas_query(
  url = paste0(rcanvas:::canvas_url(), file.path("/sections", section_id, "enrollments")),
  args = list(
    `enrollment[user_id]` = user_id
  ),
  "POST"
)
}


#' Calculate the date of the next weekday
#'
#' Taken from a [stack overflow solution by TimTeaFan](https://stackoverflow.com/questions/57893554/get-next-wednesday-date-after-a-date-with-r)
#'
#' @param date A datetime
#' @param weekday A day of the week as a number, with 1 = Sunday and 7 = Saturday
#'
#' @returns The date of the next [weekday] after the given [date]
#' @export
#'
#' @examples
#'
next_weekday <- function(date, weekday){
  date + (seq(weekday - 1, length = 7) %% 7 + 1L)[8 - lubridate::wday(date)]
}
