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

  cnvs::rcanvas_canvas_query(
    url = paste0(cnvs::rcanvas_canvas_url(), file.path("/courses", module_id)),
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

  args <- c(list(access_token = cnvs::rcanvas_check_token(), per_page = 100),
            args)

  resp <- cnvs::rcanvas_process_response(url, args) |>
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
         "/api/vi")
}

#' Send a query to Canvas
#'
#' Copied from the unexported `canvas_query()` function from [the {rcanvas} package](https://github.com/daranzolin/rcanvas).
#' See [cnvs::rcanvas_canvas_url()] for generating the API URL
#'
#' @param urlx A Canvas API URL
#' @param args Arguments to append to the API request
#' @param type Type of query, of "GET", "POST", "PUT"
#'
#' @returns Response from Canvas
#' @export

rcanvas_canvas_query <- function(urlx, args = NULL, type = "GET") {
  args <- cnvs::rcanvas_sc(args)
  resp_fun_args <- list(url = urlx, httr::user_agent("rcanvas - https://github.com/daranzolin/rcanvas"),
                        httr::add_headers(Authorization = paste("Bearer", cnvs::rcanvas_check_token())))
  if (type %in% c("POST", "PUT"))
    resp_fun_args$body = args
  else resp_fun_args$query = args
  resp <- do.call(type, resp_fun_args)
  httr::stop_for_status(resp)
  resp
}

#' Check Canvas token
#'
#' Copied from the unexported `check_token()` function from [the {rcanvas} package](https://github.com/daranzolin/rcanvas).
#'
#' @returns The user's Canvas token
#' @export

rcanvas_check_token <- function ()
{
  token <- keyring::key_get("rcanvas_CANVAS_API_TOKEN")
  if (identical(token, "")) {
    stop("Please set your Canvas API token with set_canvas_token.",
         call. = FALSE)
  }
  token
}

#' Get and process a response from Canvas
#'
#' Copied from the unexported `process_response()` function from [the {rcanvas} package](https://github.com/daranzolin/rcanvas).
#'
#' @param url A Canvas API URL
#' @param args Arguments to append to the API request
#'
#' @returns A dataset containing the requested information
#' @export

rcanvas_process_response <- function (url, args)
  {
    resp <- cnvs::rcanvas_canvas_query(url, args, "GET")
    d <- cnvs::rcanvas_paginate(resp) |> purrr::map(httr::content, "text") |>
      purrr::map(jsonlite::fromJSON, flatten = TRUE)
    dplyr::bind_rows(d)
}

#' Create a list of arguments for an API query
#'
#' Copied from the unexported `iter_args_list()` function from [the {rcanvas} package](https://github.com/daranzolin/rcanvas).
#'
#' @param x A vector of values
#' @param label Labels for the values of `x`
#'
#' @returns A labeled (named) list
#' @export

rcanvas_iter_args_list <- function (x, label)
{
  ln <- list()
  for (i in seq_along(x)) {
    ln[[i]] <- x[i]
    names(ln)[[i]] <- label
  }
  ln
}

#' Paginate a response from Canvas
#'
#' Copied from the unexported `paginate()` function from [the {rcanvas} package](https://github.com/daranzolin/rcanvas).
#'
#' @param x Response from Canvas from e.g. [cnvs::rcanvas_canvas_query()]
#' @param showProgress Show progress bar
#'
#' @returns Paginated responses (probably)
#' @export

rcanvas_paginate <- function (x, showProgress = FALSE)
{
  first_response <- list(x)
  stopifnot(httr::status_code(x) == 200)
  pages <- httr::headers(x)$link
  if (is.null(pages))
    return(first_response)
  should_continue <- TRUE
  if (cnvs::rcanvas_has_rel(pages, "last")) {
    last_page <- cnvs::rcanvas_get_page(x, "last")
    n_pages <- readr::parse_number(stringr::str_extract(last_page,
                                                        "page=[0-9]{1,}"))
    if (n_pages == 1) {
      return(first_response)
    }
    pages <- cnvs::rcanvas_increment_pages(last_page, 2:n_pages)
    if (showProgress) {
      bar = utils::txtProgressBar(max = n_pages, style = 3)
    }
    queryfunc = function(...) {
      if (showProgress)
        bar$up(bar$getVal() + 1)
      cnvs::rcanvas_canvas_query(...)
    }
    responses <- pages |> purrr::map(queryfunc, args = list(access_token = cnvs::rcanvas_check_token()))
    responses <- c(first_response, responses)
    return(responses)
  }
  else {
    if (cnvs::rcanvas_has_rel(httr::headers(x)$link, "next")) {
      pages[[1]] <- cnvs::rcanvas_get_page(x, "current")
      inc <- 2
      while (should_continue) {
        page_temp <- cnvs::rcanvas_get_page(x, "next")
        pages[[inc]] <- page_temp
        x <- cnvs::rcanvas_canvas_query(page_temp, args = list(access_token = cnvs::rcanvas_check_token()),
                          type = "HEAD")
        if (!cnvs::rcanvas_has_rel(httr::headers(x)$link, "next")) {
          should_continue <- FALSE
        }
        else {
          inc <- inc + 1
        }
      }
      responses <- pages |> purrr::map(cnvs::rcanvas_canvas_query, args = list(access_token = cnvs::rcanvas_check_token()))
    }
  }
}



#' Check "rel" property (maybe?)
#'
#' Copied from the unexported `has_rel()` function from [the {rcanvas} package](https://github.com/daranzolin/rcanvas).
#' Not 100% on what this one does but seems important!
#'
#' @param x Input
#' @param rel rel value to search for in `x`
#'
#' @returns A Boolean
#' @export

rcanvas_has_rel <- function(x, rel){
  stopifnot(!is.null(rel))
  any(grepl(paste0("rel=\"", rel, "\""), x))
}

#' Discard NULLs
#'
#' Copied from the unexported `sc()` function from [the {rcanvas} package](https://github.com/daranzolin/rcanvas).
#'
#' @param x A list or vector
#'
#' @returns `x` with any NULL values removed
#' @export
rcanvas_sc <- function(x){
  purrr::discard(x, is.null)
}

#' Get a page from a response
#'
#' Copied from the unexported `get_page()` function from [the {rcanvas} package](https://github.com/daranzolin/rcanvas).
#'
#' @param resp Response from Canvas
#' @param page Which page to get
#'
#' @returns A URL
#' @export

rcanvas_get_page <- function (resp, page)
{
  pages <- resp$headers$link
  url_pattern <- "http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+"
  pages <- stringr::str_split(pages, ",")[[1]]
  url <- stringr::str_subset(pages, page)
  url <- stringr::str_extract(url, url_pattern)
  url <- stringr::str_replace_all(url, "[<>;]", "")
  return(url)
}

#' Increment pages
#'
#' Copied from the unexported `increment_pages()` function from [the {rcanvas} package](https://github.com/daranzolin/rcanvas).
#'
#' @param base_url A URL
#' @param n_pages Number of pages
#'
#' @returns URL with incremented pages (probably)
#' @export

rcanvas_increment_pages <- function (base_url, n_pages)
{
  stringr::str_replace(base_url, "([\\?&])(page=[0-9a-zA-Z]{1,})",
                       sprintf("\\1page=%s", n_pages))
}
