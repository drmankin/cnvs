## HEY YOU YES YOU
## Thinking of making sections?? Wow cool bro I guess
## Just be aware that I used this code to add sections to an existing module
## And it fucked up the gradebook and required students to re-accept their module invites
## FOR SOME REASON
## So just think carefully about whether you actually need to do this or if you're just being Like This again


#' Create a new section in a module
#'
#' @param module_id Canvas module ID number
#' @param section_name Name to give the new section
#'
#' @returns A response from Canvas.
#' @export

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
#' @param module_id Canvas module ID number
#' @param search_term Optional string to search for in section names.
#'
#' @returns Tibble of information about sections
#' @export

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
#' @returns Response from Canvas
#' @export

enroll_in_section <- function(section_id, user_id){
  rcanvas:::canvas_query(
    url = paste0(rcanvas:::canvas_url(), file.path("/sections", section_id, "enrollments")),
    args = list(
      `enrollment[user_id]` = user_id
    ),
    "POST"
  )
}
