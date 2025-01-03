## Note that Canvas "modules" are called "units" at our Uni
## Because here a "module" is what American unis call a "course"

#' Return all the units ("modules") on a Canvas site
#'
#' @param module_id Canvas module ID number
#' @param search_term String to search for in the unit name
#' @param id_only Whether to return the unit ID only. Best used with a specific search term.
#'
#' @returns A tibble of all units matching the search term, if provided; or only ID code(s), if id_only is TRUE.
#' @export
#'
#' @examples
get_units <- function(module_id, search_term = NULL, id_only = FALSE){

  url <- paste0("https://canvas.sussex.ac.uk/api/v1/courses/", module_id, "/modules")

  args <- list(
    access_token = rcanvas:::check_token(),
    `include[]` = "items",
    `search_term` = search_term
  )

  units <- cnvs::get(url, args)

  # units <- rcanvas:::process_response(url, args) |>
  #   dplyr::bind_rows() |>
  #   dplyr::mutate(course_id = module_id)

  if(id_only){
    units <- units |> dplyr::pull(id)
  }

  return(units)
}

add_page_to_unit <- function(module_id, unit_search_term, page_search_term){

  unit_id <- cnvs::get_units(module_id, unit_search_term,
                           id_only = TRUE)

  pages <- cnvs::get_pages(module_id)

  page_url <- pages |>
    dplyr::filter(grepl(pattern = tolower(page_search_term),
                        x = tolower(title))) |>
    dplyr::pull(url)

  if(length(url) > 1){
    stop("Your page search returned more than one match. Use a more precise page search term.")
  } else if (length(url) < 1) {
    stop("Your page search didn't return any matches.")
  }

  url <- file.path(rcanvas:::canvas_url(), "courses", module_id, "modules", unit_id, "items")

  args <- list(
    access_token = rcanvas:::check_token(),
    `module_item[type]` = "Page",
    `module_item[page_url]` = page_url,
    `module_item[position]` = 2
  )

  rcanvas:::canvas_query(
    url, args, "POST")

}
