#' Create an in-text link to various Canvas destinations
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
#' @param type What kind of link to produce - "markdown" or "html"
#' @param web_url URL to use for "website"
#' @param url The end of a Canvas URL - the first bit will be added
#'   automatically up to ".../courses/module_id/".
#'
#' @return An embedded link text string
#' @export

embed_link <- function(link_text, dest, search_term, type, web_url = "https://r-training.netlify.app", url = NULL){

  if(!missing(search_term)){
    module_id <- cnvs::set_module_id(search_term)
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

#' Calculate the date of the next (or previous) weekday from a date
#'
#' Adapted from a [stack overflow solution by TimTeaFan](https://stackoverflow.com/questions/57893554/get-next-wednesday-date-after-a-date-with-r)
#'
#' @param date A datetime
#' @param weekday A day of the week as a number, with 1 = Sunday and 7 = Saturday, OR as a string with a weekday name
#' @param direction Either "previous" or "next"
#'
#' @returns The date of the next [weekday] before or after the given [date]
#' @export

find_weekday <- function(date, weekday, direction = c("previous", "next")){

  weekdays <- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday") |> tolower()

  if(is.character(weekday)){
    this_weekday <- which(weekdays == tolower(weekday))
  }

  ##calculate the next date (in the future)
  next_date <- date + (seq(this_weekday - 1, length = 7) %% 7 + 1L)[8 - lubridate::wday(date)]

  if(direction == "next"){
    return(next_date)
  } else if (direction == "previous") {
    return(next_date - lubridate::weeks(1))
  } else {
    stop("Please provide either 'previous' or 'next' for direction")
  }

}

#' Generate a link to a Posit list
#'
#' @param list_number The numerical ID of the list
#' @param workspace_number The numerical ID of the workspace that the list exists in
#'
#' @returns A URL to a list on a Posit workspace as a text string
#' @export

create_posit_link <- function(list_number, workspace_number){
  file.path("https://posit.cloud/spaces", workspace_number,
            "content/lists", list_number)
}


#' Create a markdown-style link for types of content
#'
#' Helper function to build in-text links for use on Canvas pages.
#' Specifically designed to work with our module website and reference docs.
#'
#' Assumes that the final (non-Posit) URL will be in the format:
#' `url`/`content`s/`content`_`week` (with `week` given a leading 0 if needed)
#' So if `content` is `skills_lab` and the `week` is 5, the resulting URL will be:
#' `url`/skills_labs/skills_lab_05
#' Look I'm sorry if that's not how you roll, this function works for me!
#'
#' @param content Type of module content
#' @param week_number Week of term
#' @param publish Boolean. If TRUE the text will be linked, otherwise will just return formatted text with no link
#' @param posit_link Whether the link should go to the Posit workspace. Calls [cnvs::create_posit_link()]
#' @param list_number The numerical ID of the Posit list. Only needed if `posit_link = TRUE`.
#' @param url Stem URL to build on for non-Posit links
#'
#' @returns A text string
#' @export

create_link <- function(content, week_number, publish = FALSE,
                        posit_link = FALSE, list_number,
                        url = "https://analysingdata.pages.dev/content"){
##Convert digits to "01" format numbers
week_number_text <- ifelse(week_number < 10, paste0("0", week_number), week_number)

## Add an "s" because the website folders have one
content_folder <- paste0(content, "s")

## Generate the text that will appear on the page using the content type
link_text <- gsub("_", " ", content) |>
  stringr::str_to_title() |>
  paste0(" ", week_number_text)

## Generate the output
if(publish & !posit_link) {
  ## If no Posit link, concatenate the link
  paste0("[", link_text, "](",
         file.path(url, content_folder,
                   paste0(content, "_", week_number_text)), ")")
} else if (publish & posit_link){
  ## Otherwise use the Posit link
  paste0("[", link_text, "](", create_posit_link(list_number), ")")
  ## Otherwise otherwise (ie when publish is false) just return the text
} else {
  link_text
}
}

#' Generate a linked bullet point
#'
#' @param ... Arguments passed to [cnvs::create_link()]
#'
#' @returns A single text string in the format "- `linked text`" with two following newlines
#' @export

create_bullet_link <- function(...){
  paste0("- ", create_link(...), "\n\n")
}


