#' Get list of pages for a module
#'
#' @param module_id The Canvas ID of the module to update (use [get_module_id()])
#' @export

get_anncs <- function(module_id){

  rcanvas:::process_response(
    url = paste0(rcanvas:::canvas_url(), file.path("/courses", module_id, "discussion_topics")),
    args = list(
      only_announcements = TRUE
    )
  )
}

#' Post or update an announcement
#'
#' This function will create a new announcement if none currently exists with
#' the same title, and will update an existing announcement if the title is the
#' same. So, if you want announcements to be separated (and not overwritten), you
#' MUST NOT duplicate titles!
#'
#' @param module_id The Canvas ID of the module to update (use
#'   [cnvs::get_module_id()])
#' @param file_path Location of a .qmd Quarto file to post
#' @param page_title Optional. Custom page title, if different from the title of
#'   the Quarto document
#' @param post_at Optional. Default is NULL which will post immediately. To
#'   delay posting, provide a datetime in local time when the announcement
#'   should be posted. Will be converted to UTC so Canvas doesn't mess it up.
#'
#' @return Response from Canvas
#' @export
#'
#' @examples
#' \dontrun{quarto_annc("123C4", here::here("announcement.qmd"))}

quarto_annc <- function(module_id, file_path, page_title, post_at = NULL){

  if(!grepl(".qmd$", file_path)){
    stop("The file location does not contain a Quarto document.",
         call. = FALSE)
  }

  ## Render the document
  quarto::quarto_render(file_path)

  ## Read the html
  page_html <- readLines(gsub("qmd", "html", file_path))

  ## Determine the title
  if(missing(page_title)){
    page_title <- stringr::str_extract(page_html, "<title>(.*)</title>", group = 1)
    ## Because na.omit() decided to ...do very strange things??
    page_title <- page_title[which(!is.na(page_title))]
  }

  ## Check whether page with same title already exists
  ### Return a table of all existing announcements
  all_anncs <- get_anncs(module_id)

  if(nrow(all_anncs) > 0){
    ext_annc_id <- all_anncs |>
      dplyr::filter(title == page_title) |>
      dplyr::pull(id)
  }

  ## If a announcement ID already exists, use that one. REQUIRES that all announcements are called different things!
  if(length(ext_annc_id) != 0) {
    annc_id <- ext_annc_id
  } else {
    annc_id <- ""
  }

  ## Delete title from HTML
  page_html <- gsub("<h1.*</h1>", "", page_html)

  ## Locate start and end of body
  body_start <- grep("<body", page_html)
  body_end <- grep("</body>", page_html)

  ## Extract body only
  page_body <- page_html[body_start:body_end] |> paste0(collapse = "")

  ## Convert time to UTC so Canvas won't mess it up
  if(!is.null(post_at)){
    post_at <- format.POSIXct(post_at, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  }

  ## Send to Canvas

rcanvas:::canvas_query(
  url = paste0(rcanvas:::canvas_url(), file.path("/courses", module_id, "discussion_topics", annc_id)),
  args = list(
    `title` = page_title,
    `message` = page_body,
    `delayed_post_at` = post_at,
    `is_announcement` = TRUE
  ),
  ifelse(annc_id == "", "POST", "PUT")
)
}

#' Schedule an announcement from the document name
#'
#' This function assumes that you have a folder of announcement .qmd files whose
#' filenames are the date and time they should be posted in a format readable by
#' [lubridate::ymd_hm()], i.e. 202308310900 for 9:00 on August 31st 2023. This
#' function will find the next closest announcement to the current datetime and
#' post it to Canvas delayed until the time indicated in its filename.
#'
#' @param module_id The Canvas ID of the module to update (use
#'   [cnvs::get_module_id()])
#' @param annc_path Path to a folder containing announcements written in qmd files
#' @param annc_title Title to give the announcement on Canvas, if different than the title of the document
#'
#' @return Response from Canvas
#' @export
#'
#' @examples
#' \dontrun{schedule_annc("123C4")}

schedule_annc <- function(module_id, annc_path = here::here("announcements"), annc_title){

  if(!any(grepl(".qmd$", list.files(annc_path)))){
    stop("The file location does not contain a Quarto document.",
         call. = FALSE)
  }

  annc_qmds <- list.files(annc_path, pattern = ".qmd$")

  ## Calculate the time difference between now and each filename
  annc_diff <- lubridate::ymd_hm(annc_qmds, tz = Sys.timezone(), quiet = TRUE) - lubridate::now()

  ## Turn NAs and past dates into Inf
  annc_diff[which(is.na(annc_diff))] <- Inf
  annc_diff[which(annc_diff < 0)] <- Inf

  ## Identify the next announcement
  next_annc <- annc_qmds[which.min(annc_diff)]

  post_at <- lubridate::ymd_hm(next_annc, tz = Sys.timezone())

  cnvs::quarto_annc(module_id, file_path = file.path(annc_path, next_annc), page_title = annc_title, post_at = post_at)
}
