#' Get list of pages for a module
#'
#' @param module_id The Canvas ID of the module to update (use [get_module_id()])
#' @export

get_pages <- function(module_id){

  rcanvas:::process_response(
    url = paste0(rcanvas:::canvas_url(), file.path("/courses", module_id, "pages")),
    args = list(
      sort = "created_at"
    )
  )
}


#' Post or update a Canvas page from a Quarto document
#'
#' @param module_id The Canvas ID of the module to update (use [get_module_id()])
#' @param file_path Location of a .qmd Quarto file to post
#' @param syllabus  Boolean. Whether the page to be uploaded is the module syllabus (`TRUE`) or not (`FALSE`, default)
#' @param page_id ID or URL of an existing Canvas page. If left as "" (the default) will instead create a new page
#' @param page_title Optional. Custom page title, if different from the title of the Quarto document
#' @param published Publish the page?
#' @param front_page Set as the module front page?
#'
#' @returns Response from Canvas
#' @export
#'

quarto_page <- function(module_id, file_path, syllabus = FALSE, page_id = "", page_title,
                        published = TRUE, front_page = FALSE){

  # get out the file name
  # file_name <- gsub(".*/([^/]*?)\\.qmd", "\\1", path)

  if(!grepl(".qmd$", file_path)){
    stop("The file location does not contain a Quarto document.",
         call. = FALSE)
  }

  ## Render the document
  quarto::quarto_render(file_path)

  ## Read the html
  page_html <- readLines(gsub("qmd", "html", file_path))

  ## Determine the title
  if(missing(page_title) & syllabus == FALSE){
    page_title <- stringr::str_extract(page_html, "<title>(.*)</title>", group = 1)
    ## Because na.omit() decided to ...do very strange things??
    page_title <- page_title[which(!is.na(page_title))]
  }

  ## Check whether page with same title already exists
  all_pages <- get_pages(module_id)

  if(nrow(all_pages) > 0){
    ext_page_id <- all_pages |>
      dplyr::filter(title == page_title) |>
      dplyr::pull(page_id)
  } else {
    ## In case of no pages
    ext_page_id <- ""
  }

  ## If a page ID already exists, use that one. REQUIRES that all pages are called different things!
  if(length(ext_page_id) != 0) {
    page_id <- ext_page_id
  }

  ## Delete title from HTML
  page_html <- gsub("<h1.*</h1>", "", page_html)

  ## Locate start and end of body
  body_start <- grep("<body", page_html)
  body_end <- grep("</body>", page_html)

  ## Extract body only
  page_body <- page_html[body_start:body_end] |> paste0(collapse = "")

  ## Send to Canvas
  if (syllabus == TRUE){
    rcanvas:::canvas_query(
      url = paste0(rcanvas:::canvas_url(), file.path("/courses", module_id)),
      args = list(
        `course[syllabus_body]` = page_body
      ),
      "PUT"
    )
  } else {
    rcanvas:::canvas_query(
    url = paste0(rcanvas:::canvas_url(), file.path("/courses", module_id, "pages", page_id)),
    args = list(
      `wiki_page[title]` = page_title,
      `wiki_page[body]` = page_body,
      `wiki_page[published]` = published,
      `wiki_page[front_page]` = front_page
    ),
    ifelse(page_id == "", "POST", "PUT")
  )
  }
}
