
# Pages -------------------------------------------------------------------

library(devtools)
devtools::load_all()

canvas_setup()

# Get module ID number
#module_id <- get_module_id(search_term = "dissertation supervision", academic_year = "continuous")

module_id <- get_module_id(search_term = "sandbox")

# Get list of pages
pages <- rcanvas:::process_response(
  url = paste0(rcanvas:::canvas_url(), file.path("/courses", module_id, "pages")),
  args = list(
    sort = "created_at"
  )
)

# Create new page

rcanvas:::canvas_query(
  url = paste0(rcanvas:::canvas_url(), file.path("/courses", module_id, "pages")),
  args = list(
    `wiki_page[title]` = "Test Page",
    `wiki_page[body]` = "<h2>Test Content</h2> Does this work?"
  ),
  "POST"
)


# Delete

# delete_ids <- pages |>
#   dplyr::filter(
#     grepl("-3", title)
#   ) |>
#   dplyr::pull(page_id)
#
# resp <- rcanvas:::canvas_query(
#   url = paste0(rcanvas:::canvas_url(), file.path("/courses", module_id, "pages", delete_ids[1])),
#   "DELETE"
# )

docs_path <- "C:/Users/manki/Documents/r_projects/diss_sup"

rcanvas:::canvas_query(
  url = paste0(rcanvas:::canvas_url(), file.path("/courses", module_id)),
  args = list(
    `wiki_page[title]` = "Test Page",
    `wiki_page[body]` = "<h2>Test Content</h2> Does this work?"
  ),
  "POST"
)


## Upload multiple qmds from a directory
purrr::map(
  ## Only qmds
  .x = grep(".qmd", dir(file.path(folder_path, "pages")), value = TRUE),
  .f = ~ quarto_page(get_module_id("train"), file.path(folder_path, "pages", .x))
)



