#' Get a spreadsheet of all "worksheet" quiz marks
#'
#' @param module_code A Sussex module code, e.g. "C8891".
#' @param academic_year A string containing the academic year for the desired
#'   module as e.g. "22/23"; defaults to the current year as calculated by
#'   [cnvs::get_ac_year()]. Does not need to be specified for
#'   ongoing/non-academic modules
#' @param quiz_title Common element of the names of all quizzes to be used in
#'   the calculation. Defaults to "Worksheet".
#' @param max_score Maximum possible score for the quizzes. Defaults to 7.
#'
#' @returns A list with two elements: $marks, a tibble of quiz marks for all
#'   students, and $dist, a histogram of the distribution of those marks
#' @export
#'
#' @examples
# get_quiz_marks <- function(module_code,
#                            academic_year,
#                            quiz_title = "Worksheet"){
#
#   ## Input checks
#   ### Checking module code input and attempting to search for module ID
#   if (!stringr::str_detect(module_code, "^[A-Z]?[0-9]+[A-Z]?[0-9]+")){
#     stop("Module code is not specified correctly. Please enter a module code formatted as C1234 or 123C4.")
#   }
#
#   ## Setting academic year
#   if(missing(academic_year)){
#     academic_year <- cnvs::get_ac_year()
#     message(paste("Using current year", academic_year, "if necessary"))
#   }
#
#   module_id <- try(cnvs::get_module_id(module_code, academic_year = academic_year), silent = TRUE)
#
#   if(inherits(module_id, "try-error")){
#     cnvs::canvas_setup()
#     module_id <- cnvs::get_module_id(module_code, academic_year = academic_year)
#   }
#
#   ## Get student info from Canvas
#   students <- cnvs::get_students(module_id)
#
#   #### Get quizzes from Canvas ####
#
#   quizzes <- cnvs::get_all_quizzes(module_id)
#
#   marked_quizzes <- quizzes |>
#     dplyr::filter(
#       grepl(pattern = quiz_title, x = title) & quiz_type == "assignment" & published
#     )
#
#   ## Get vector of quiz IDs
#   quiz_ids <- marked_quizzes |>
#     dplyr::pull(id)
#
#   ## Download those submissions and collapse into one dataframe
#   quiz_subs <- purrr::map(
#     .x = quiz_ids,
#     .f = ~cnvs::get_submissions(module_id, type = "quizzes", type_id = .x) |> magrittr::extract2(1)
#   ) |>
#     purrr::reduce(
#       .f = dplyr::bind_rows
#     )
#
#   ## Read in quiz titles so we know which week is which
#   quiz_subs <- quiz_subs |>
#     dplyr::left_join(marked_quizzes |> dplyr::select(id, title),
#                      dplyr::join_by(quiz_id == id)
#     )
#
#   ## Combine with student info
#
#   quiz_scores <- students |>
#     dplyr::left_join(quiz_subs |>
#                        dplyr::select(user_id, title, kept_score, attempt),
#                      by = dplyr::join_by(id == user_id))
#
#   quiz_scores <- quiz_scores |>
#     dplyr::mutate(
#       title = ifelse(is.na(title),
#                      title,
#                      paste0("w", gsub(".* ([0-9]+) .*", "\\1", title))
#       )
#     )
#
#   ## Little pivot to fill in missing weeks with 0s
#   ## Slightly complicated by keeping "attempt" as well
#
#   quiz_scores_full <- quiz_scores |>
#     tidyr::pivot_wider(
#       names_from = title,
#       values_from = c(kept_score, attempt),
#       values_fill = 0
#     ) |>
#     dplyr::select(!dplyr::contains("NA")) |>
#     tidyr::pivot_longer(
#       cols = dplyr::contains("_w"),
#       names_to = c(".value", "week"),
#       names_pattern = "(.*)_(.*)"
#     )
#
#   return(quiz_scores_full)
#
# }

### Extra stuff from the old get_quiz_sub_info

## HERE - CAN'T RETAIN ALL ROWS

## Read in quiz titles so we know which week is which
# quiz_subs <- marked_quizzes |> dplyr::select(id, name, assign_id) |>
#   dplyr::full_join(quiz_subs,
#                    dplyr::join_by(id == quiz_id)
#   )
#
# dplyr::full_join(students, marked_quizzes, dplyr::join_by(id == user_id)) |> View()


# quiz_scores <- students |>
#   dplyr::full_join(quiz_subs |>
#                      dplyr::select(user_id, title = name, quiz_id, assign_id, kept_score, attempt, workflow_state),
#                    by = dplyr::join_by(id == user_id))
#
# quiz_scores |>
#   dplyr::right_join(marked_quizzes |> dplyr::select(id, name, assign_id),
#                     dplyr::join_by(title == name)) |> View()


# ## Little pivot to fill in missing weeks with 0s
# ## Slightly complicated by keeping "attempt" as well
#
# if(quiz_title == "Worksheet"){
# quiz_scores_full <- quiz_scores |>
#   tidyr::pivot_wider(
#     names_from = title,
#     values_from = c(kept_score, attempt),
#     values_fill = 0
#   ) |>
#   dplyr::select(!dplyr::contains("NA")) |>
#   tidyr::pivot_longer(
#     cols = dplyr::contains("_w"),
#     names_to = c(".value", "week"),
#     names_pattern = "(.*)_(.*)"
#   )
# } else if(grepl("Learning Log", quiz_title)){
#   quiz_scores_full <- quiz_scores |>
#     dplyr::select(-attempt) |>
#     tidyr::pivot_wider(
#       id_cols = id:assign_id,
#       names_from = month,
#       values_from = c(kept_score, workflow_state),
#       values_fill = NA
#     ) |>
#     tidyr::pivot_longer(
#       cols = dplyr::contains(month.name) & -dplyr::contains("id"),
#       names_to = c(".value", "month"),
#       names_pattern = "(.*)_([A-Z]{1}.*)",
#       #values_drop_na = TRUE
#     )
# }
#
