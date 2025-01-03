
#' Get all submissions for a particular assignment
#'
#' @param module_id A Canvas module ID number (NOT university module code).
#' @param type Either "quizzes" or "assignments".
#' @param type_id A Canvas assignment ID number.
#'
#' @returns A tibble of submission information for all submissions for all
#'   students.
#' @export
#'
#' @examples
#'

get_submissions <- function (module_id, type, type_id)
{
  if (!type %in% c("quizzes", "assignments"))
    stop("type must be 'quizzes' or 'assignments'")
  url <- sprintf("%scourses/%s/%s/%s/submissions", "https://canvas.sussex.ac.uk/api/v1/",
                 module_id, type, type_id)
  args <- list(access_token = rcanvas:::check_token(), per_page = 100)
  rcanvas:::process_response(url, args) |>  dplyr::bind_rows() |>  dplyr::mutate(course_id = module_id)
}

#' Calculate "best-of" quiz marks
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
calc_quiz_mark <- function(module_code,
                           academic_year,
                           quiz_title = "Worksheet",
                           max_score = 7){

  ## Input checks
  ### Checking module code input and attempting to search for module ID
  if (!stringr::str_detect(module_code, "^[A-Z]?[0-9]+[A-Z]?[0-9]+"))
    stop("Module code is not specified correctly. Please enter a module code formatted as C1234 or 123C4.")

  module_id <- try(cnvs::get_module_id(module_code), silent = TRUE)

  if(inherits(module_id, "try-error")){
    cnvs::canvas_setup()
    module_id <- cnvs::get_module_id(module_code)
  }

  ## Setting academic year
  if(missing(academic_year)){
    academic_year <- get_ac_year()
    message(paste("Using current year", academic_year, "if necessary"))
  }

  ## Get student info from Canvas
  students <- cnvs::get_students(course_id = module_id)

  #### Get quizzes from Canvas ####

  quizzes <- cnvs::get_all_quizzes(module_id)

  marked_quizzes <- quizzes |>
    dplyr::filter(
      grepl(pattern = quiz_title, x = title) & quiz_type == "assignment" & published
    )

  ## Get ids of specifically challengr quizzes
  ### NOTE TO SELF: CHECK THIS WITH AND
  quiz_ids <- marked_quizzes |>
    dplyr::pull(id)

  ## Download those submissions and collapse into one dataframe
  quiz_subs <- purrr::map(
    .x = quiz_ids,
    .f = ~cnvs::get_submissions(course_id, type = "quizzes", type_id = .x) |> magrittr::extract2(1)
  ) |>
    purrr::reduce(
      .f = bind_rows
    )

  ## Read in quiz titles so we know which week is which
  quiz_subs <- quiz_subs |>
    dplyr::left_join(marked_quizzes |> dplyr::select(id, title),
                     dplyr::join_by(quiz_id == id)
    )

  ## Check if each quiz has an entry for ALL students

  n_quizzes <- quiz_subs |>
    dplyr::group_by(title) |>
    dplyr::summarise(n = dplyr::n()) |>
    nrow()

  ## Looks like these are only submissions - so don't include non-submissions
  ## Hmm. Let's keep this in mind shall we

  ## Combine with student info

  quiz_scores <- students |>
    dplyr::left_join(quiz_subs |>
                       dplyr::select(user_id, title, kept_score),
                     by = dplyr::join_by(id == user_id))

  quiz_scores <- quiz_scores |>
    dplyr::mutate(
      title = ifelse(is.na(title),
                     title,
                     paste0("w", gsub(".* ([0-9]+) .*", "\\1", title))
      )
    )

  ## Already in long form. But not everyone has entries for every week
  ## Pivot to wide to fill in then back to long! Is there a better way?? oh well

  quiz_scores_full <- quiz_scores |>
    tidyr::pivot_wider(
      names_from = title,
      values_from = kept_score,
      values_fill = 0
    ) |>
    dplyr::select(-`NA`) |>
    tidyr::pivot_longer(
      cols = dplyr::starts_with("w"),
      names_to = "week",
      values_to = "score"
    )

  ## Rank and then calculate marks
  quiz_marks <- quiz_scores_full |>
    dplyr::group_by(cand_no) |>
    dplyr::arrange(desc(score), .by_group = TRUE) |>
    dplyr::mutate(
      rank = 1:dplyr::n()
    ) |>
    dplyr::slice_min(
      order_by = rank,
      n = n_quizzes - 2
    ) |>
    dplyr::summarise(
      quiz_mark = round(sum(score)/((n_quizzes-2)*max_score)*100)
    )

  ## Marks distribution
  quiz_dist <- quiz_marks |>
    ggplot2::ggplot(aes(quiz_mark)) +
    ggplot2::geom_histogram(
      fill = "darkcyan",
      colour = "black",
      binwidth = 1
    ) +
    ggplot2::labs(x = "Quiz Mark", y = "Count") +
    ggplot2::heme_bw()

  out <- list(
    "marks" = quiz_marks,
    "dist" = quiz_dist
  )

  return(out)
}
