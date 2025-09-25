
#' Get all submissions for a particular assignment
#'
#' @param module_id A Canvas module ID number (NOT university module code).
#' @param type Either "quizzes" or "assignments".
#' @param type_id A Canvas assignment ID number.
#' @param add_args Any additional arguments to be added to the query
#'
#' @returns A tibble of submission information for all submissions for all
#'   students.
#' @export
#'
#' @examples
#' \dontrun{get_submissions("123C4", type = assignments, type_id = 123456)}

get_submissions <- function (module_id, type, type_id, add_args = list())
{
  if (!type %in% c("quizzes", "assignments"))
    stop("type must be 'quizzes' or 'assignments'")
  url <- sprintf("%scourses/%s/%s/%s/submissions", "https://canvas.sussex.ac.uk/api/v1/",
                 module_id, type, type_id)

    args <- append(list(access_token = rcanvas:::check_token(), per_page = 100),
                 add_args)

  rcanvas:::process_response(url, args)  |>
    dplyr::bind_rows() |>
    dplyr::mutate(course_id = module_id) |>
    tibble::as_tibble()

}

#' Get information about all quizzes in a module
#'
#' @param module_id A Canvas module ID number (NOT university module code).
#'
#' @returns A tibble of information about all quizzes on the module.
#' @export
#'
#' @examples
#' \dontrun{cnvs::get_all_quizzes(module_id = 12345)}
#'
get_all_quizzes <- function(module_id){
  url <- sprintf("%scourses/%s/quizzes", "https://canvas.sussex.ac.uk/api/v1/",
                 module_id)
  args <- list(access_token = rcanvas:::check_token(), per_page = 100)
  quizzes <- rcanvas:::process_response(url, args) |>
    dplyr::bind_rows() |>
    dplyr::mutate(course_id = module_id) |>
    tibble::as_tibble()
}

#' Get information about all questions in a quiz
#'
#' @param module_id A Canvas module ID number (NOT university module code).
#' @param quiz_id A Canvas quiz ID number
#'
#' @returns A tibble of information about each question in the quiz.
#' @export

get_quiz_questions <- function(module_id, quiz_id){
  url <- sprintf("%scourses/%s/quizzes/%s/questions", "https://canvas.sussex.ac.uk/api/v1/",
                 module_id, quiz_id)
  args <- list(access_token = rcanvas:::check_token(), per_page = 100)
  quizzes <- rcanvas:::process_response(url, args) |>
    dplyr::bind_rows() |>
    dplyr::mutate(course_id = module_id) |>
    tibble::as_tibble()
}


#' Get the responses submitted to a quiz
#'
#' This has ONLY been tested on getting the responses to quiz questions, and will likely work different for retrieving submitted files!
#'
#' @param module_id A Canvas module ID number (NOT university module code).
#' @param type_id A Canvas quiz ID number
#'
#' @returns A long-form tibble of all submitted responses to all questions by everyone who submitted the quiz.
#' @export

get_submitted_responses <- function(module_id, type_id){
  quiz_subs <- cnvs::get_submissions(module_id = module_id,
                                     type = "assignments",
                                     type_id = type_id,
                                     add_args = list(`include[]` = "submission_history")
  )

  all_subs <- quiz_subs |>
    dplyr::select(user_id, submission_history) |>
    tidyr::unnest(cols = submission_history,
                  names_repair = "minimal") |>
    tidyr::unnest(cols = submission_data,
                  names_repair = "minimal")

  return(all_subs)
}

#' Get information about the quizzes on a module
#'
#' @param module_id A Canvas module ID number (NOT university module code).
#'
#' @returns A tibble of quiz information include both assignment and ID numbers.
#'   Note that these are matched by the name/title of the quiz so THEY MUST BE
#'   UNIQUE.
#' @export

get_quiz_info <- function(module_id){
  quizzes <- cnvs::get_all_quizzes(module_id)

  assignments <- rcanvas::get_assignment_list(course_id = module_id) |>
    dplyr::select(assign_id = id, name)

  quiz_info <- assignments |>
    dplyr::left_join(quizzes, dplyr::join_by(name == title)) |>
    tibble::as_tibble()

  return(quiz_info)
}

#' Get a spreadsheet of full info and marks for all quizzes
#'
#' If you already have `module_id` stored, you can use it directly.
#' Otherwise, you can skip that step by providing `module_code` and
#' `academic_year`.
#'
#' @param module_id Canvas module ID code
#' @param module_code A Sussex module code, e.g. "C8891".
#' @param academic_year A string containing the academic year for the desired
#'   module as e.g. "22/23"; defaults to the current year as calculated by
#'   [cnvs::get_ac_year()]. Does not need to be specified for
#'   ongoing/non-academic modules
#' @param quiz_title Common element of the names of all quizzes to be used in
#'   the calculation. Defaults to "Worksheet".
#'
#' @returns A tibble of information about all submissions for all students for
#'   all quizzes on a module
#' @export

get_quiz_marks <- function(module_id, module_code, academic_year,
                           quiz_title = "Worksheet"){

  ## Input checks

  if(missing(module_id)){
    ### Checking module code input and attempting to search for module ID
    if (!stringr::str_detect(module_code, "^[A-Z]?[0-9]+[A-Z]?[0-9]+")){
      stop("Module code is not specified correctly. Please enter a module code formatted as C1234 or 123C4.")
    }

    ## Setting academic year
    if(missing(academic_year)){
      academic_year <- cnvs::get_ac_year()
      message(paste("Using current year", academic_year, "if necessary"))
    }

    module_id <- try(cnvs::get_module_id(module_code, academic_year = academic_year), silent = TRUE)

    if(inherits(module_id, "try-error")){
      cnvs::canvas_setup()
      module_id <- cnvs::get_module_id(module_code, academic_year = academic_year)
    }
  }

  ## Get student info from Canvas
  students <- cnvs::get_students(module_id)

  #### Get quizzes from Canvas ####
  quiz_info <- cnvs::get_quiz_info(module_id)
  #quizzes <- cnvs::get_all_quizzes(module_id)

  marked_quizzes <- quiz_info |>
    dplyr::filter(
      grepl(pattern = quiz_title, x = name) & quiz_type == "assignment" & published
    )

  ## Get vector of quiz IDs
  quiz_ids <- marked_quizzes |>
    dplyr::pull(id)

  ## Download those submissions and collapse into one dataframe
  quiz_subs <- purrr::map(
    .x = quiz_ids,
    .f = ~cnvs::get_submissions(module_id, type = "quizzes", type_id = .x) |> magrittr::extract2(1)
  ) |>
    purrr::reduce(
      .f = dplyr::bind_rows
    )

  ## Create an expanded list of all possible combos of student and quiz IDs
  ## Then join in submissions, quiz info, and student info

  quiz_scores <- tidyr::crossing(quiz_id = marked_quizzes$id, user_id = students$id) |>
    dplyr::left_join(quiz_subs |>
                       dplyr::select(user_id, quiz_id,
                                     kept_score, attempt, workflow_state),
                     by = c("quiz_id", "user_id")) |>
    dplyr::left_join(marked_quizzes |>
                       dplyr::select(quiz_id = id, assign_id, title = name), by = "quiz_id") |>
    dplyr::left_join(students, by = dplyr::join_by(user_id == id))

  ## Tidying up
  if(quiz_title == "Worksheet"){
  quiz_scores <- quiz_scores |>
    dplyr::mutate(
      week = ifelse(is.na(title),
                     title,
                     paste0("w", gsub(".* ([0-9]+) .*", "\\1", title))
      )
    )
  } else if (grepl("Learning Log", quiz_title)) {
    quiz_scores <- quiz_scores |>
      tidyr::separate_wider_regex(cols = title,
                                  patterns = c(month = "[:alpha:]*?", " ", type = ".*")) |>
      dplyr::mutate(
        month_number = match(month, month.name),
        .before = month
      ) |>
      dplyr::select(-type)
  }

  quiz_scores <- quiz_scores |>
    tidyr::replace_na(list(kept_score = 0, attempt = 0, workflow_state = "no_submission"))

  return(quiz_scores)

}

#' Calculate "best-of" worksheet marks
#'
#' @param module_code A Sussex module code, e.g. "C8891".
#' @param academic_year A string containing the academic year for the desired
#'   module as e.g. "22/23"; defaults to the current year as calculated by
#'   [cnvs::get_ac_year()]. Does not need to be specified for
#'   ongoing/non-academic modules
#' @param quiz_title Common element of the names of all quizzes to be included.
#'   Defaults to "Worksheet". Passed to [cnvs::get_quiz_marks()].
#' @param quiz_data A long-form dataset of quiz scores, including `cand_no`
#'   (candidate number), `week` (week of term), and `score` (quiz score). Use
#'   this option if you have previously needed to download the quiz data and
#'   make manual changes before calculating the quiz mark. If this argument is
#'   empty, the function will instead download the quiz marks directly from
#'   Canvas using the information in the following arguments.
#' @param max_score Maximum possible score for the quizzes
#'
#' @returns A list with three elements: $all_marks, a tibble of all quiz scores for all
#'   students; $mean_marks, a tibble of final quiz mark for all students; and $dist, a histogram of the distribution of those final marks
#' @export

calc_quiz_marks <- function(module_code,
                           academic_year,
                           quiz_title = "Worksheet",
                           quiz_data,
                           max_score){
  ## CHECK IF THIS WORKS
  if(missing(quiz_data)){
    quiz_scores_full <- cnvs::get_quiz_marks(module_code = module_code,
                                      academic_year = academic_year)
  } else {
    quiz_scores_full <- quiz_data
  }

  n_quizzes <- quiz_scores_full |>
    dplyr::pull(week) |>
    unique() |>
    length()

  ## Rank and then calculate marks
  quiz_marks <- quiz_scores_full |>
    dplyr::group_by(cand_no) |>
    dplyr::arrange(desc(kept_score), .by_group = TRUE) |>
    dplyr::mutate(
      rank = 1:dplyr::n()
    ) |>
    dplyr::slice_min(
      order_by = rank,
      n = n_quizzes - 2
    ) |>
    dplyr::summarise(
      quiz_mark = round(sum(kept_score)/((n_quizzes-2)*max_score)*100)
    )

  ## Marks distribution
  quiz_dist <- quiz_marks |>
    ggplot2::ggplot(ggplot2::aes(quiz_mark)) +
    ggplot2::geom_histogram(
      fill = "darkcyan",
      colour = "black",
      binwidth = 1
    ) +
    ggplot2::labs(x = "Quiz Mark", y = "Count") +
    ggplot2::theme_bw()

  out <- list(
    "all_marks" = quiz_scores_full,
    "mean_marks" = quiz_marks,
    "dist_marks" = quiz_dist
  )

  return(out)
}


