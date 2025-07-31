cnvs::canvas_setup()

module_id <- cnvs::get_module_id("C8891")

quizzes <- cnvs::get_all_quizzes(module_id)

# narm_quiz <- quizzes |>
#   dplyr::filter(grepl("NARM", title)) |>
#   dplyr::pull(id)

# test_quiz_question <- tibble::tibble(
#   question_name = "worst",
#   question_text = "Which of the Ambachters is CLEARLY the worst?",
#   question_type = "multiple_choice_question",
#   points_possible = 1,
#   position = NA,
#   correct_comments_html = "You're right, he IS the worst",
#   incorrect_comments_html = "Don't be ridiculous, the answer is Aabvious",
#   answers = list(tibble::tibble(answer_text = c("Akeelv", "Aa"), answer_weight = c(0, 100)))
# )
#
test_quiz_questions <- tibble::tibble(
  question_name = c("worst", "alive"),
  question_text = c("Which of the Ambachters is CLEARLY the worst?", "Which are alive?"),
  question_type = c("multiple_choice_question", "multiple_answers_question"),
  points_possible = 1,
  position = NA,
  correct_comments_html = c("You're right, he IS the worst", NA),
  incorrect_comments_html = c("Don't be ridiculous, the answer is Aabvious", NA),
  answers = list(
    tibble::tibble(answer_text = c("Akeelv", "Aa"), answer_weight = c(0, 100)),
    tibble::tibble(answer_text = c("Akeelv", "Aa", "Rooster", "Ten Vine Holder"),
                   answer_weight = c(100, 100, 100, 0))
    )
)

data <- test_quiz_questions

make_question_args <- function(data){
  ## Unlisted tibble
  question_list <- data |> unlist()
  ## Unlisted names only
  arg_names <- names(question_list)

  #### Deal with [answers] ####
  ## Find the names that contain "answers" (all of the ones from inside the nested tibble)
  answers_names <- arg_names[grepl("answer", arg_names)]

  ## Extract the numerical indices at the end and subtract one to start numbering from 0
  answers_indices <- gsub(".*(\\d+)", "\\1", answers_names) |>
    as.numeric() - 1

  ## Drop the numerical indices from the answer names
  answers_names <- gsub("(.*)\\d+", "\\1", arg_names[grepl("answer", arg_names)])

  ## Paste back together with the 0-start indices instead
  answers_names_numb <- paste0(answers_names, answers_indices)

  ## Reformat names and rename in argument names
  arg_names[grepl("answer", arg_names)] <- gsub("(.+?)\\.(.+?)(\\d+)", "question\\[\\1\\]\\[\\3\\]\\[\\2\\]", answers_names_numb)

  #### Reformat and Output ####
  ## non-answer question names
  arg_names[!grepl("answer", arg_names)] <- paste0("question[", arg_names[!grepl("answer", arg_names)], "]")

  ## Assign names to values
  names(question_list) <- arg_names

  return(question_list)
}


  ## BY THE WAY. THat trick was:
## Open Developer. Do the thing on Canvas you want to do. Look under Network and especially Requests for the relevant info


create_quiz_questions <- function(quiz_id, data){
  split_data <- data |>
    dplyr::rowwise() |>
    dplyr::group_split()

  data_args <- purrr::map(split_data, make_question_args)

  purrr::map2(
    quiz_id, data_args,
    \(.x, .y) {
      args <- append(list(access_token = rcanvas:::check_token()), .y)
      quiz <- rcanvas:::canvas_query(
        paste0("https://canvas.sussex.ac.uk/api/v1/courses/", module_id, "/quizzes/", .x, "/questions"),
        args, "POST")
    }
  )
}


create_quiz_questions(narm_quiz, data)

