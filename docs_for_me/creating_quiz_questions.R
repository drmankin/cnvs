cnvs::canvas_setup()

module_id <- cnvs::get_module_id("C8891")

quizzes <- cnvs::get_all_quizzes(module_id)

narm_quiz <- quizzes |>
  dplyr::filter(grepl("NARM", title)) |>
  dplyr::pull(id)

if(length(narm_quiz) == 0){
  cnvs::create_quiz(module_id, "NARM Quiz")

  narm_quiz <- cnvs::get_all_quizzes(module_id) |>
    dplyr::filter(grepl("NARM", title)) |>
    dplyr::pull(id)
}
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

args_data <- tibble::tibble(
  name = c("easy", "hard"),
  pick_count = c(1, 3)
)

  ## BY THE WAY. THat trick was:
## Open Developer. Do the thing on Canvas you want to do. Look under Network and especially Requests for the relevant info

cnvs::create_quiz_questions(module_id, narm_quiz, data)

