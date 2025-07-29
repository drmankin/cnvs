cnvs::canvas_setup()

module_id <- get_module_id("C8891")

quizzes <- cnvs::get_all_quizzes(module_id)

## Let's first GET a quix or answer object to see what they look like

w3_quiz <- quizzes |>
  dplyr::filter(grepl("Week 3 Worksheet", title)) |>
  dplyr::pull(id)

w3_quiz_qs <- get_quiz_questions(module_id, w3_quiz)




narm_quiz <- quizzes |>
  dplyr::filter(grepl("NARM", title)) |>
  dplyr::pull(id)

test_quiz_question <- tibble::tibble(
  quiz_id = narm_quiz,
  question_name = "worst",
  question_text = "Which of the Ambachters is CLEARLY the worst?",
  question_type = "multiple_choice_question",
  points_possible = 1,
  position = NA,
  correct_comments_html = "You're right, he IS the worst",
  incorrect_comments_html = "Don't be ridiculous, the answer is Aabvious"
  #answers = list(tibble::tibble(text = c("Akeelv", "Aa")))
)

make_question_names <- function(.x){
  .x <- dplyr::select(-quiz_id)

  arg_names <- names(.x)

  question_arg_names <- paste0("`question[", arg_names[!grepl(pattern = "answer", x = arg_names)], "]`")

  ## First gotta decide how this is represented in the datset
  answer_arg_names <- "?"
}

## Do I really WANT to use purrr here? or is for actually going to be less awful

create_quiz_question <- function(quiz_id, question_name, question_text, question_type,
                                 points_possible, position,
                                 correct_comments, incorrect_comments){ #, answers){

  ##Okay we need a way to dynamically name this list
  ## based on the kind of question, and how many answers it has
  ## Can't use a function on the name side so probably create first, then rename with names()
  ## Could that make this whole process more efficient anyway?
  ## Like if we name the arguments appropriately can we select AND name them dynamically
  ## because this is already a lot of messing about with duplicates

  ## BY THE WAY. THat trick was:
  ## Open Developer. Do the thing on Canvas you want to do. Look under Network and especially Requests for the relevant info
  args <- list(
    access_token = rcanvas:::check_token(),
    `question[question_name]` = question_name,
    `question[question_text]` = question_text,
    `question[question_type]` = question_type,
    `question[points_possible]` = points_possible,
    `question[position]` = position,
    `question[correct_comments_html]` = correct_comments,
    `question[incorrect_comments_html]` = incorrect_comments,
    `question[answers][0][answer_text]` = "Akeelv",
    `question[answers][0][answer_weight]` = 0,
    `question[answers][1][answer_text]` = "Aa",
    paste0("`question[answers][,", 1, "][answer_weight]`") = 0
  )
  quiz <- rcanvas:::canvas_query(
    paste0("https://canvas.sussex.ac.uk/api/v1/courses/", module_id, "/quizzes/", quiz_id, "/questions"),
    args, "POST")
}

purrr::pmap(test_quiz_question, create_quiz_question)

narm_quiz_qs <- get_quiz_questions(module_id, narm_quiz)
