library(cnvs)
library(tidyverse)

## Setup
cnvs::canvas_setup()

module_id <- cnvs::get_module_id("C8891")

## Get relevant quizzes
all_quizzes <- cnvs::get_all_quizzes(module_id)

these_quizzes <- all_quizzes |>
  dplyr::filter(grepl("Worksheet|CEX", title)) |>
  dplyr::pull(id)

## Create list of datasets of all questions and answers
these_questions <- these_quizzes |>
  purrr::map(~cnvs::get_quiz_questions(module_id = module_id, quiz_id = .x))

## Fixing a data type mismatch
these_questions <- these_questions |>
  purrr::map(
    ~dplyr::mutate(.x, answer_tolerance = as.numeric(answer_tolerance))
  )

## Combine into a single dataset
question_bank <- these_questions |>
  purrr::reduce(dplyr::bind_rows)

## Success!! ##

## Join in quiz names
question_bank <- all_quizzes |>
  dplyr::filter(id %in% these_quizzes) |>
  dplyr::select(quiz_id = id, title) |>
  dplyr::right_join(question_bank)

## Fix the hideous html formatting nonsense (although don't drop all of it!)

question_bank <- question_bank |>
  dplyr::mutate(
    question_text = gsub('.+?<p>(.*)(</p>)*<script.*',
                         "\\1", question_text)
  )

## Add group info

question_bank <- cnvs::add_quiz_groups_info(module_id, question_bank)

## Save

saveRDS(question_bank, "C:/Users/manki/OneDrive - University of Sussex/modules/and_private/quiz_banks/and_bank_24_25.rds")
