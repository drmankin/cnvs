library(devtools)


# Cheatsheet ---------------------------------------------------------------

## Load all functions
devtools::load_all()

## Insert roxygen skeleton by putting your cursor in a function and press ctrl
## shift alt r
## Wrap comments with ctrl shift /

## Get set up
canvas_setup()

## Create new Canvas site folder
canvas_path <- "C:/Users/manki/OneDrive - University of Sussex/Canvas"

### Replace with a unique string in the name of the Canvas site
search_term <- "C8891"
folder_path <- cnvs_folder("C:/Users/manki/OneDrive - University of Sussex/Canvas", search_term)




# Quizzes -----------------------------------------------------------------

## For AND 23/24:

## Create worksheet quiz info
quiz_times <- create_quiz_info(module_code = "C8891", quiz_desc = NULL,
                             first_unlock_date = "2024/01/29", unlock_time = 11,
                             last_lock_date = "2024/05/03", lock_time = 13,
                             quiz_type = "assignment",
                             make_practice = c(1, 7),
                             quiz_name_fn = function(x) paste("Week", x, "Worksheet", collapse = " "),
                             time_limit = 30,
                             has_break_weeks = TRUE, break_weeks = 9:11)

# purrr::pmap(quiz_times, create_quizzes)

## Create ChallengR quiz info
quiz_times <- create_quiz_info(module_code = "C8891",
                               quiz_desc = create_quiz_desc(challengr = TRUE),
                               ## Choose release date/time of first tutorial
                               first_unlock_date = "2024/01/31", unlock_time = 9,
                               ## Choose morning of last skills lab
                               last_lock_date = "2024/05/02", lock_time = 9,
                               quiz_type = "assignment",
                               make_practice = NULL,
                               quiz_name_fn = function(x) paste("Week", x, "ChallengR", collapse = " "),
                               time_limit = NULL,
                               has_break_weeks = TRUE, break_weeks = 9:11,
                               challengr = TRUE)

# purrr::pmap(quiz_times, create_quizzes)
