#' Create quiz descriptions
#'
#' @param n_questions Total number of questions the quiz will contain
#' @param standard_time_limit Time limit in minutes for the "standard" amount of
#'   time, i.e. before any extra time reasonable adjustments are implemented.
#'   The function will automatically calculate and add info about 25% and 50%
#'   extra time based on this number.
#' @param challengr Are the quizzes being created ChallengRs?
#'
#' @returns A list of two descriptions, "practice" and "marked".
#' @export

create_quiz_desc <- function(n_questions = 5, standard_time_limit = 12, challengr = FALSE){
  extra_25 <- (standard_time_limit*1.25) |> round()
  extra_50 <- (standard_time_limit*1.5) |> round()

  quiz_desc_marked <- paste("<h2>Instructions</h2>
<p>This is a marked worksheet that contains", n_questions, "questions that you should answer on Canvas. The questions cover topics from last week's lectures and skills lab, and this week's tutorial and worksheet. Before you begin this quiz, you should first read these instructions and complete all the analyses in the worksheet. You will need to use the output from your analysis to answer the questions.</p>
<p>You will have ", n_questions, " randomly selected questions to answer; do not be surprised if you have different questions from others working on the same worksheet!</p>
<p>To access the worksheet, you must attend your practical session. In the session, an access code will be announced to unlock the worksheet; you must begin the worksheet within 5 minutes of the access code being released. You will have ", standard_time_limit, " minutes to complete the worksheet, unless you have reasonable adjustments for extra time (", extra_25, " minutes for 25% extra time, and ", extra_50, " minutes for 50% extra time).</p>"
)

  quiz_desc_practice <- paste("<h2>Instructions</h2>
  <p>This is an unmarked (practice) worksheet that contains", n_questions, "questions. Before you begin the worksheet, you should first read these instructions and complete all the analyses in the worksheet. You will need to use the output from your analysis to answer the questions. This quiz is simply for practice; you will not be marked for your quiz attempt this week.&nbsp;</p>
  <p>You will have ", n_questions, " randomly selected questions to answer; do not be surprised if you have different questions from others working on the same worksheet!</p>
  <p>This week, you will be able to access the worksheet at any time, take it as many times as you want, and make use of the full quiz time to do it. However, this is not the same for future weeks, when you will be required to attend your practical session (online or in person) in order to access the quiz. From next week, you will have ", standard_time_limit, " minutes to complete the worksheet, unless you have reasonable adjustments for extra time (", extra_25, " minutes for 25% extra time, and ", extra_50, " minutes for 50% extra time).</p>"
)

  quiz_desc_challengr <- paste("<h2>Instructions</h2>
                               <p>To complete this quiz, you will first need to complete the associated ChallengR task. When you successfully complete the task, you will have passcode(s) to enter in this quiz.</p>
                               <p>To redeem your Kahoot! points, take this quiz and enter the passcode(s) as directed and submit. We will automatically combine your ChallengR points with the rest of your Kahoot! points.</p>"
                               )

  quiz_desc_absent <- paste0(
                            paste("<h2>Requirements</h2>
                            <p>In order for your mark on this quiz to count, one of the following must be true:<br>
                            1. You are attempting this quiz in a live practical session, and your attendance has been recorded in the session<br>
                            2. You were absent from your practical session, and have already notified the School office of the absence before you begin this quiz<br>
                            3. You had a technical issue with the quiz on your first attempt, and have already notified the module convenor and received confirmation that you can retake via the mop-up code<br>
                            4. You have reasonable adjustments in place and have agreed with the module convenor that you will take the quizzes via the mop-up code<br><br>

                            Note that if you do not meet any of the requirements above, your mark <strong>will not be valid</strong> and will be replaced with a 0 before calculating your final quiz score. For more details, see "),
                            cnvs::embed_link(link_text = "the Worksheet Information page", search_term = "C8891", dest = "worksheet-information", type = "html"), ".</p>"
  )

  quiz_desc_achon <- paste("<h3>Academic Honesty</h3>
    <p>You are welcome to use module resources - e.g. lecture slides, tutorials, skills labs scripts - to answer these questions. You are also welcome to use RStudio to solve problems, do calculations, or refer to output. However, you should <strong>not</strong> work with other students while you are completing the worksheet, and tutors will only be able to answer questions about technical problems (e.g. computer crash).</p>"
)

  return(list(
    practice = paste(quiz_desc_practice, quiz_desc_achon),
    marked = ifelse(challengr == TRUE,
                    paste(quiz_desc_challengr),
                    paste(quiz_desc_marked, quiz_desc_absent, quiz_desc_achon)
    )
  ))
}

#' Create quiz metadata
#'
#' Make a tibble of information to use in [cnvs::create_quizzes()].
#' Specifically designed to make weekly quizzes for worksheets.
#'
#' If quizzes are "ChallengRs", they will be named accordingly and be practice quizzes.
#'
#' @param module_code A string containing the module code in the format "123C4"
#'   or "C1234". Either module_code or module_id must be provided.
#' @param module_id Canvas module ID number. Either module_code or module_id must be provided.
#' @param description Quiz description as a string, which can include basic HTML
#'   formatting tags. If `NULL` (the default), runs
#'   [cnvs::create_quiz_desc()] with default settings. Also accepts
#'   either a single string (which will be applied to all quizzes) or a list of
#'   two descriptions, named `practice` and `marked`. The `practice` description
#'   will only be used for the week(s) indicated in `make_practice`.
#' @param first_unlock_date Date as "YYYY/MM/DD" of the date that the first quiz
#'   of term should unlock.
#' @param unlock_time Hours as a number past midnight when the quizzes should
#'   unlock each week. E.g. for 9am, enter `9`.
#' @param last_lock_date Date as "YYYY/MM/DD" of the date that the last quiz of
#'   term should lock.
#' @param lock_time Hours as a number past midnight when the quizzes should lock
#'   each week. E.g. for 6pm, enter `18`.
#' @param starting_week Which week of term do quizzes start in?
#' @param mopup_day Optional. If mop-up quizzes are allowed, what day should the
#'  quiz close on? Lock time will be set to to 23:59:59 on that day.
#' @param quiz_type Either one of "assignment", "practice_quiz",
#'   "graded_survey", or "survey" (which will be applied to all) or a vector of
#'   combinations of the same in the order in which they should be applied.
#' @param make_practice Which quiz(zes) should be practice quizzes, rather than
#'   marked? Defaults to `1` but accepts a numeric vector corresponding to week
#'   numbers, or `NULL` if none should be changed. For those quizzes indicated,
#'   the following will be changed:
#'     * `quiz_type` will be set to "practice_quiz"
#'     * `title` will be updated so the word "(Practice)" is at the end of
#'     the title
#'     * `description` will be updated to the description stored in "$practice"
#'     * `allowed_attempts` will be set to -1 (which will translate to
#'     unlimited)
#'     * `access_code` will be set to FALSE (which will be converted to NULL
#'     when combined with [cnvs::create_quizzes()])
#' @param title_fn A function to name the quizzes. Default is `function(x)
#'   paste("Week", x, "Worksheet", collapse = " ")`. Minimally requires `x`,
#'   which is a number corresponding to the week of term for that quiz.
#' @param n_questions Total number of questions the quiz will contain
#' @param standard_time_limit "Standard" amount of time for the quiz. Actual
#'   quiz length will be 1.5 times longer (for 50% extra time)
#' @param has_break_weeks Logical. Does the module you are making quizzes for
#'   have break weeks (e.g. spring break)?
#' @param break_weeks Numerical vector indicating weeks out of the total number
#'   of weeks that the term spans are break weeks (or otherwise don't have
#'   quizzes). E.g. for an 11-week term, with three weeks of spring break, the
#'   total number of weeks is 14: 10 weeks of term, three weeks break, and one
#'   final week of term. In this case the input would be 11:13.
#' @param challengr Should the quizzes be ChallengRs?
#'
#' @returns A tibble of quiz information to pass to
#'   [cnvs::create_quizzes()].
#' @export

create_quiz_info <- function(module_code, module_id, description = NULL,
                             first_unlock_date, unlock_time,
                             last_lock_date, lock_time,
                             starting_week = 1, mopup_day,
                             quiz_type = c("assignment", "practice_quiz", "graded_survey", "survey"),
                             make_practice = 1,
                             title_fn = function(x) paste("Week", x, "Worksheet", collapse = " "),
                             n_questions = 5,
                             standard_time_limit = 12,
                             has_break_weeks = TRUE, break_weeks = 11:13,
                             challengr = FALSE){

  # module ID/code input validation
  if(missing(module_code) & missing(module_id)){
    stop("You must provide either module_code or module_id")
  }

  if(missing(module_id) & !missing(module_code)){
    if (!stringr::str_detect(module_code, "^[A-Z]?[0-9]+[A-Z]?[0-9]+"))
      stop("Module code is not specified correctly. Please enter a module code formatted as C1234 or 123C4.")

    module_id <- try(cnvs::get_module_id(module_code), silent = TRUE)

    if(inherits(module_id, "try-error")){
      cnvs::canvas_setup()
      module_id <- cnvs::get_module_id(module_code)
    }
  }

  # If no description provided, defaults to the cnvs-generated one
  if(is.null(description)){
    description <- cnvs::create_quiz_desc(n_questions = n_questions,
                                  standard_time_limit = standard_time_limit,
                                  challengr = challengr)
  }

  # Set default quiz types and names for ChallengRs if none provided

  if(challengr & missing(title_fn)){
    title_fn <- function(x) paste("Week", x, "ChallengR", collapse = " ")
  }

  if(challengr & missing(quiz_type)){
    quiz_type <- "practice_quiz"
  }

  if(challengr & missing(make_practice)){
    make_practice <- NULL
  }

  # Checks if the quiz description input is structured correctly
  if(length(description) > 1 & !all(names(description) == c("practice", "marked"))){
    stop("Quiz description not constructed correctly, please provide two descriptions named `practice` and `marked`.")
  }

  # Calculate the total weeks spanned by the practicals
  total_weeks <- suppressMessages(difftime(last_lock_date, first_unlock_date, units = "weeks") |> ceiling())

  # Generate an initial table with lock and unlock dates and times, module ID, week numbers, and allowed attempts
  quiz_times <- tibble::tibble(
    unlock_at = seq.Date(as.Date(first_unlock_date), by = "week", length.out = total_weeks) + lubridate::hours(unlock_time),
    ## This nonsense is to get the sequence of days right counting from the last lock date!
    lock_at = seq.Date(as.Date(last_lock_date) - lubridate::weeks(total_weeks - 1),
                    by = "week", length.out = total_weeks) + lubridate::hours(lock_time),
    module_id = module_id,
    week = 1:total_weeks,
    time_limit = 1.5*standard_time_limit,
    allowed_attempts = 1
  )

  # Update if there's a mopup day

  if(!missing(mopup_day)){

    # weekdays <- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
    # this_weekday <- which(weekdays == mopup_day)

    ## THIS IS THROWING AN ERROR BUT MY HEAD HURTS. FIX IT
    quiz_times <- quiz_times |>
      dplyr::mutate(
        lock_at = dplyr::case_when(
          week != max(week) ~ (cnvs::find_weekday(date = as.Date(unlock_at), weekday = mopup_day, direction = "next")
                      + lubridate::hours(23) + lubridate::minutes(59) + lubridate::seconds(59)),
          week == max(week) ~ lock_at
      )
      )
  }

  # Make sure datetimes are right
  quiz_times <- quiz_times |>
    dplyr::mutate(
      dplyr::across(
        dplyr::contains("lock"),
        ## keeps the timezone right but converts to character
        ~ lubridate::format_ISO8601(lubridate::as_datetime(.x), usetz = FALSE, precision = "ymdhms"))
    )

  ### FIGURE OUT THIS BREAK WEEK AND STARTING WEEK STUFF
  # Remove break weeks (if there are any)
  if (has_break_weeks){
    quiz_times <- quiz_times |>
      dplyr::filter(!week %in% c(break_weeks)) |>
      dplyr::mutate(week = starting_week:dplyr::n())
  }

  # Set the quiz types, names, and access code
  quiz_times <- quiz_times |>
    dplyr::mutate(
      quiz_type = quiz_type
    ) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      title = do.call(title_fn, list(x = week)),
      access_code = TRUE
    )

  # Set the description for the quiz - the same for all if there's only one, or the "$marked" description if there's two
  if(length(description) == 1){
    quiz_times <- quiz_times |>
      dplyr::mutate(
        description = description
      )
  } else {
    quiz_times <- quiz_times |>
      dplyr::mutate(
        description = description$marked
      )
  }

  # Make changes to any weeks that should be practice weeks
  quiz_times$quiz_type[make_practice] <- "practice_quiz"
  quiz_times$title[make_practice] <- gsub("(.*)", "\\1 (Practice)", quiz_times$title[make_practice])
  quiz_times$description[make_practice] <- ifelse(length(description) > 1, description$practice, description)
  quiz_times$allowed_attempts[make_practice] <- -1
  quiz_times$access_code[make_practice] <- FALSE

  quiz_times <- quiz_times |>
    dplyr::mutate(
      access_code = dplyr::case_when(
        access_code == TRUE ~ paste(sample(0:9, 6, replace = TRUE), collapse = ""),
        .default = NULL
      ),
      time_limit = dplyr::case_when(!is.na(time_limit) ~ time_limit, .default = NULL)
    )

  if(challengr == TRUE){
    quiz_times <- quiz_times |>
      dplyr::mutate(
        ## Lock ChallengRs only at the end of term
        lock_at = (as.Date(last_lock_date) + lubridate::hours(lock_time)) |>
          lubridate::format_ISO8601(usetz = FALSE, precision = "ymdhm"),
        allowed_attempts = -1,
        access_code = FALSE,
        time_limit = NULL
      )


  }

  return(quiz_times)

}

#' Create (multiple) Canvas quizzes
#'
#' Create weekly worksheet quizzes on your Canvas site. For making multiple
#' quizzes at once, use in conjunction with [purrr::pmap()] and a tibble of
#' associated quiz info generated by [cnvs::create_quiz_info()].
#'
#' This function is built to work with others for generating multiple similar
#' quizzes at once. If you want a more basic version with fewer assumptions,
#' see [cnvs::create_quiz()].
#'
#' @param week Week of term
#' @param title Title of the quiz to be created
#' @param unlock_at A datetime using [format_ISO8601()] indicating when the quiz
#'   should be unlocked.
#' @param lock_at A datetime using [format_ISO8601()] indicating when the quiz
#'   should be locked.
#' @param module_id Canvas module ID number
#' @param description Single string containing the text description for the quiz.
#'   Accepts basic HTML formatting tags.
#' @param quiz_type Either one of "assignment", "practice_quiz",
#'   "graded_survey", or "survey" (which will be applied to all) or a vector of
#'   combinations of the same in the order in which they should be applied.
#' @param time_limit Time limit for the quiz in minutes.
#' @param allowed_attempts A number, with -1 corresponded to infinite attempts.
#' @param access_code Logical. If TRUE, a random 6-digit code will be generated.
#'   If FALSE, no code will be required to access the quiz.

#'
#' @seealso [cnvs::get_module_id()]
#'
#' @returns Answer from Canvas; quizzes should appear on Canvas.
#' @export

create_quizzes <- function(week, title, unlock_at, lock_at, module_id, description,
                           quiz_type, time_limit, allowed_attempts, access_code){

  quiz_access <- NULL

  quiz_access <- if(access_code) paste(sample(0:9, 6, replace = TRUE), collapse = "")

  if(missing(time_limit)){
    time_limit <- NULL
  }

  args <- list(
    access_token = rcanvas:::check_token(),
    `quiz[title]` = title,
    `quiz[description]` = description,
    `quiz[quiz_type]` = quiz_type,
    `quiz[time_limit]` = time_limit, # NULL for no time limit
    `quiz[shuffle_answers]` = TRUE,
    `quiz[hide_results]` = "always",
    `quiz[allowed_attempts]` = allowed_attempts,
    `quiz[access_code]` = quiz_access,
    `quiz[unlock_at]` = unlock_at,
    `quiz[lock_at]` = lock_at,
    `quiz[published]` = FALSE
  )
  quiz <- rcanvas:::canvas_query(
    paste0("https://canvas.sussex.ac.uk/api/v1/courses/", module_id, "/quizzes"),
    args, "POST")
}

#' Create a Canvas quiz
#'
#' An open-ended function with (extremely) few assumptions about the settings
#' the quiz should have. If you want an easier way to batch-create multiple
#' quizzes that DOES make (quite a few) more assumptions, see
#' [cnvs::create_quizzes()].
#'
#' The only essential information to provide is the quiz title, but I do
#' recommend providing more than this, unless you're quite happy with the
#' defaults (unlikely)!
#'
#' @param module_id Canvas module ID number
#' @param title Title of the quiz.
#' @param ... Any number of additional arguments. These MUST correspond to valid
#' parameter names for an API request, as they will be automatically interpreted
#' as such. For instance, to set the parameter `quiz[quiz_type]`, provide the
#' argument `quiz_type = "assignment"` (or whichever allowed value you want.)
#' For a complete list of parameters and allowed values, see [the Quizzes API]
#' (https://developerdocs.instructure.com/services/canvas/resources/quizzes#method.quizzes/quizzes_api.create).
#'
#'
#' @returns Response from Canvas
#' @export


create_quiz <- function(module_id, title,  ...){
  args <- tibble::tibble(title = title, ...)
  args <- cnvs::make_args(args, type = "quiz")

  args <- append(list(access_token = rcanvas:::check_token()), args)

  quiz <- rcanvas:::canvas_query(
    paste0(Sys.getenv("CANVAS_DOMAIN"), "/api/v1/courses/", module_id, "/quizzes"),
    args, "POST")
}

#' Update existing Canvas quizzes
#'
#' Change settings for existing Canvas quizzes.
#'
#' @param module_code A string containing the module code in the format "123C4"
#'   or "C1234". Either module_code or module_id must be provided.
#' @param module_id Canvas module ID number. Either module_code or module_id must be provided.
#' @param search_term Title or substring of the quiz name to modify. Either `search_term` or `quiz_id` must be provided.
#' @param quiz_id Canvas quiz ID number. Either `search_term` or `quiz_id` must be provided.
#' @param task What do you want to change about the quiz?
#'  * `release_answers`: Release both results and correct answers
#'  * `publish`: Publish an unpublished quiz
#'  * `change_code`: Change the access code for a quiz (to a random six-digit code)
#'  * `update_description`: Overwrite the description of the quiz.
#'  * `other`: Any other changes
#' @param description Optional - string containing the new description, if using "update_description".
#' @param other_args List of arguments to pass to Canvas when `task = "other"`. See
#'   [Canvas API documentation for quizzes](https://canvas.instructure.com/doc/api/quizzes.html#method.quizzes/quizzes_api.create)
#'   for options.
#'
#' @returns Response from Canvas.
#' @export
#'

update_quiz <- function(module_code, module_id,
                           search_term, quiz_id,
                           task = c("release_answers", "publish", "change_code", "update_description", "other"),
                           description,
                           other_args = list()){

  if(missing(module_code) & missing(module_id)){
    stop("You must provide either module_code or module_id")
  }

  if(missing(module_id) & !missing(module_code)){
    if (!stringr::str_detect(module_code, "^[A-Z]?[0-9]+[A-Z]?[0-9]+"))
      stop("Module code is not specified correctly. Please enter a module code formatted as C1234 or 123C4.")

    module_id <- try(cnvs::get_module_id(module_code), silent = TRUE)

    if(inherits(module_id, "try-error")){
      cnvs::canvas_setup()
      module_id <- cnvs::get_module_id(module_code)
    }
  }

  if(missing(search_term) & missing(quiz_id)){
    stop("You must provide either search_term or quiz_id")
  }

  if(missing(quiz_id) & !missing(search_term)){
    quizzes <- quizzes <- cnvs::get_all_quizzes(module_id)
    quiz_id <- quizzes |>
      dplyr::filter(grepl(search_term, title)) |>
      dplyr::pull(id)
  }

  if(task == "update_description" & missing(description)){
    stop("You must provide a new description")
  }

  if(task == "release_answers"){
    args <- list(
      `quiz[hide_results]` = "",
      `quiz[show_correct_answers]` = TRUE)
  } else if (task == "publish"){
    args <- list(
      `quiz[published]` = TRUE)
  } else if (task == "change_code"){
    args <- list(
      `quiz[access_code]` = paste(sample(0:9, 6, replace = TRUE), collapse = ""))
  } else if (task == "update_description"){
    args <- list(
      `quiz[description]` = description)
  } else {
    args <- other_args
  }

  args <- c(list(access_token = rcanvas:::check_token()),
            args)

  rcanvas:::canvas_query(
    paste0(Sys.getenv("CANVAS_DOMAIN"), "/api/v1/courses/", module_id, "/quizzes/", quiz_id),
    args, "PUT")
}

#' Convert a tibble to Canvas query arguments
#'
#' This function converts a tibble of API parameters into a formatted args list
#' to pass on to an API query. The variable names MUST match allowable
#' parameters for the relevant query scope, and the values MUST match allowable
#' values (which I imagine is unsurprising).
#'
#' @param data A tibble with variable names
#' @param type What type of arguments to generate. This string will be pasted
#' to the beginning of each argument name (except in the case of variable names
#' beginning with the string "answers"). For example, use "quiz" for use with
#' [cnvs::create_quiz()] and "question" for [cnvs::create_quiz_question()].
#'
#' @returns A list of arguments, one for each quiz question to be created.

make_args <- function(data, type){
  ## Unlisted tibble
  data_list <- data |> unlist()
  ## Unlisted names only
  arg_names <- names(data_list)

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
  arg_names[!grepl("answer", arg_names)] <- paste0(type, "[", arg_names[!grepl("answer", arg_names)], "]")

  ## Assign names to values
  names(data_list) <- arg_names

  return(data_list)
}


#' Post quiz questions to a Canvas quiz
#'
#' In order to create a question successfully, the variable names in `data`
#' MUST match the corresponding fields for the QuizQuestion and Answer
#' objects [see Canvas API help documentation for options](https://developerdocs.instructure.com/services/canvas/resources/quiz_questions).
#' Furthermore, the answer information must be held in its own tibble, nested
#' in the question data tibble, in a variable called `answers`. For a template,
#' use [cnvs::quiz_question_example].
#'
#' @param module_id Canvas module ID number
#' @param quiz_id Numerical Canvas ID number for the quiz to which the questions will be added.
#' @param data A tibble of quiz information with answer information nested by
#' row; for a template, use [cnvs::quiz_question_example].
#'
#' @returns Response from Canvas
#' @export

create_quiz_questions <- function(module_id, quiz_id, data){
  ## Split data by row into a list
  split_data <- data |>
    dplyr::rowwise() |>
    dplyr::group_split()

  ## Drop columns with NA (to prevent extraneous arguments)
  split_data <- split_data |>
    purrr::map(~dplyr::select(.x, where(~all(!is.na(.x)))))

  ## Convert tibble to argument list
  data_args <- purrr::map(split_data, ~cnvs::make_args(data = .x, type = "question"))

  ## Post
  purrr::map2(
    quiz_id, data_args,
    \(.x, .y) {
      args <- append(list(access_token = rcanvas:::check_token()), .y)
      quiz <- rcanvas:::canvas_query(
        paste0(Sys.getenv("CANVAS_DOMAIN"), "/api/v1/courses/", module_id, "/quizzes/", .x, "/questions"),
        args, "POST")
    }
  )
}

#' Get a Canvas quiz ID for an existing quiz
#'
#' Note that this function will happily return more than one quiz ID, so if you
#' don't want that, do check your search term!
#'
#' @param module_id Canvas module ID number to search in (see [cnvs::get_module_id()])
#' @param search_term String or regex to search the quiz names
#'
#' @returns A vector of quiz IDs for all quizzes matching the search term
#' @export

get_quiz_id <- function(module_id, search_term){
  all_quizzes <- cnvs::get_quiz_info(module_id)

  all_quizzes |>
    dplyr::filter(grepl(search_term, name)) |>
    dplyr::pull(id)
}



#' #' Create information about quiz groups
#' #'
#' #'
#'
#' which_week <- 1:11
#'
#' module_id <- try(convenrhelpr::get_module_id(module_code), silent = TRUE)
#'
#' if(inherits(module_id, "try-error")){
#'   convenrhelpr::canvas_setup()
#'   module_id <- convenrhelpr::get_module_id(module_code)
#' }
#'
#' assignments <- rcanvas::get_assignment_list(module_id) |> tibble::as_tibble()
#'
#' quiz_id <- assignments |>
#'   dplyr::filter(name %in% paste("Week", which_week, "Worksheet", sep = " ")) |>
#'   dplyr::pull(quiz_id)
#'
#' groups_info <- tidyr::crossing(quiz_id,
#'                                group_name = c("Concepts", "Code", "Interpretation")) |>
#'   dplyr::mutate(
#'     pick_count = ifelse(group_name == "Interpretation", 3, 2),
#'     points = 1,
#'     module_id = module_id
#'   )
#'

# purrr::pmap(groups_info, create_quiz_groups)

#' Create groups in a quiz
#'
#' Generate question groups in an existing quiz. There are only four valid
#' parameters that can be provided:
#'
#' - `name`: the name of the group
#' - `pick_count`: how many questions should be randomly selected from the group
#' - `question_points`: how many points each question is worth
#' - `assessment_question_bank_id`: ID number of the assessment question bank to
#' link to. Unclear at the moment if/how this ID number can be accessed via the
#' API; otherwise, find it here:
#' https://canvas.your_uni.ac.uk/courses/module_id/question_banks/THIS_NUMBER
#'
#' Parameters can be provided as either a tibble (for multiple groups at once)
#' or as named arguments. Either method will accept any combination of
#' parameters, and the argument method will accept none.
#'
#' @param module_id Canvas module ID
#' @param quiz_id Canvas quiz ID
#' @param args_data Tibble of arguments, with variable names the same as the
#' parameter options (listed above) and values for each group in each row
#' @param ... Parameter options (listed above) given as named arguments, e.g.
#' `name = "Hard Questions"`.
#'
#' @returns Response from Canvas
#' @export

create_quiz_groups <- function(module_id, quiz_id, args_data, ...){

  ## Use tibble if provided, otherwise use any named arguments
  if(missing(args_data)){
    data <- tibble::tibble(...)
  } else {
    data <- args_data
  }

  ## Split data by row into a list
  split_data <- data |>
    dplyr::rowwise() |>
    dplyr::group_split()

  ## Drop columns with NA (to prevent extraneous arguments)
  split_data <- split_data |>
    purrr::map(~dplyr::select(.x, where(~all(!is.na(.x)))))

  ## Convert data to arguments
  args <- purrr::map(split_data, ~cnvs::make_args(data = .x, type = "quiz_groups[]"))

  ## Post
  purrr::map2(
    quiz_id, args,
    \(.x, .y) {
      args <- append(list(access_token = rcanvas:::check_token()), .y)
      quiz <- rcanvas:::canvas_query(
        paste0(Sys.getenv("CANVAS_DOMAIN"), "/api/v1/courses/", module_id, "/quizzes/", .x, "/groups"),
        args, "POST")
    }
  )
}

#' Get information about quiz groups
#'
#' For a quiz with existing quiz groups, retrieve the info/settings about these
#' groups. Accepts one or multiple quiz group IDs but will ignore duplicate IDs.
#'
#' @param module_id Canvas module ID
#' @param quiz_id Canvas quiz ID with existing quiz groups
#' @param quiz_group_id Canvas quiz IDs of existing quiz groups
#'
#' @returns Tibble of quiz groups information
#' @export

get_quiz_groups <- function(module_id, quiz_id, quiz_group_id){
  quiz_group_id <- unique(quiz_group_id)

  url <- paste0(Sys.getenv("CANVAS_DOMAIN"), "/api/v1/courses/", module_id, "/quizzes/", quiz_id, "/groups/", quiz_group_id)
  args <- list(access_token = rcanvas:::check_token(), per_page = 100)

  purrr::map(
    url,
    ~rcanvas:::process_response(.x, args)
  )  |>
    purrr::reduce(
      dplyr::bind_rows
    ) |>
    dplyr::mutate(module_id = module_id)

}

#' Add info about quiz groups to question info
#'
#' Given a tibble of question info, will retrieve the information about quiz
#' group info for each question and merge into the table. Great for creating
#' question banks!
#'
#' @param module_id Canvas module ID number
#' @param quizzes_info A tibble of quiz question info minimally containing
#' variables `quiz_id` and `quiz_group_id`.
#'
#' @returns The same `quizzes_info` tibble but with added details about the quiz
#' group each question belongs to - see [cnvs::get_quiz_groups()]
#' @export

add_quiz_groups_info <- function(module_id, quizzes_info){
  quizzes_info |>
    dplyr::select(quiz_id, quiz_group_id) |>
    dplyr::filter(!is.na(quiz_group_id)) |>
    dplyr::distinct() |>
    purrr::pmap(
      .l = _,
      .f = ~cnvs::get_quiz_groups(module_id, ..1, ..2)
    ) |>
    purrr::reduce(
      dplyr::bind_rows
    ) |>
    dplyr::rename(quiz_group_id = id) |>
    dplyr::right_join(quizzes_info, by = join_by(quiz_id, quiz_group_id))
}
