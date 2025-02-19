% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/quizzes.R
\name{update_quizzes}
\alias{update_quizzes}
\title{Update existing Canvas quizzes}
\usage{
update_quizzes(
  module_code,
  module_id,
  search_term,
  quiz_id,
  task = c("release_answers", "publish", "change_code", "update_description", "other"),
  description,
  other_args = list()
)
}
\arguments{
\item{module_code}{A string containing the module code in the format "123C4"
or "C1234". Either module_code or module_id must be provided.}

\item{module_id}{Canvas module ID number. Either module_code or module_id must be provided.}

\item{search_term}{Title or substring of the quiz name to modify. Either `search_term` or `quiz_id` must be provided.}

\item{quiz_id}{Canvas quiz ID number. Either `search_term` or `quiz_id` must be provided.}

\item{task}{What do you want to change about the quiz?
* `release_answers`: Release both results and correct answers
* `publish`: Publish an unpublished quiz
* `change_code`: Change the access code for a quiz (to a random six-digit code)
* `update_description`: Overwrite the description of the quiz.
* `other`: Any other changes}

\item{description}{Optional - string containing the new description, if using "update_description".}

\item{other_args}{List of arguments to pass to Canvas when `task = "other"`. See
[Canvas API documentation for quizzes](https://canvas.instructure.com/doc/api/quizzes.html#method.quizzes/quizzes_api.create)
for options.}
}
\value{
Response from Canvas.
}
\description{
Change settings for existing Canvas quizzes.
}
