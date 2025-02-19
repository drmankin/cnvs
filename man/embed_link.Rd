% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helpers.R
\name{embed_link}
\alias{embed_link}
\title{Create an in-text link to various Canvas destinations}
\usage{
embed_link(
  link_text,
  dest,
  search_term,
  type,
  web_url = "https://r-training.netlify.app",
  url = NULL
)
}
\arguments{
\item{link_text}{The text to be hyperlinked}

\item{dest}{The desination, one of:
- "website", which will link to `web_url`
- One of the normal Canvas elements (e.g. "Assignments", "Quizzes")
- "Zoom" or "Panopto Recordings"
- A text string that matches the machine-readable page name in the Canvas
site returned by `search_term`
 If missing, `url` must be provided for a custom link.}

\item{search_term}{Passed to [get_module_id()] to return the module ID}

\item{type}{What kind of link to produce - "markdown" or "html"}

\item{web_url}{URL to use for "website"}

\item{url}{The end of a Canvas URL - the first bit will be added
automatically up to ".../courses/module_id/".}
}
\value{
An embedded link text string
}
\description{
Convenience function to use as inline code to quickly insert embedded
hyperlinks
}
