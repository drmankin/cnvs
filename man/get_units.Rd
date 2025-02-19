% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/units.R
\name{get_units}
\alias{get_units}
\title{Return all the units ("modules") on a Canvas site}
\usage{
get_units(module_id, search_term = NULL, id_only = FALSE)
}
\arguments{
\item{module_id}{Canvas module ID number}

\item{search_term}{String to search for in the unit name}

\item{id_only}{Whether to return the unit ID only. Best used with a specific search term.}
}
\value{
A tibble of all units matching the search term, if provided; or only ID code(s), if id_only is TRUE.
}
\description{
Return all the units ("modules") on a Canvas site
}
