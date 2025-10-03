get_sussex_dates <- function(which = c("current_year", "min_service", "next_year"),
                             teaching_only = FALSE){

  which <- switch(
    which,
    current_year = 1,
    min_service = 2,
    next_year = 3
  )

  session <- rvest::session("https://www.sussex.ac.uk/about/term-dates")

  page <- xml2::read_html(session)

  dates <- page |>
    rvest::html_nodes("table") |>
    rvest::html_table() |>
    magrittr::extract2(which)

  dates <- dplyr::rename_with(dates, ~c("term", "event", "date")) |>
    dplyr::mutate(event = tolower(event))

  if(teaching_only == TRUE){
    dates <- dates |>
      dplyr::filter(grepl("teaching", event) | grepl("(spring|summer) term", event)) |>
      dplyr::filter(event != "spring term begins")
  }

  return(dates)
}

parse_sussex_dates <- function(date){lubridate::parse_date_time(date, orders = "%a %d %b %Y")}


create_teaching_calendar <- function(which){

  if(missing(which)){
    stop("You must indicate either current_year or next_year")
  }

  dates <- get_sussex_dates(which, teaching_only = TRUE)

  ## Autumn Term (no break)
  autumn_start <- dates |>
    dplyr::filter(grepl("One", term) & grepl("begins", event)) |>
    dplyr::pull(date) |>
    parse_sussex_dates() |>
    as.Date()

  autumn_weeks <- 11

  autumn_calendar <- tibble::tibble(
    term = "autumn",
    week = 1:autumn_weeks,
    week_start = seq.Date(from = autumn_start, by = "week", length.out = autumn_weeks),
    week_end = week_start + lubridate::days(4)
  )

  ## Spring Term (with break)

  spring_start <- dates |>
    dplyr::filter(grepl("Two", term) & grepl("teaching begins", event)) |>
    dplyr::pull(date) |>
    parse_sussex_dates() |>
    as.Date()

  easter_start <- dates |>
    dplyr::filter(grepl("Two", term) & grepl("spring term ends", event)) |>
    dplyr::pull(date) |>
    parse_sussex_dates() |>
    as.Date()

  spring_weeks <- 14

  spring_calendar <- tibble::tibble(
    term = "spring",
    week = 1:spring_weeks,
    week_start = seq.Date(from = spring_start, by = "week", length.out = spring_weeks),
    week_end = week_start + lubridate::days(4)
  )

  easter_week <- spring_calendar |>
    dplyr::filter(week_end == easter_start) |>
    dplyr::pull(week)

  break_weeks <- seq(from = easter_week + 1, length.out = 3)

  spring_calendar <- spring_calendar |>
    dplyr::filter(!week %in% break_weeks) |>
    dplyr::mutate(week = 1:autumn_weeks)

  term_calendars <- list(
    "autumn" = autumn_calendar,
    "spring" = spring_calendar,
    "spring_break" = paste0("This year, the spring holiday begins after Week ", easter_week, ".")
  )

  return(term_calendars)
}

