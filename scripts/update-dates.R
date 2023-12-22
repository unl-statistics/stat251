# This script populates dates and weeks from _schedule.yml to the yml files themselves

source("scripts/change-yml.R")

# read in course settings
course_settings <- yaml::read_yaml("_schedule.yml")
timezone <- course_settings$`auto-publish`$timezone

# Set up weeks to map to dates.
week_mapping <- course_settings$calendar |>
  purrr::map_df(as.data.frame) |>
  dplyr::mutate(date = lubridate::ymd(date, tz = timezone),
                week_start = date,
                week_end = date + lubridate::days(7)) |>
  dplyr::select(-date)

update_yaml_params <- function(lst) {
  if(is.null(lst$href)) {
    return(NULL)
  }
  if(!file.exists(lst$href)) {
    cli::cli_alert_info("File {lst$href} does not exist.
                         Checking to see if it has been hidden.")
    lst$href <- file.path(dirname(lst$href), paste0("_", basename(lst$href)))
    if (!file.exists(lst$href)) {
      cli::cli_alert_info("File {lst$href} does not exist. No metadata will be changed.")
      return(NULL)
    }
  }

  params <- lst[-which(names(lst)%in% c("href", "pub-date"))]
  params$input_file <- lst$href
  params$output_file <- lst$href
  if ("date" %in% names(lst)) {
    # Identify which week the assignment falls in
    assign_week <- dplyr::filter(
      week_mapping,
      week_start <= lubridate::ymd(lst$date, tz = timezone),
      week_end >= lubridate::ymd(lst$date, tz = timezone))

    # Update the category accordingly
    if (nrow(assign_week) == 1){
      params$categories <- c(params$categories, assign_week$name)
    }
  }

  do.call("change_yaml_matter", params)
}

materials <- purrr::map(course_settings$schedule, "materials") |>
  purrr::list_flatten() |>
  purrr::map(update_yaml_params)
