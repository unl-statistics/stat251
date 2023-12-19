# This script populates dates from _schedule.yml to the yml files themselves

source("scripts/change-yml.R")

# read in course settings
course_settings <- yaml::read_yaml("_schedule.yml")


update_yaml_params <- function(lst) {
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
  do.call("change_yaml_matter", params)
}

materials <- purrr::map(course_settings$schedule, "materials") |>
  purrr::list_flatten() |>
  purrr::map(update_yaml_params)
