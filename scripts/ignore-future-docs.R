# From https://github.com/stat20/stat20/blob/main/assets/scripts/ignore-future-docs.r

# read in course settings
course_settings <- yaml::read_yaml("_schedule.yml")

# extract auto-publish parameters
timezone <- course_settings$`auto-publish`$timezone
publish_week_before <- course_settings$`auto-publish`$`publish-week-before`
live_as_of <- course_settings$`auto-publish`$`live-as-of`


# Set up weeks to map to dates.
week_mapping <- course_settings$calendar |>
  purrr::map_df(as.data.frame) |>
  dplyr::mutate(date = lubridate::ymd(date, tz = timezone),
                week_start = date,
                week_end = date + lubridate::days(7)) |>
  dplyr::select(-date)


publish_cutoff <- if (live_as_of == "Sys.time()") {
  lubridate::with_tz(time = eval(parse(text = live_as_of)),
                     tz = timezone)
} else {
  lubridate::ymd(live_as_of, tz = timezone)
}

weeks_to_publish <- dplyr::filter(
  week_mapping,
  week_start <= publish_cutoff + lubridate::weeks(publish_week_before)
)
weeks_to_exclude <- dplyr::anti_join(week_mapping, weeks_to_publish)

# collect data frame of all materials
materials <- purrr::map(course_settings$schedule, "materials") |>
  purrr::list_flatten() |>
  purrr::map_df(as.data.frame) |>
  tidyr::nest(categories = categories)

readings <- dplyr::filter(materials, is.na(href))

materials <- materials |>
  dplyr::filter(!is.na(href)) |>
  dplyr::arrange(href) |>
  dplyr::mutate_at(
    .vars = c("date", "pub.date"),
    lubridate::ymd, tz=timezone
  ) |>
  dplyr::mutate(
    week_start = lubridate::floor_date(date, "week", week_start = 7)
  ) |>
  dplyr::left_join(week_mapping) |>
  dplyr::mutate(
    dir = dirname(href),
    file = basename(href),
    # show_file = stringr::str_remove(file, "^_"),
    # hide_file = paste0("_", show_file),
    pub.date = ifelse(is.na(pub.date),
                      date - lubridate::weeks(publish_week_before),
                      pub.date) |> lubridate::as_datetime(tz = timezone),
    is_live = (pub.date <= publish_cutoff) & !is.na(href),
    # is_visible = file.exists(file.path(dir, show_file)),
    file_date = lubridate::ymd(date, tz = timezone),
    # current_path = ifelse(is_visible, file.path(dir, show_file), file.path(dir, hide_file)),
    # rename_path = ifelse(is_live, file.path(dir, show_file), file.path(dir, hide_file))
    categories = purrr::map2(categories, name, ~c(as.character(unlist(.x)), .y))
  )

hidden_materials <- dplyr::filter(materials, !is_live) |> tidyr::unnest(categories)

live_materials <- dplyr::filter(materials, is_live) |> tidyr::unnest(categories)

exclude_cats <- dplyr::anti_join(hidden_materials, live_materials, by = "categories")$categories |> unique()


source("scripts/change-yml.R")

idx_lines <- readLines("index.qmd")
yaml_lines <- which(idx_lines == "---")
index_yml <- yaml::read_yaml(text = idx_lines[min(yaml_lines):max(yaml_lines)])
index_yml$listing[[1]]$exclude = list(categories = weeks_to_exclude$name)
index_yml$input_file <- "index.qmd"
index_yml$output_file <- "index.qmd"

# Exclude unpublished stuff from the listing by updating the yaml
do.call("change_yaml_matter", index_yml)

# renamed_files <- materials |>
#   dplyr::filter(is_live != is_visible) |>
#   dplyr::mutate(renamed = purrr::map2_lgl(current_path, rename_path, file.rename))
#
# if (nrow(renamed_files) > 0) {
#   cli::cli_alert_success("The following files have publish dates after {publish_cutoff} and therefore are ignored: {renamed_files$rename_path}")
# } else {
#   cli::cli_alert_info("All files have published dates in the past. No files will be ignored.")
# }
