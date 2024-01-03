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

# collect list of all materials
materials_list <- purrr::map(course_settings$schedule, "materials") |>
  purrr::list_flatten()

# Get only things that have a path field
local_files_list <- purrr::keep(materials_list, ~"path" %in% names(.))


# Deal with assignments and things that have an href field 
# instead of a path field
materials <- materials_list |>
  purrr::keep(~"href" %in% names(.)) |>
  purrr::map_df(as.data.frame) |>
  tidyr::nest(categories = categories) |>
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
    pub.date = ifelse(is.na(pub.date),
                      date - lubridate::weeks(publish_week_before),
                      pub.date) |> lubridate::as_datetime(tz = timezone),
    is_live = (pub.date <= publish_cutoff) & !is.na(href),
    file_date = lubridate::ymd(date, tz = timezone),
    categories = purrr::map2(categories, name, ~c(as.character(unlist(.x)), .y))
  )

include_files <- dplyr::filter(materials, is_live)

## This writes out yaml for things that don't have href fields
local_files_df <- local_files_list |>
  purrr::map_df(as.data.frame) |>
  dplyr::mutate_at(
    .vars = c("date", "pub.date"), 
    lubridate::ymd, tz=timezone
  ) |>
  dplyr::mutate(
    week_start = lubridate::floor_date(date, "week", week_start = 7)
  ) |>
  dplyr::left_join(week_mapping) |>
  dplyr::mutate(
    pub.date = ifelse(is.na(pub.date),
                      date - lubridate::weeks(publish_week_before),
                      pub.date) |> lubridate::as_datetime(tz = timezone),
    is_live = (pub.date <= publish_cutoff) & !is.na(path),
    file_date = lubridate::ymd(date, tz = timezone),
    type = categories,
    categories = purrr::map2(categories, name, ~c(as.character(unlist(.x)), .y))
  ) |>
  dplyr::mutate(image = sprintf("figs/%s.svg", tolower(type))) |>
  dplyr::select(path, title, date, pub.date, categories, type, image, is_live) 


source("scripts/change-yml.R")

include_links <- local_files_df |>
  dplyr::filter(is_live) |>
  split(1:sum(local_files_df$is_live)) |>
  purrr::map(as.list) 

include_links <- purrr::modify(include_links, ~purrr::modify_in(., "categories", unlist))

names(include_links) <- NULL
yaml::write_yaml(include_links, "reading.yml", handlers = yml_handlers)


# Write out exclude files for homeworks and assignments that aren't yet relevant
idx_lines <- readLines("index.qmd")
yaml_lines <- which(idx_lines == "---")
index_yml <- yaml::read_yaml(text = idx_lines[min(yaml_lines):max(yaml_lines)])
index_yml$listing[[1]]$contents = c(include_files$href, "reading.yml")
index_yml$input_file <- "index.qmd"
index_yml$output_file <- "index.qmd"


# Exclude unpublished stuff from the listing by updating the yaml
do.call("change_yaml_matter", index_yml)


# This bit renames files with _ in front to actually prevent them from rendering
# This sometimes screws with quarto's render process, especially anytime 
# something changes. 

# renamed_files <- materials |>
#   dplyr::mutate(
#     show_file = stringr::str_remove(file, "^_"),
#     hide_file = paste0("_", show_file),
#     is_visible = file.exists(file.path(dir, show_file)),
#     current_path = ifelse(is_visible, file.path(dir, show_file), file.path(dir, hide_file)),
#     rename_path = ifelse(is_live, file.path(dir, show_file), file.path(dir, hide_file))
#   )
#   dplyr::filter(is_live != is_visible) |>
#   dplyr::mutate(renamed = purrr::map2_lgl(current_path, rename_path, file.rename))
# 
# if (nrow(renamed_files) > 0) {
#   cli::cli_alert_success("The following files have publish dates after {publish_cutoff} and therefore are ignored: {renamed_files$rename_path}")
# } else {
#   cli::cli_alert_info("All files have published dates in the past. No files will be ignored.")
# }
