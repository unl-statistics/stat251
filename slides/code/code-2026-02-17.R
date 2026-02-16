library(tidytuesdayR)
library(ggplot2)
library(gt)

tuesdata <- tidytuesdayR::tt_load('2026-02-17')$dataset
tuesdata |>
  dplyr::mutate(year = year_ended_june) |>
  dplyr::filter(year >= 1980) |>
  dplyr::filter(measure == "Total Sheep") |>
  ggplot(aes(x = year, y = value)) +
  geom_point(aes(colour = "# sheep in NZ")) +
  geom_point(aes(x = year, y = population,
                 colour ="# people in NZ"),
             data = countrypops |> dplyr::filter(country_name == "New Zealand", year >= 1980)) +
  theme_bw()


tuesdata |> dplyr::filter(year_ended_june %in% c(1980, 2023), measure == "Total Sheep")

countrypops |> dplyr::filter(country_name == "New Zealand", year %in% c(1980, 2023))

tuesdata |> count(measure) |> slice(grep("deer", measure))


tuesdata |>
  dplyr::mutate(year = year_ended_june) |>
  dplyr::filter(year >= 1980) |>
  dplyr::filter(measure %in% c("Total female deer", "Total male deer")) |>
  ggplot(aes(x = year, y = value)) +
  geom_point(aes(colour = measure)) +
  geom_point(aes(x = year, y = population,
                 colour ="# people in NZ"),
             data = countrypops |> dplyr::filter(country_name == "New Zealand", year >= 1980)) +
  theme_bw()
