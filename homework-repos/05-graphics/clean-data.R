# This code reads in the data from the groundhog-day.com api and saves it to two CSV files.

library(jsonlite)
library(tidyr)
library(dplyr)
library(readr)

tmp <- fromJSON("https://groundhog-day.com/api/v1/groundhogs/")

groundhogs <- tmp$groundhogs %>%
  tidyr::separate(col = coordinates, into = c("lat", "long"), sep = ",", remove = F) %>%
  mutate_at(c("lat", "long"), .funs = as.numeric)

predictions <- select(groundhogs, id, name, lat, long, isGroundhog, active, predictions) %>%
  unnest(predictions)

write_csv(predictions, "groundhog-predictions.csv")

groundhogs %>%
  select(-predictions) %>%
  write_csv("groundhogs.csv")
