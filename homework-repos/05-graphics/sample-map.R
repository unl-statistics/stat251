library(ggplot2)
library(dplyr)
world <- map_data("world") %>%
  filter(region %in% c("USA", "Canada")) %>%
  filter(long < 0)

ggplot(world, aes(long, lat)) +
  geom_path(aes(group = group), fill = "white") +
  coord_map("sinusoidal", xlim = c(-130, -60), ylim = c(27, 55), clip = "on") +
  geom_point(data = groundhogs, aes(x = long, y = lat))
