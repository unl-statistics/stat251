cookies <- read.csv("choc_chip_cookie_ingredients.csv")
cookies |> str()

cookies |> filter(Recipe_Index=="AR_1")

cookies |> group_by(Ingredient) |>
  summarize(avg_rating = mean(Rating, na.rm=T),
            n=n(),
            missing_ratings = sum(is.na(Rating))) |>
  arrange(desc(avg_rating)) |>
  ggplot(aes(x = n-missing_ratings, y=avg_rating, label=Ingredient)) +
  geom_point()


library(plotly)
ggplotly()
