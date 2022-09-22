predData <- modelData %>%
  filter(season == 2021)
predictions <- predict(object = model, predData[], interval = 'prediction', level = 0.53)
results<-data.frame(predData['away_team'], predData['home_team'], predData['total_line'], predictions)
results <- results %>% 
  mutate(margin = fit - total_line,
         bet = if_else(margin < 0,
                       if_else(total_line < lwr,
                               bet = "Under",
                               bet = "None"),
                       if_else(total_line > upr,
                               bet = "Over",
                               bet = "None"))
         )
as_tibble(results)
