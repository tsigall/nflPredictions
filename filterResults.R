filter <- resultsExtended
filter <- resultsExtended %>% filter((away_team == 'GB' | home_team == 'GB')
                                     & season > 2016)
filter %>%
  group_by(season) %>%
  dplyr::summarise(pct = sum(Win)) %>%
  mutate(pct = if_else(season < 2021, pct/16, pct/17))

mean(filter$Win)
