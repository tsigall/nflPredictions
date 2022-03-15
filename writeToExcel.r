#for spread model
resultsExtended <- modelData %>%
  select(season,
         week,
         away_team,
         home_team,
         a_oqepa,
         a_dqepa,
         h_oqepa,
         h_dqepa,
         Actual = result,
         Spread = spread_line) %>%
  mutate(
    Predicted = predictions
  )
resultsExtended <- resultsExtended %>% mutate(
  Bet = Predicted - Spread,
  Result = Actual - Spread,
  Win = if_else(Bet*Result>0,1,0)
)
write.xlsx2(resultsExtended, "C:\\Users\\Thoma\\OneDrive - Friends and Family Dogfood sigall.org\\Examining Results.xlsx", sheetName = "Data",
            col.names = TRUE, row.names = TRUE, append = FALSE)

#for o/u model
resultsExtended <- modelData %>%
  select(a_drya,
           a_dpya,
           a_ofum,
           a_dfum,
           a_oqepa,
           a_drya,
           h_drya,
           h_opya,
           h_dpya,
           h_ofum,
           h_dfum,
           h_oqepa,
           Actual = result,
           Spread = spread_line) %>%
  mutate(
    Predicted = predictions
  )
resultsExtended <- resultsExtended %>% mutate(
  Bet = Predicted - Spread,
  Result = Actual - Spread,
  Win = if_else(Bet*Result>0,1,0),
  Win = if_else(Actual == Spread & Win == 0, 0.5, Win)
)
write.xlsx2(resultsExtended, "C:\\Users\\Thoma\\OneDrive - Friends and Family Dogfood sigall.org\\Examining Results.xlsx", sheetName = "Data",
            col.names = TRUE, row.names = TRUE, append = FALSE)

