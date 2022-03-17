#for spread model
resultsExtended <- modelData %>%
  select(season,
         week,
         away_team,
         home_team,
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
  select(season,
         week,
         away_team,
         home_team,
         Actual = total,
         OU = total_line) %>%
  mutate(
    Predicted = predictions
  )
resultsExtended <- resultsExtended %>% mutate(
  Bet = Predicted - OU,
  Result = Actual - OU,
  Win = if_else(Bet*Result>0,1,0)
)

resultsExtended <- resultsExtended %>%
  add_column(modelData %>% 
               select(away_qb_name,
                      home_qb_name,
                      a_orya,
                      a_opya,
                      a_ofum,
                      a_oepa,
                      a_oqepa,
                      a_ocpoe,
                      a_ocmp,
                      a_oatt,
                      a_ocmpct,
                      a_oyds,
                      a_otd,
                      a_oint,
                      a_orat,
                      a_oyatt,
                      a_oaya,
                      a_drya,
                      a_dpya,
                      a_dfum,
                      a_depa,
                      a_dqepa,
                      a_dcpoe,
                      h_orya,
                      h_opya,
                      h_ofum,
                      h_oepa,
                      h_oqepa,
                      h_ocpoe,
                      h_ocmp,
                      h_oatt,
                      h_ocmpct,
                      h_oyds,
                      h_otd,
                      h_oint,
                      h_orat,
                      h_oyatt,
                      h_oaya,
                      h_drya,
                      h_dpya,
                      h_dfum,
                      h_depa,
                      h_dqepa,
                      h_dcpoe
               ))
write.xlsx2(resultsExtended, "C:\\Users\\Thoma\\OneDrive - Friends and Family Dogfood sigall.org\\Examining Results.xlsx", sheetName = "Data",
            col.names = TRUE, row.names = TRUE, append = FALSE)

