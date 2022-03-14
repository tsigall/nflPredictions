#Write results to excel file
resultsExtended <- data.frame(modelData[,c(1:6)], modelData[,c(10,11,13:14,20,21,23:26)], predictions, modelData[,5], modelData[,7])
names(resultsExtended)[17:19] <- c('Predicted', 'Actual', 'Spread')
resultsExtended <- resultsExtended %>% mutate(
  Bet = Predicted - Spread,
  Result = Actual - Spread
)
resultsExtended<-transform(resultsExtended,'Win'=ifelse(Bet*Result>0,1,0))

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
           h_oqepa) %>%
  mutate(
    Predicted = predictions,
    Actual = modelData[,5],
    Spread = modelData[,6]
  )
resultsExtended <- resultsExtended %>% mutate(
  Bet = Predicted - Spread.total_line,
  Result = Actual.total - Spread.total_line
)
resultsExtended<-transform(resultsExtended,'Win'=ifelse(Bet*Result>0,1,0))

write.xlsx2(resultsExtended, "C:\\Users\\Thoma\\OneDrive - Friends and Family Dogfood sigall.org\\Examining Results.xlsx", sheetName = "Data",
            col.names = TRUE, row.names = TRUE, append = FALSE)
