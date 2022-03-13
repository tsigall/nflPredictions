library(xlsx)

resultsExtended <- data.frame(modelData[,1:4], modelData[,7:26], predictions, modelData[,5], abs(predictions - modelData[,5]), modelData[,6])
names(resultsExtended)[25:28] <- c('Predicted', 'Actual', 'Error', 'OU')
resultsExtended<-transform(resultsExtended,'Bet'= ifelse(predictions>0, Predicted-OU, 0))
resultsExtended<-transform(resultsExtended,'Result'=ifelse(Predicted>0, Actual-OU, 0))
resultsExtended<-transform(resultsExtended,'Win'=ifelse(Bet*Result>0,1,0))

write.xlsx2(resultsExtended, "C:\\Users\\Thoma\\OneDrive - Friends and Family Dogfood sigall.org\\Examining Results.xlsx", sheetName = "Sheet1",
            col.names = TRUE, row.names = TRUE, append = FALSE)