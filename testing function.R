testModel <- function(toTest){
  testData <- modelData %>%
    na.omit()
  predictions<-predict(object=toTest, testData[])
  results<-data.frame(predictions, testData[,5], abs(predictions - testData[,5]), testData[,6])
  names(results)<-c('Predicted','Actual','Error', 'OU')
  results<-transform(results,'Bet'= ifelse(Predicted>0, Predicted-OU, 0))
  results<-transform(results,'Result'=ifelse(Predicted>0, Actual-OU, 0))
  results<-transform(results,'Win'=ifelse(Bet*Result>0,1,0))
  mean(results$Error)
  final<-data.frame(Margin = c('All','3','5','7','10'),
                    Win = c(sum(results$Win == 1),
                            sum(results$Win == 1 & (results$Bet > 3 | results $Bet < -3)),
                            sum(results$Win == 1 & (results$Bet > 5 | results $Bet < -5)),
                            sum(results$Win == 1 & (results$Bet > 7 | results $Bet < -7)),
                            sum(results$Win == 1 & (results$Bet > 10 | results $Bet < -10))),
                    Lose = c(sum(results$Win == 0),
                             sum(results$Win == 0 & (results$Bet > 3 | results $Bet < -3)),
                             sum(results$Win == 0 & (results$Bet > 5 | results $Bet < -5)),
                             sum(results$Win == 0 & (results$Bet > 7 | results $Bet < -7)),
                             sum(results$Win == 0 & (results$Bet > 10 | results $Bet < -10))))
  final<-transform(final,'Total'=ifelse(Win>-1,Win+Lose,0))
  final<-transform(final,'Pct.'=ifelse(Win>-1,Win/Total*100))
  final
  
  margins <- data.frame(Margin = c('0', '3', '5', '7', '10+', 'Total'),
                        Win = c(final[1,2] - final[2,2],
                                final[2,2] - final[3,2],
                                final[3,2] - final[4,2],
                                final[4,2] - final[5,2],
                                final[5,2],
                                final[1,2]),
                        Lose = c(final[1,3] - final[2,3],
                                 final[2,3] - final[3,3],
                                 final[3,3] - final[4,3],
                                 final[4,3] - final[5,3],
                                 final[5,3],
                                 final[1,3])) %>%
    transform('Total'=ifelse(Win>-1,Win+Lose,0)) %>%
    transform('Pct.'=ifelse(Win>-1,Win/Total*100))
  
  margins
  
  
}
testModel(model)
