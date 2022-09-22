#make future predictions
makePred <- function(Season, Week){
  game <- subset(modelData, season == Season & week == Week)
  weekPreds <- predict(model, game[])
  weekResults<-data.frame(game[,1:4], weekPreds, game[,6])
  names(weekResults)<-c('Season', 'Week', 'Away', 'Home', 'Predicted', 'OU')
  weekResults<-transform(weekResults,'Difference'= ifelse(Predicted>0, Predicted-OU, 0))
  weekResults<-transform(weekResults,'Bet' = if_else(weekResults[,7] > 0, 'Over', 'Under', 'No data'))
  weekResults
}


makePred(2022, 3)