#make future predictions
makePred <- function(Season, Week){
  game <- subset(modelData, season == Season & week == Week)
  weekPreds <- predict(model, game[])
  weekResults<-data.frame(game[,1:4], round(weekPreds, 2), game[,6])
  names(weekResults)<-c('Season', 'Week', 'Away', 'Home', 'Pred.', 'OU')
  weekResults<-transform(weekResults,'Diff.'= ifelse(Pred.>0, Pred.-OU, 0))
  weekResults<-transform(weekResults,'Bet' = if_else(weekResults[,7] > 0, 'Over', 'Under', 'No data'))
  weekResults
}


makePred(2022, 4)
