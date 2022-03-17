filter <- resultsExtended
filter <- resultsExtended %>% filter(season > 2006)
filter %>%
  group_by(season) %>%
  dplyr::summarise(pct = sum(Win)) %>%
  mutate(pct = if_else(season < 2021, pct/256, pct/272))

mean(filter$Win)

windows()
par(mfrow=c(2,2))
plot(model)

x<-modelData[,c(5,7:48)]
windows()
cor <- cor(x)
omcdiag(model)
imcdiag(model)
vif(model)
