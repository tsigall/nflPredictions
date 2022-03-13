#install packages
library(nflverse)
library(tidyverse)
library(zoo)
library(xlsx)

#Load data
games <- nflreadr::load_schedules()
games <- games %>%
  filter(game_type == 'REG') %>%
  mutate(
    away_team = case_when(
      away_team == 'OAK' ~ 'LV',
      away_team == 'SD' ~ 'LAC',
      away_team == 'STL' ~ 'LA',
      TRUE ~ away_team
    )
  ) %>%
  mutate(
    home_team = case_when(
      home_team == 'OAK' ~ 'LV',
      home_team == 'SD' ~ 'LAC',
      home_team == 'STL' ~ 'LA',
      TRUE ~ home_team
    )
  )
pbp <- nflfastR::load_pbp(1999:2021, qs = TRUE) %>%
  dplyr::filter(season_type == "REG") %>%
  dplyr::filter(!is.na(posteam) & (rush == 1 | pass == 1)) %>%
  select(posteam,
         defteam, 
         season, 
         week,
         score_differential,
         qtr,
         down,
         half_seconds_remaining, 
         rushing_yards, 
         rush_attempt, 
         passing_yards, 
         pass_attempt,
         fumble, 
         interception,
         epa)

#filters
pbpFilter <- pbp %>%
  filter(down < 4, qtr < 4, half_seconds_remaining > 120) 


#Select stats used for the model and group by offense and defense
offense <- pbpFilter %>%
  dplyr::group_by(posteam, season, week) %>%
  dplyr::summarise(off_rya = sum(rushing_yards, na.rm = TRUE)/sum(rush_attempt, na.rm = TRUE), 
                   off_pya = sum(passing_yards, na.rm = TRUE)/sum(pass_attempt, na.rm = TRUE), 
                   off_fum = sum(fumble, na.rm = TRUE),  
                   off_int = sum(interception, na.rm = TRUE),
                   off_epa = mean(epa, na.rm = TRUE)) %>%
  group_by(posteam)%>%
  mutate(
    pw_orya = rollmean(off_rya, 10,  na.pad =TRUE, align='right'),
    pw_orya = lag(pw_orya),
    pw_opya = rollmean(off_pya, 10,  na.pad =TRUE, align='right'),
    pw_opya = lag(pw_opya),
    pw_ofum = rollmean(off_fum, 10,  na.pad =TRUE, align='right'),
    pw_ofum = lag(pw_ofum),
    pw_oint = rollmean(off_int, 10,  na.pad =TRUE, align='right'),
    pw_oint = lag(pw_oint),
    pw_oepa = rollmean(off_epa, 10,  na.pad =TRUE, align='right'),
    pw_oepa = lag(pw_oepa)
  )
defense <- pbpFilter %>%
  dplyr::group_by(defteam, season, week) %>%
  dplyr::summarise(def_rya = sum(rushing_yards, na.rm = TRUE)/sum(rush_attempt, na.rm = TRUE), 
                   def_pya = sum(passing_yards, na.rm = TRUE)/sum(pass_attempt, na.rm = TRUE), 
                   def_fum = sum(fumble, na.rm = TRUE),  
                   def_int = sum(interception, na.rm = TRUE),
                   def_epa = mean(epa, na.rm = TRUE)) %>%
  group_by(defteam)%>%
  mutate(
    pw_drya = rollmean(def_rya, 10,  na.pad =TRUE, align='right'),
    pw_drya = lag(pw_drya),
    pw_dpya = rollmean(def_pya, 10,  na.pad =TRUE, align='right'),
    pw_dpya = lag(pw_dpya),
    pw_dfum = rollmean(def_fum, 10,  na.pad =TRUE, align='right'),
    pw_dfum = lag(pw_dfum),
    pw_dint = rollmean(def_int, 10,  na.pad =TRUE, align='right'),
    pw_dint = lag(pw_dint),
    pw_depa = rollmean(def_epa, 10,  na.pad =TRUE, align='right'),
    pw_depa = lag(pw_depa)
  )

#Combine offense and defense data
pg_defense <- defense[,c(1:3,9:13)]
pg_offense <- offense[,c(1:3,9:13)]
rawModelData <- inner_join(pg_offense, pg_defense, by = c('season', 'week', 'posteam' = 'defteam'))

#Combine data with schedule
modelData <- games[,c(2, 4, 8, 10, 14, 45, 29, 13, 26)]
modelData <- left_join(modelData, rawModelData, by = c('season', 'week', 'away_team' = 'posteam'))
modelData <- modelData %>%
  rename(
    a_orya = pw_orya,
    a_drya = pw_drya,
    a_opya = pw_opya,
    a_dpya = pw_dpya,
    a_ofum = pw_ofum,
    a_dfum = pw_dfum,
    a_oint = pw_oint,
    a_dint = pw_dint,
    a_oepa = pw_oepa,
    a_depa = pw_depa
  )
modelData <- left_join(modelData, rawModelData, by = c('season', 'week', 'home_team' = 'posteam'))
modelData <- modelData %>%
  rename(
    h_orya = pw_orya,
    h_drya = pw_drya,
    h_opya = pw_opya,
    h_dpya = pw_dpya,
    h_ofum = pw_ofum,
    h_dfum = pw_dfum,
    h_oint = pw_oint,
    h_dint = pw_dint,
    h_oepa = pw_oepa,
    h_depa = pw_depa
  ) %>%
  ungroup() %>%
  na.omit()

#Fit to a lm model
spreadModel <- lm(result ~
                    a_drya +
                    a_opya +
                    a_dpya +
                    a_ofum +
                    a_dfum +
                    a_oepa +
                    h_drya +
                    h_opya +
                    h_dpya +
                    h_ofum +
                    h_dfum +
                    h_oepa,
                  data = modelData)

#Test multiple regression
predictions<-predict(object=spreadModel, modelData[,c(7:29)])
results<-data.frame(predictions, modelData[,8], modelData[,9])
names(results)<-c('Predicted','Actual', 'Spread')
results <- results %>% mutate(
  Bet = Predicted - Spread,
  Result = Actual - Spread
)
results<-transform(results,'Win'=ifelse(Bet*Result>0,1,0))
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


