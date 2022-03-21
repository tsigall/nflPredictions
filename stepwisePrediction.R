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
pbp <- nflfastR::load_pbp(2006:2021, qs = TRUE) %>%
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
         epa,
         qb_epa,
         cpoe)

qbData <- read.csv("C:\\Users\\Thoma\\OneDrive - Friends and Family Dogfood sigall.org\\Examining Results.csv")
qbData <- qbData %>%
  mutate(
    posteam = case_when(
      Tm == 'GNB' ~ 'GB',
      Tm == 'KAN' ~ 'KC',
      Tm == 'LAR' ~ 'LA',
      Tm == 'OAK' ~ 'LVR',
      Tm == 'NOR' ~ 'NO',
      Tm == 'NWE' ~ 'NE',
      Tm == 'SFO' ~ 'SF',
      Tm == 'TAM' ~ 'TB',
      TRUE ~ Tm
    )
  ) %>% mutate(
    defteam = case_when(
      Opp == 'GNB' ~ 'GB',
      Opp == 'KAN' ~ 'KC',
      Opp == 'LAR' ~ 'LA',
      Opp == 'OAK' ~ 'LVR',
      Opp == 'NOR' ~ 'NO',
      Opp == 'NWE' ~ 'NE',
      Opp == 'SFO' ~ 'SF',
      Opp == 'TAM' ~ 'TB',
      TRUE ~ Opp
    )
  ) %>%
  select (
    posteam,
    defteam,
    Player,
    Season,
    Week,
    Cmp,
    Att,
    Cmp.,
    Yds,
    TD,
    Int,
    Rate,
    Sk,
    Y.A,
    AY.A
  )
#filters
pbpFilter <- pbp %>%
  filter(season > 2016) 


#Select stats used for the model and group by offense and defense
offense <- pbpFilter %>%
  dplyr::group_by(posteam, season, week) %>%
  dplyr::summarise(off_rya = sum(rushing_yards, na.rm = TRUE)/sum(rush_attempt, na.rm = TRUE), 
                   off_pya = sum(passing_yards, na.rm = TRUE)/sum(pass_attempt, na.rm = TRUE), 
                   off_fum = sum(fumble, na.rm = TRUE),  
                   off_int = sum(interception, na.rm = TRUE),
                   off_epa = mean(epa, na.rm = TRUE),
                   off_qepa = mean(qb_epa),
                   off_cpoe = mean(cpoe, na.rm = T)) %>%
  group_by(posteam)%>%
  mutate(
    pw_orya = rollmean(off_rya, 10,  na.pad =TRUE, align='right'),
    pw_orya = lag(pw_orya),
    pw_opya = rollmean(off_pya, 10,  na.pad =TRUE, align='right'),
    pw_opya = lag(pw_opya),
    pw_ofum = rollmean(off_fum, 10,  na.pad =TRUE, align='right'),
    pw_ofum = lag(pw_ofum),
    pw_oepa = rollmean(off_epa, 10,  na.pad =TRUE, align='right'),
    pw_oepa = lag(pw_oepa),
    pw_oqepa = rollmean(off_qepa, 10,  na.pad =TRUE, align='right'),
    pw_oqepa = lag(pw_oqepa),
    pw_ocpoe = rollmean(off_cpoe, 10,  na.pad =TRUE, align='right'),
    pw_ocpoe = lag(pw_ocpoe)
  ) %>%
  ungroup()
offense <- inner_join(offense, qbData, by = c('season' = 'Season', 'week' = 'Week', 'posteam'))

offense <- offense %>%
  group_by(Player) %>%
  mutate(
    pw_ocmp = rollmean(Cmp, 10,  na.pad =TRUE, align='right'),
    pw_ocmp = lag(pw_ocmp),
    pw_oatt = rollmean(Att, 10,  na.pad =TRUE, align='right'),
    pw_oatt = lag(pw_oatt),
    pw_ocmpct = rollmean(Cmp., 10,  na.pad =TRUE, align='right'),
    pw_ocmpct = lag(pw_ocmpct),
    pw_oyds = rollmean(Yds, 10,  na.pad =TRUE, align='right'),
    pw_oyds = lag(pw_oyds),
    pw_otd = rollmean(TD, 10,  na.pad =TRUE, align='right'),
    pw_otd = lag(pw_otd),
    pw_oint = rollmean(Int, 10,  na.pad =TRUE, align='right'),
    pw_oint = lag(pw_oint),
    pw_orat = rollmean(Rate, 10,  na.pad =TRUE, align='right'),
    pw_orat = lag(pw_orat),
    pw_oyatt = rollmean(Y.A, 10,  na.pad =TRUE, align='right'),
    pw_oyatt = lag(pw_oyatt),
    pw_oaya = rollmean(AY.A, 10,  na.pad =TRUE, align='right'),
    pw_oaya = lag(pw_oaya),
    qb = Player
  ) %>%
  ungroup()

defense <- pbpFilter %>%
  dplyr::group_by(defteam, season, week) %>%
  dplyr::summarise(def_rya = sum(rushing_yards, na.rm = TRUE)/sum(rush_attempt, na.rm = TRUE), 
                   def_pya = sum(passing_yards, na.rm = TRUE)/sum(pass_attempt, na.rm = TRUE), 
                   def_fum = sum(fumble, na.rm = TRUE),  
                   def_int = sum(interception, na.rm = TRUE),
                   def_epa = mean(epa, na.rm = TRUE),
                   def_qepa = mean(qb_epa),
                   def_cpoe = mean(cpoe, na.rm = T)) %>%
  group_by(defteam)%>%
  mutate(
    pw_drya = rollmean(def_rya, 10,  na.pad =TRUE, align='right'),
    pw_drya = lag(pw_drya),
    pw_dpya = rollmean(def_pya, 10,  na.pad =TRUE, align='right'),
    pw_dpya = lag(pw_dpya),
    pw_dfum = rollmean(def_fum, 10,  na.pad =TRUE, align='right'),
    pw_dfum = lag(pw_dfum),
    pw_depa = rollmean(def_epa, 10,  na.pad =TRUE, align='right'),
    pw_depa = lag(pw_depa),
    pw_dqepa = rollmean(def_qepa, 10,  na.pad =TRUE, align='right'),
    pw_dqepa = lag(pw_dqepa),
    pw_dcpoe = rollmean(def_cpoe, 10,  na.pad =TRUE, align='right'),
    pw_dcpoe = lag(pw_dcpoe),
  ) %>%
  ungroup()
defense <- inner_join(defense, qbData, by = c('season' = 'Season', 'week' = 'Week', 'defteam'))

defense <- defense %>%
  group_by(Player) %>%
  mutate(
    pw_dcmp = rollmean(Cmp, 10,  na.pad =TRUE, align='right'),
    pw_dcmp = lag(pw_dcmp),
    pw_datt = rollmean(Att, 10,  na.pad =TRUE, align='right'),
    pw_datt = lag(pw_datt),
    pw_dcmpct = rollmean(Cmp., 10,  na.pad =TRUE, align='right'),
    pw_dcmpct = lag(pw_dcmpct),
    pw_dyds = rollmean(Yds, 10,  na.pad =TRUE, align='right'),
    pw_dyds = lag(pw_dyds),
    pw_dtd = rollmean(TD, 10,  na.pad =TRUE, align='right'),
    pw_dtd = lag(pw_dtd),
    pw_dint = rollmean(Int, 10,  na.pad =TRUE, align='right'),
    pw_dint = lag(pw_dint),
    pw_drat = rollmean(Rate, 10,  na.pad =TRUE, align='right'),
    pw_drat = lag(pw_drat),
    pw_dyatt = rollmean(Y.A, 10,  na.pad =TRUE, align='right'),
    pw_dyatt = lag(pw_dyatt),
    pw_daya = rollmean(AY.A, 10,  na.pad =TRUE, align='right'),
    pw_daya = lag(pw_daya)
  ) %>% 
  ungroup()

#Combine offense and defense data
pg_defense <- defense %>% 
  select(defteam,
         season,
         week,
         pw_drya,
         pw_dpya,
         pw_dfum,
         pw_depa,
         pw_dqepa,
         pw_dcpoe,
         pw_dcmp,
         pw_datt,
         pw_dcmpct,
         pw_dyds,
         pw_dtd,
         pw_dint,
         pw_drat,
         pw_dyatt,
         pw_daya)
pg_offense <- offense %>% 
  select(posteam,
         season,
         week,
         qb,
         pw_orya,
         pw_opya,
         pw_ofum,
         pw_oepa,
         pw_oqepa,
         pw_ocpoe,
         pw_ocmp,
         pw_oatt,
         pw_ocmpct,
         pw_oyds,
         pw_otd,
         pw_oint,
         pw_orat,
         pw_oyatt,
         pw_oaya)
rawModelData <- inner_join(pg_offense, pg_defense, by = c('season', 'week', 'posteam' = 'defteam'))
rawModelData <- rawModelData[,1:25]

#Combine data with schedule
modelData <- games %>%
  select(season,
         week,
         away_team,
         home_team,
         total,
         total_line,
         away_qb_name,
         home_qb_name)
modelData <- left_join(modelData, rawModelData, by = c('season', 'week', 'away_team' = 'posteam', 'away_qb_name' = 'qb'))
modelData <- modelData %>%
  rename(
    a_orya = pw_orya,
    a_drya = pw_drya,
    a_opya = pw_opya,
    a_dpya = pw_dpya,
    a_ofum = pw_ofum,
    a_dfum = pw_dfum,
    a_oint = pw_oint,
    a_oepa = pw_oepa,
    a_depa = pw_depa,
    a_oqepa = pw_oqepa,
    a_dqepa = pw_dqepa,
    a_ocpoe = pw_ocpoe,
    a_dcpoe = pw_dcpoe,
    a_ocmp = pw_ocmp,
    a_oatt = pw_oatt,
    a_ocmpct = pw_ocmpct,
    a_oyds = pw_oyds,
    a_otd = pw_otd,
    a_oint = pw_oint,
    a_orat = pw_orat,
    a_oyatt = pw_oyatt,
    a_oaya = pw_oaya
  )
modelData <- left_join(modelData, rawModelData, by = c('season', 'week', 'home_team' = 'posteam', 'home_qb_name' = 'qb'))
modelData <- modelData %>%
  rename(
    h_orya = pw_orya,
    h_drya = pw_drya,
    h_opya = pw_opya,
    h_dpya = pw_dpya,
    h_ofum = pw_ofum,
    h_dfum = pw_dfum,
    h_oint = pw_oint,
    h_oepa = pw_oepa,
    h_depa = pw_depa,
    h_oqepa = pw_oqepa,
    h_dqepa = pw_dqepa,
    h_ocpoe = pw_ocpoe,
    h_dcpoe = pw_dcpoe,
    h_ocmp = pw_ocmp,
    h_oatt = pw_oatt,
    h_ocmpct = pw_ocmpct,
    h_oyds = pw_oyds,
    h_otd = pw_otd,
    h_oint = pw_oint,
    h_orat = pw_orat,
    h_oyatt = pw_oyatt,
    h_oaya = pw_oaya
  ) %>%
  ungroup() %>%
  na.omit()
modelData <- modelData %>% distinct()

#Fit to a lm model
model <- lm(total ~
              a_orya +
              a_opya +
              a_ofum +
              a_oepa +
              a_oqepa +
              a_ocpoe +
              a_ocmp +
              a_oatt +
              a_oyds +
              a_otd +
              a_oint +
              a_orat +
              a_oyatt +
              a_oaya +
              a_drya +
              a_dpya +
              a_dfum +
              a_depa +
              a_dqepa +
              a_dcpoe +
              h_orya +
              h_opya +
              h_ofum +
              h_oepa +
              h_oqepa +
              h_ocpoe +
              h_ocmp +
              h_oatt +
              h_oyds +
              h_otd +
              h_oint +
              h_orat +
              h_oyatt +
              h_oaya +
              h_drya +
              h_dpya +
              h_dfum +
              h_depa +
              h_dqepa +
              h_dcpoe,
            data = modelData)
summary(model)
ols_step_both_p(model, details = TRUE, penter=0.2, prem=0.2)
stepwise.model <- lm(total ~ h_oaya + a_otd + h_ocpoe, data = modelData)
summary(stepwise.model)
k <- ols_step_backward_aic(model, details = TRUE)
aic.model <- lm(total ~ a_oepa + a_oqepa + a_otd + a_orat + h_orya + h_ocpoe + h_ocmp + h_oyds + h_orat, data = modelData)
summary(aic.model)
windows()
plot(k)
#Test multiple regression
predictions<-predict(object= aic.model, modelData[])
results<-data.frame(predictions, modelData[,5], abs(predictions - modelData[,5]), modelData[,6])
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
