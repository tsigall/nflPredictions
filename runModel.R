#install packages
library(nflverse)
library(tidyverse)
library(zoo)
library(olsrr)
library(foreign)
library(MASS)

#load data
schedule <- clean_homeaway(load_schedules()) %>%
  dplyr::select(
    posteam = team,
    season,
    week,
    qb = team_qb_id
  )
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

pbp <- nflfastR::load_pbp(1999:2022) %>%
  dplyr::filter(season_type == "REG") %>%
  dplyr::filter(!is.na(posteam) & (rush == 1 | pass == 1)) %>%
  dplyr::select(posteam,
                defteam, 
                season, 
                week,
                passer_player_id,
                score_differential,
                qtr,
                down,
                half_seconds_remaining, 
                rushing_yards, 
                rush_attempt,
                rush_touchdown,
                passing_yards,
                air_yards,
                yards_after_catch,
                pass_attempt,
                pass_touchdown,
                fumble, 
                interception,
                sack,
                epa)

#filters
pbpFilter <- pbp %>%
  filter(season > 2012)

#select stats used for the model and group by offense and defense
offense <- pbpFilter %>%
  dplyr::group_by(posteam, season, week) %>%
  dplyr::summarise(off_rya = sum(rushing_yards, na.rm = TRUE)/sum(rush_attempt, na.rm = TRUE),
                   off_rtd = sum(rush_touchdown, na.rm = TRUE),
                   off_fum = sum(fumble, na.rm = TRUE),
                   off_epa = mean(epa, na.rm = TRUE),
                   off_yac = mean(yards_after_catch, na.rm = TRUE),
                   off_sack = mean(sack, na.rm = TRUE))%>%
  group_by(posteam)%>%
  mutate(
    pw_orya = rollmean(off_rya, 5,  na.pad =TRUE, align='right'),
    pw_ortd = rollmean(off_rtd, 5,  na.pad =TRUE, align='right'),
    pw_ofum = rollmean(off_fum, 5,  na.pad =TRUE, align='right'),
    pw_oepa = rollmean(off_epa, 5,  na.pad =TRUE, align='right'),
    pw_oyac = rollmean(off_yac, 5,  na.pad =TRUE, align='right'),
    pw_osack = rollmean(off_sack, 5,  na.pad =TRUE, align='right'),
  ) %>%
  ungroup()

qbData <- pbpFilter %>%
  dplyr::group_by(passer_player_id, posteam, season, week) %>%
  dplyr::summarise(pya = sum(passing_yards, na.rm = TRUE)/sum(pass_attempt, na.rm = TRUE),
                   td = sum(pass_touchdown, na.rm = TRUE),
                   int = sum(interception, na.rm = TRUE),
                   air_yds = mean(air_yards, na.rm = TRUE),
                   att = sum(pass_attempt, na.rm = TRUE))%>%
  filter(att > 0) %>%
  group_by(passer_player_id)%>%
  mutate(
    qb_game = row_number(),
    pw_opya = rollmean(pya, 5,  na.pad =TRUE, align='right'),
    pw_optd = rollmean(td, 5,  na.pad =TRUE, align='right'),
    pw_oint = rollmean(int, 5,  na.pad =TRUE, align='right'),
    pw_oayds = rollmean(air_yds, 5,  na.pad =TRUE, align='right'),
    
  ) %>%
  arrange(posteam, season, week) %>%
  ungroup() %>%
  filter(pya > 0) %>%
  distinct()

offense <- left_join(schedule, offense, by = c('season', 'week', 'posteam')) %>%
  filter(season > 2016,
         week < 19) %>%
  arrange(posteam)

offense <- left_join(offense, qbData, by = c('season', 'week', 'posteam', 'qb' = 'passer_player_id')) %>%
  mutate(
    pw_orya = lag(pw_orya),
    pw_ortd = lag(pw_ortd),
    pw_ofum = lag(pw_ofum),
    pw_oepa = lag(pw_oepa),
    pw_oyac = lag(pw_oyac),
    pw_osack = lag(pw_osack),
    pw_opya = lag(pw_opya),
    pw_optd = lag(pw_optd),
    pw_oint = lag(pw_oint),
    pw_oayds = lag(pw_oayds)
  )


defense <- pbpFilter %>%
  dplyr::group_by(defteam, season, week) %>%
  dplyr::summarise(def_rya = sum(rushing_yards, na.rm = TRUE)/sum(rush_attempt, na.rm = TRUE), 
                   def_pya = sum(passing_yards, na.rm = TRUE)/sum(pass_attempt, na.rm = TRUE), 
                   def_rtd = sum(rush_touchdown, na.rm = TRUE),
                   def_ptd = sum(pass_touchdown, na.rm = TRUE),
                   def_sack = sum(sack, na.rm = TRUE),
                   def_fum = sum(fumble, na.rm = TRUE),  
                   def_int = sum(interception, na.rm = TRUE),
                   def_epa = mean(epa, na.rm = TRUE),
                   def_yac = mean(yards_after_catch, na.rm = TRUE),
                   def_ayds = sum(air_yards, na.rm = TRUE),) %>%
  group_by(defteam)%>%
  mutate(
    pw_drya = rollmean(def_rya, 5,  na.pad =TRUE, align='right'),
    pw_dpya = rollmean(def_pya, 5,  na.pad =TRUE, align='right'),
    pw_dfum = rollmean(def_fum, 5,  na.pad =TRUE, align='right'),
    pw_dint = rollmean(def_int, 5,  na.pad =TRUE, align='right'),
    pw_depa = rollmean(def_epa, 5,  na.pad =TRUE, align='right'),
    pw_dsack = rollmean(def_sack, 5,  na.pad =TRUE, align='right'),
    pw_drtd = rollmean(def_rtd, 5,  na.pad =TRUE, align='right'),
    pw_dptd = rollmean(def_ptd, 5,  na.pad =TRUE, align='right'),
    pw_dayds = rollmean(def_ayds, 5,  na.pad =TRUE, align='right'),
    pw_dyac = rollmean(def_yac, 5,  na.pad =TRUE, align='right'),
  ) %>%
  ungroup()

defense <- left_join(schedule, defense, by = c('season', 'week', 'posteam' = 'defteam')) %>%
  filter(season > 2016,
         week < 19) %>%
  arrange(posteam) %>%
  rename(defteam = posteam) %>%
  mutate(pw_drya = lag(pw_drya),
         pw_dpya = lag(pw_dpya),
         pw_dfum = lag(pw_dfum),
         pw_dint = lag(pw_dint),
         pw_depa = lag(pw_depa),
         pw_dsack = lag(pw_dsack),
         pw_drtd = lag(pw_drtd),
         pw_dptd = lag(pw_dptd),
         pw_dayds = lag(pw_dayds),
         pw_dyac = lag(pw_dyac))

pg_defense <- defense %>% 
  dplyr::select(defteam,
                season,
                week,
                pw_drya,
                pw_dpya,
                pw_dfum,
                pw_dint,
                pw_depa,
                pw_dsack,
                pw_drtd,
                pw_dptd,
                pw_dayds,
                pw_dyac
  )
pg_offense <- offense %>% 
  dplyr::select(posteam,
                season,
                week,
                qb,
                pw_orya,
                pw_opya,
                pw_ofum,
                pw_oint,
                pw_oepa,
                pw_osack,
                pw_ortd,
                pw_optd,
                pw_oayds,
                pw_oyac)
rawModelData <- inner_join(pg_offense, pg_defense, by = c('season', 'week', 'posteam' = 'defteam'))


#Combine data with schedule
modelData <- games %>%
  dplyr::select(season,
                week,
                away_team,
                home_team,
                total,
                total_line,
                away_qb_id,
                home_qb_id,
                away_qb_name,
                home_qb_name) %>%
  filter(season > 2016)
modelData <- left_join(modelData, rawModelData, by = c('season', 'week', 'away_team' = 'posteam', 'away_qb_id' = 'qb'))
modelData <- modelData %>%
  rename(
    a_orya = pw_orya,
    a_opya = pw_opya,
    a_ofum = pw_ofum,
    a_oint = pw_oint,
    a_oepa = pw_oepa,
    a_osack = pw_osack,
    a_ortd = pw_ortd,
    a_optd = pw_optd,
    a_oayds = pw_oayds,
    a_oyac = pw_oyac,
    a_drya = pw_drya,
    a_dpya = pw_dpya,
    a_dfum = pw_dfum,
    a_dint = pw_dint,
    a_depa = pw_depa,
    a_dsack = pw_dsack,
    a_drtd = pw_drtd,
    a_dptd = pw_dptd,
    a_dayds = pw_dayds,
    a_dyac = pw_dyac
  )
modelData <- left_join(modelData, rawModelData, by = c('season', 'week', 'home_team' = 'posteam', 'home_qb_id' = 'qb'))
modelData <- modelData %>%
  rename(
    h_orya = pw_orya,
    h_opya = pw_opya,
    h_ofum = pw_ofum,
    h_oint = pw_oint,
    h_oepa = pw_oepa,
    h_osack = pw_osack,
    h_ortd = pw_ortd,
    h_optd = pw_optd,
    h_oayds = pw_oayds,
    h_oyac = pw_oyac,
    h_drya = pw_drya,
    h_dpya = pw_dpya,
    h_dfum = pw_dfum,
    h_dint = pw_dint,
    h_depa = pw_depa,
    h_dsack = pw_dsack,
    h_drtd = pw_drtd,
    h_dptd = pw_dptd,
    h_dayds = pw_dayds,
    h_dyac = pw_dyac
  ) %>%
  ungroup()
modelData <- modelData %>% 
  distinct() %>%
  mutate(
    OU = ifelse(total > total_line,1, 0)
  ) %>%
  filter(season > 2016)


#Fit to a lm model
model <- lm(total ~
              a_orya +
              a_opya +
              a_ofum +
              a_oint +
              a_oepa +
              a_osack +
              a_ortd +
              a_optd +
              a_oayds +
              a_oyac +
              a_drya +
              a_dpya +
              a_dfum +
              a_dint +
              a_depa +
              a_dsack +
              a_drtd +
              a_dptd +
              a_dayds +
              a_dyac +
              h_orya +
              h_opya +
              h_ofum +
              h_oint +
              h_oepa +
              h_osack +
              h_ortd +
              h_optd +
              h_oayds +
              h_oyac +
              h_drya +
              h_dpya +
              h_dfum +
              h_dint +
              h_depa +
              h_dsack +
              h_drtd +
              h_dptd +
              h_dayds +
              h_dyac,
            data = modelData)
