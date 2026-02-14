# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# This script scrapes all the data for the Hack n Roll app.
# Author: Filippos Polyzos
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Setup environment:
# pacman::p_load(tidyverse,hoopR,httr,jsonlite,rvest,glue,janitor,fuzzyjoin)
library(dplyr)
library(purrr)
library(stringr)
library(hoopR)
library(httr)
library(jsonlite)
library(rvest)
library(glue)
library(janitor)
library(fuzzyjoin)
library(plyr)

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

season="2025-26";season_type="Regular%20Season";per_mode="Totals"
# Team Information:
## CODE: INFO_
teams_info = left_join(
  nba_leaguedashteamstats(season=season,per_mode=per_mode) %>% 
    pluck(1) %>% 
    select(INFO_TEAM_NAME=TEAM_NAME,INFO_TEAM_ID=TEAM_ID,INFO_GP=GP,INFO_W=W,INFO_L=L,
           INFO_DREB=DREB,INFO_OREB=OREB,INFO_AST=AST,INFO_STL=STL,INFO_BLK=BLK,
           INFO_FGA=FGA,INFO_FG3A=FG3A),
  nba_leaguedashteamstats(season=season,measure_type="Advanced") %>% 
    pluck(1) %>% 
    select(INFO_TEAM_ID=TEAM_ID,INFO_OFF_RATING=OFF_RATING,INFO_DEF_RATING=DEF_RATING,
           INFO_NET_RATING=NET_RATING),
  by="INFO_TEAM_ID"
)

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Team Shot Location Frequency and Efficiency:
## CODE: LOC_
Sys.sleep(5)
myurl=GET(glue("https://api.pbpstats.com/get-totals/nba?Season={season}&SeasonType={season_type}&Type=Team&StatType={per_mode}&StarterState=All&StartType=All"))

teams_loc_all = suppressMessages(
  fromJSON(content(myurl , "text")) %>% 
    pluck("multi_row_table_data") %>% 
    tibble() %>% 
    clean_names("all_caps")
)

teams_loc = teams_loc_all %>% 
  mutate(MID_RANGE_FGM=as.numeric(SHORT_MID_RANGE_FGM+LONG_MID_RANGE_FGM),
         MID_RANGE_FGA=as.numeric(SHORT_MID_RANGE_FGA+LONG_MID_RANGE_FGA),
         MID_RANGE_FREQUENCY=MID_RANGE_FGA/(as.numeric(FG2A+FG3A)),
         MID_RANGE_ACCURACY=MID_RANGE_FGM/MID_RANGE_FGA %>% round(3),
         ALL3_FREQ=FG3A/(as.numeric(FG2A+FG3A)),
         ALL3_ACCURACY=FG3M/FG3A) %>% 
  select(INFO_TEAM_ID=TEAM_ID,
         LOC_RIM_FREQ=AT_RIM_FREQUENCY,LOC_RIM_EFF=AT_RIM_ACCURACY,
         LOC_SHORT_MID_FREQ=SHORT_MID_RANGE_FREQUENCY,LOC_SHORT_MID_EFF=SHORT_MID_RANGE_ACCURACY,
         LOC_LONG_MID_FREQ=LONG_MID_RANGE_FREQUENCY,LOC_LONG_MID_EFF=LONG_MID_RANGE_ACCURACY,
         LOC_ALL_MID_FREQ=MID_RANGE_FREQUENCY,LOC_ALL_MID_EFF=MID_RANGE_ACCURACY,
         LOC_CORNER_FREQ=CORNER3FREQUENCY,LOC_CORNER_EFF=CORNER3ACCURACY,
         LOC_ARC_FREQ=ARC3FREQUENCY,LOC_ARC_EFF=ARC3ACCURACY,
         LOC_ALL3_FREQ=ALL3_FREQ,LOC_ALL3_EFF=ALL3_ACCURACY) %>% 
  mutate_at(-1, ~round(.,3))

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Opponent Shot Location Frequency and Efficiency:
## CODE: OPP_LOC_
Sys.sleep(5)
myurl=GET(glue("https://api.pbpstats.com/get-totals/nba?Season={season}&SeasonType={season_type}&Type=Opponent&StatType={per_mode}&StarterState=All&StartType=All"))

opp_loc_all = suppressMessages(
  fromJSON(content(myurl , "text")) %>% 
    pluck("multi_row_table_data") %>% 
    tibble() %>% 
    clean_names("all_caps")
)

opp_loc = opp_loc_all %>% 
  mutate(MID_RANGE_FGM=as.numeric(SHORT_MID_RANGE_FGM+LONG_MID_RANGE_FGM),
         MID_RANGE_FGA=as.numeric(SHORT_MID_RANGE_FGA+LONG_MID_RANGE_FGA),
         MID_RANGE_FREQUENCY=MID_RANGE_FGA/(as.numeric(FG2A+FG3A)),
         MID_RANGE_ACCURACY=MID_RANGE_FGM/MID_RANGE_FGA %>% round(3),
         ALL3_FREQ=FG3A/(as.numeric(FG2A+FG3A)),
         ALL3_ACCURACY=FG3M/FG3A) %>% 
  select(INFO_TEAM_ID=TEAM_ID,
         OPP_LOC_RIM_FREQ=AT_RIM_FREQUENCY,OPP_LOC_RIM_EFF=AT_RIM_ACCURACY,
         OPP_LOC_SHORT_MID_FREQ=SHORT_MID_RANGE_FREQUENCY,OPP_LOC_SHORT_MID_EFF=SHORT_MID_RANGE_ACCURACY,
         OPP_LOC_LONG_MID_FREQ=LONG_MID_RANGE_FREQUENCY,OPP_LOC_LONG_MID_EFF=LONG_MID_RANGE_ACCURACY,
         OPP_LOC_ALL_MID_FREQ=MID_RANGE_FREQUENCY,OPP_LOC_ALL_MID_EFF=MID_RANGE_ACCURACY,
         OPP_LOC_CORNER_FREQ=CORNER3FREQUENCY,OPP_LOC_CORNER_EFF=CORNER3ACCURACY,
         OPP_LOC_ARC_FREQ=ARC3FREQUENCY,OPP_LOC_ARC_EFF=ARC3ACCURACY,
         OPP_LOC_ALL3_FREQ=ALL3_FREQ,OPP_LOC_ALL3_EFF=ALL3_ACCURACY) %>% 
  mutate_at(-1, ~round(.,3))

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# PBPStats Extras:
## CODE: PBP_
Sys.sleep(5)
teams_pbp = teams_loc_all %>% 
  mutate(PBP_HIGH_VALUE_AST=(AT_RIM_ASSISTS+THREE_PT_ASSISTS)) %>% 
  select(INFO_TEAM_ID=TEAM_ID,
         PBP_HIGH_VALUE_AST,PBP_OFF_POSS=OFF_POSS,PBP_DEF_POSS=DEF_POSS,
         PBP_PTS=POINTS,PBP_OPP_POINTS=OPPONENT_POINTS,
         PBP_LOOSE_PFD=LOOSE_BALL_FOULS_DRAWN,
         PBP_OFF_SECONDS=SECONDS_PER_POSS_OFF,PBP_PACE=PACE,PBP_SHOT_QUALITY=SHOT_QUALITY_AVG)
rm(teams_loc_all,opp_loc_all,myurl)
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Four Factors:
## CODE: FF_
Sys.sleep(5)
fourfactors = nba_leaguedashteamstats(season=season,measure_type="Four Factors") %>% 
  pluck(1) %>% 
  select(INFO_TEAM_ID=TEAM_ID,
         FF_EFG_PCT=EFG_PCT,FF_FTA_RATE=FTA_RATE,FF_TOV_PCT=TM_TOV_PCT,FF_OREB_PCT=OREB_PCT,
         FF_OPP_EFG_PCT=OPP_EFG_PCT,FF_OPP_FTA_RATE=OPP_FTA_RATE,FF_OPP_TOV_PCT=OPP_TOV_PCT,
         FF_OPP_OREB_PCT=OPP_OREB_PCT)

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Assisted Made Field Goals:
## CODE: MISC_
Sys.sleep(5)
assistedd = nba_leaguedashteamstats(season=season,measure_type="Scoring") %>% 
  pluck(1) %>% 
  select(INFO_TEAM_ID=TEAM_ID,
         MISC_PCT_AST_FGM=PCT_AST_FGM,MISC_PCT_AST_2PM=PCT_AST_2PM,MISC_PCT_AST_3PM=PCT_AST_3PM)

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Hustle:
## CODE: MISC_
Sys.sleep(5)
hustle = nba_leaguehustlestatsteam(season=season,per_mode=per_mode) %>% 
  pluck(1) %>% 
  select(INFO_TEAM_ID=TEAM_ID,
         MISC_SCREEN_AST=SCREEN_ASSISTS,MISC_DEFLECTIONS=DEFLECTIONS,MISC_CONTESTED_SHOTS=CONTESTED_SHOTS,
         MISC_CONTESTED_SHOTS_2PT=CONTESTED_SHOTS_2PT,MISC_CONTESTED_SHOTS_3PT=CONTESTED_SHOTS_3PT)

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Tracking
### CODE: MISC_
Sys.sleep(5)
tracking = plyr::join_all(
  list(
    nba_leaguedashptstats(player_or_team="Team",season=season,per_mode=per_mode,
                          pt_measure_type="Drives") %>% 
      pluck(1) %>% 
      select(INFO_TEAM_ID=TEAM_ID,MISC_DRIVES=DRIVES),
    nba_leaguedashptstats(player_or_team="Team",season=season,per_mode=per_mode,
                          pt_measure_type="Passing") %>% 
      pluck(1) %>% 
      select(INFO_TEAM_ID=TEAM_ID,MISC_PASSES=PASSES_MADE,
             MISC_POTENTIAL_AST=POTENTIAL_AST),
    nba_leaguedashptstats(player_or_team="Team",season=season,per_mode=per_mode,
                          pt_measure_type="CatchShoot") %>% 
      pluck(1) %>% 
      select(INFO_TEAM_ID=TEAM_ID,MISC_CS_FG3_PCT=CATCH_SHOOT_FG3_PCT,
             MISC_CS_FG3A=CATCH_SHOOT_FG3A),
    nba_leaguedashptstats(player_or_team="Team",season=season,per_mode=per_mode,
                          pt_measure_type="Possessions") %>% 
      pluck(1) %>% 
      select(INFO_TEAM_ID=TEAM_ID,MISC_SEC_PER_TOUCH=AVG_SEC_PER_TOUCH,
             MISC_ELBOW_TOUCHES=ELBOW_TOUCHES,MISC_POST_TOUCHES=POST_TOUCHES,
             MISC_PAINT_TOUCHES=PAINT_TOUCHES),
    nba_leaguedashptstats(player_or_team="Team",season=season,per_mode=per_mode,
                          pt_measure_type="SpeedDistance") %>% 
      pluck(1) %>% 
      select(INFO_TEAM_ID=TEAM_ID,MISC_DIST_MILES_OFF=DIST_MILES_OFF,
             MISC_DIST_MILES_DEF=DIST_MILES_DEF)
  ),
  by="INFO_TEAM_ID",type="left") %>% 
  as_tibble()

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Standings
## CODE: STD_

mystandings = nba_leaguestandings(season=season) %>% 
  pluck(1) %>% 
  clean_names("all_caps") %>% 
  select(INFO_TEAM_ID=TEAM_ID,STD_RECORD=RECORD,STD_CONF_RANK=PLAYOFF_RANK,STD_L10=L10) %>% 
  left_join(.,
            nba_leaguedashteamstats(season=season,measure_type="Advanced",
                                    last_n_games=10) %>% 
              pluck(1) %>% 
              select(INFO_TEAM_ID=TEAM_ID,STD_L10_OFF_RATING=OFF_RATING,
                     STD_L10_DEF_RATING=DEF_RATING,STD_L10_NET_RATING=NET_RATING),
            by="INFO_TEAM_ID")

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Join all tables:

hnrt = mget(ls(), .GlobalEnv) %>%
  Filter(is.data.frame, .) %>% 
  reduce(., left_join, by = "INFO_TEAM_ID") %>% 
  # Re-organize columns
  select(1,38:41,18:23,42:51,66:74,5:12,52:65,24:37,2:4,75:85) %>% 
  mutate_at(-c(1,2,6,8),as.numeric) %>% 
  # Convert counting stats to per 100 possessions format
  mutate_at(c(13,14,17,18,22,25,27,70:72,74,76:79), ~./PBP_OFF_POSS*100) %>% 
  mutate_at(c(12,15,16,26,80), ~./PBP_DEF_POSS*100) 
rm(list=setdiff(ls(),"hnrt"))

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## Save data:
write_csv(hnrt,"hnr-teams.csv")

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

rm(list=ls())
season="2025-26";season_type="Regular%20Season";per_mode="Totals"
s = "https://raw.githubusercontent.com/filippospol/R-bball-projects/refs/heads/main/"
source(paste0(s,"scraper%20functions/bbref/bbref_advanced.R"))
source(paste0(s,"scraper%20functions/bbref/bbref_play_by_play.R"))
source(paste0(s,"mergeStats.R")) ; rm(s)

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Basic Stats
## Traditional Totals
## GP,MP,POSS
## POS, CTG_POS
## USG%, TS%, %
pcom = left_join(
  nba_leaguedashplayerstats(season=season,per_mode=per_mode,measure_type="Advanced") %>% 
    pluck(1) %>% 
    select(PLAYER_ID,TEAM_ID,PLAYER_NAME,TEAM_NBACOM=TEAM_ABBREVIATION,GP,POSS),
  nba_leaguedashplayerstats(season=season,per_mode=per_mode,measure_type="Base") %>% 
    pluck(1) %>% 
    select(PLAYER_ID,MIN,PTS,REB,DREB,OREB,AST,STL,BLK,TOV,FTA,FTM,FGA,FGM,FG3A,FG3M),
  by="PLAYER_ID"
)

pref = left_join(
  bbref_advanced(season=season) %>% 
    select(PLAYER_NAME,TEAM_BBREF=TEAM,TS_PCT,USG_PCT,FTA_RATE:TOV_PCT),
  bbref_play_by_play(season=season) %>% 
    select(PLAYER_NAME,POS,PG_PERCENT:C_PERCENT),
  by="PLAYER_NAME"
)

pBase = mergeStats(pcom,pref) %>% 
  mutate_at(33:37, ~round(as.numeric(.)/100,3)) %>% 
  mutate(POSITION = case_when(
    PG_PERCENT>=0.85 ~ "Point",
    (PG_PERCENT>=0.5 | SG_PERCENT>=0.5) | PG_PERCENT+SG_PERCENT>0.8 ~ "Combo",
    (SG_PERCENT>=0.85 | SF_PERCENT>=0.85) | (SG_PERCENT+SF_PERCENT>0.7) ~ "Wing",
    C_PERCENT<0.1 & (SF_PERCENT>=0.65 | PF_PERCENT>=0.65) | SF_PERCENT+PF_PERCENT>=0.75 ~ "Forward",
    TRUE ~ "Big"
  )) %>% 
  select(PLAYER_ID,TEAM_ID,TEAM_NBACOM,TEAM_BBREF,PLAYER_NAME,
         POS,ROLE=POSITION,GP,MIN,POSS,
         PTS:FG3M,TS_PCT:TOV_PCT)
rm(pcom,pref)

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Shot distribution
## FGA & FG%
myurl=GET(glue("https://api.pbpstats.com/get-totals/nba?Season={season}&SeasonType={season_type}&Type=Player&StatType={per_mode}&StarterState=All&StartType=All"))

pPbp = suppressMessages(
  fromJSON(content(myurl , "text")) %>% 
    pluck("multi_row_table_data") %>% 
    tibble() %>% 
    clean_names("all_caps")
) %>% 
  mutate_all(~replace(., is.na(.), 0)) %>% 
  mutate(MID_FGA=LONG_MID_RANGE_FGA+SHORT_MID_RANGE_FGA,
         MID_FGM=LONG_MID_RANGE_FGM+SHORT_MID_RANGE_FGM,
         MID_FREQUENCY=MID_FGA/(FG2A+FG3A),
         MID_ACCURACY=MID_FGM/MID_FGA,
         FG2_PCT=FG2M/FG2A,
         THREE_FREQUENCY=FG3A/(FG2A+FG3A),
         THREE_ACCURACY=FG3M/FG3A) %>% 
  select(PLAYER_ID=ENTITY_ID,FG2_PCT,AT_RIM_FGA,AT_RIM_FREQUENCY,AT_RIM_ACCURACY,
         MID_FGA,MID_FREQUENCY,MID_ACCURACY,THREE_FREQUENCY,THREE_ACCURACY) %>% 
  mutate_at(c(2,4,5,7:10), ~round(.,3)) ; rm(myurl)

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Merge Data:
hnrp = plyr::join_all(list(pBase,pPbp),type="left") %>% 
  as_tibble() %>% 
  mutate_at(8:42, as.numeric) %>% 
  mutate(FG3_PCT=THREE_ACCURACY,
         FG3X=(2/(1+exp(-FG3A/POSS*100))-1)*FG3_PCT,
         BOX_CREATION=AST/POSS*100*0.1843 + (PTS/POSS*100+TOV/POSS*100)*0.0969-
           2.3021*(FG3X)+0.0582*(AST/POSS*100*(PTS/POSS*100+TOV/POSS*100)*FG3X)-1.1942,
         OFF_LOAD=0.75*(AST/POSS*100-(0.38*BOX_CREATION))+FGA/POSS*100+
           0.44*FTA/POSS*100+BOX_CREATION+TOV/POSS*100,
         OFF_LOAD=round(OFF_LOAD,1),
         BOX_CREATION=round(BOX_CREATION,1),
         rTS=round((TS_PCT*100)-mean(TS_PCT*100,na.rm=T),1),
         MIN=round(MIN)) %>% 
  select(TEAM_ID,PLAYER_NAME,PLAYER_ID,TEAM_NBACOM,POS,ROLE,GP,MIN,POSS,
         PTS,REB,AST,STL,BLK,TOV,FG2_PCT,FG3_PCT,OREB,DREB,FG3A, # Traditional
         AT_RIM_FGA:THREE_ACCURACY, # Shot Distribution
         rTS,USG_PCT,BOX_CREATION,OFF_LOAD, # Usage Metrics
         FTA_RATE:TOV_PCT # Advanced
  ) %>% 
  mutate_all(~replace(., is.nan(.), 0))

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## Save data:
write_csv(hnrp,"hnr-players.csv")
rm(list=setdiff(ls(),c("hnrp","hnrt")))

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Expanded Standings
x1 = nba_leaguestandings(season="2025-26") %>% 
  pluck(1) %>% 
  clean_names("all_caps") %>% 
  mutate(TEAM_NAME=paste(TEAM_CITY,TEAM_NAME,sep=" ")) %>% 
  select(TEAM_ID,TEAM_NAME,CONFERENCE,W=WINS,L=LOSSES,WIN_PCT,STREAK=CURRENT_STREAK,
         AT_HOME=HOME,ON_THE_ROAD=ROAD,L10,JAN:POST_AS,VS_EAST,VS_WEST) %>% 
  mutate(STREAK=if_else(as.numeric(STREAK)>0,
                        paste0("W",STREAK),paste0("L",abs(as.numeric(STREAK))))
  )
x2 = suppressMessages(
  "https://www.espn.com/nba/standings/_/view/expanded" %>% 
    read_html() %>%
    html_elements("table") %>%
    html_table(fill = TRUE) %>%
    split(rep(1:2, each = 2)) %>%
    map(~ bind_cols(.x)) %>%
    bind_rows() %>% 
    clean_names("all_caps") %>% 
    select(TEAM_NAME=X1,8,9)
)
x3 = suppressWarnings(
  "https://www.basketball-reference.com/leagues/NBA_2026.html" %>% 
    read_html() %>%
    html_elements("table") %>%
    html_table(fill = TRUE) %>% 
    pluck(11) %>% 
    row_to_names(1) %>% 
    clean_names("all_caps") %>% 
    mutate(TEAM_NAME = str_remove_all(TEAM, "\\*"),
           TEAM_NAME=if_else(TEAM_NAME=="Los Angeles Clippers","LA Clippers",TEAM_NAME)) %>% 
    select(TEAM_NAME,SOS) %>% 
    filter(TEAM_NAME!="League Average")
)

# Merge x1,x2:
edd <- adist(x1$TEAM_NAME,x2$TEAM_NAME)
ind <- rep(NA,ncol(edd))
for (i in 1:nrow(x1)) {
  ind[i] <- which.min(edd[,i])
}
x2 = x2[ind,] # %>% na.omit()
x12 = suppressMessages(
  bind_cols(x1,x2 %>% select(-c(TEAM_NAME)))
)
rm(edd,ind,i,x1,x2)

# Merge x1,x2,x3:
# edd <- adist(x12$TEAM_NAME,x3$TEAM_NAME)
# ind <- rep(NA,ncol(edd))
# for (i in 1:nrow(x12)) {
#   ind[i] <- which.min(edd[,i])
# }
# x3 = x3[ind,] # %>% na.omit()
# x = suppressMessages(
#   bind_cols(x12,x3 %>% select(-c(TEAM_NAME)))
# )
x = left_join(x12,x3,by="TEAM_NAME")

hnrs = x %>% 
  select(1:6,10,29,7:9,27,28,25,26,23,24,20:22,11:14) %>% 
  mutate_at(c(4:6,8),as.numeric) %>% 
  group_by(CONFERENCE) %>% 
  mutate(RANK=paste0(row_number(),"."),.before=1) %>% 
  ungroup()
rm(list=setdiff(ls(),c("hnrs")))

## Save data:
write_csv(hnrs,"hnr-standings.csv")
rm(list=ls())

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Schedule:
scheduleSimple = nba_schedule(season="2025-26") %>% 
  clean_names("all_caps") %>% 
  select(GAME_DATE,GAME_ID,GAME_STATUS_TEXT,
         HOME_TEAM_CITY,HOME_TEAM_NAME,HOME_TEAM_TRICODE,
         AWAY_TEAM_CITY,AWAY_TEAM_NAME,AWAY_TEAM_TRICODE) %>% 
  mutate(GAME_STATUS_TEXT=str_trim(GAME_STATUS_TEXT)) %>% 
  # filter(!grepl("Final",GAME_STATUS_TEXT)) %>% 
  mutate(HOME_TEAM=paste(HOME_TEAM_CITY,HOME_TEAM_NAME,sep=" "),
         AWAY_TEAM=paste(AWAY_TEAM_CITY,AWAY_TEAM_NAME,sep=" "),
         # DATE=gsub("-","/",str_sub(GAME_DATE,-5)),
         MATCHUP=paste0(HOME_TEAM," @ ",AWAY_TEAM),
         MATCHUP_2=paste0(HOME_TEAM_TRICODE," @ ",AWAY_TEAM_TRICODE)) %>% 
  select(GAME_STATUS_TEXT,GAME_DATE,HOME_TEAM,AWAY_TEAM,MATCHUP,MATCHUP_2)

hnrsc = bind_rows(
  scheduleSimple %>% select(-3) %>% rename(TEAM_NAME=3),
  scheduleSimple %>% select(-4) %>% rename(TEAM_NAME=3)
) %>% 
  arrange(GAME_DATE,MATCHUP_2) %>% 
  filter(!grepl("Final",GAME_STATUS_TEXT)) %>%
  select(DATE=GAME_DATE,TEAM_NAME,MATCHUP,MATCHUP_2) %>% 
  mutate(
    away_team = str_trim(str_extract(MATCHUP, ".*(?= @)")),
    home_team = str_trim(str_extract(MATCHUP, "(?<=@ ).*")),
    away_abbrev = str_trim(str_extract(MATCHUP_2, "^[A-Z]{3}")),
    home_abbrev = str_trim(str_extract(MATCHUP_2, "(?<= )[A-Z]{3}$")),
    matchup_type = case_when(
      TEAM_NAME == home_team ~ paste0("@ ", away_abbrev),
      TEAM_NAME == away_team ~ paste0("vs ", home_abbrev),
      TRUE ~ NA_character_
    )
  ) %>% 
  select(DATE,TEAM_NAME,MATCHUP_TYPE=matchup_type) %>% 
  arrange(TEAM_NAME,DATE) %>% 
  mutate(DATE=gsub("-","/",str_sub(DATE,-5))) %>% 
  group_by(TEAM_NAME) %>% 
  slice_head(n=5) %>% 
  ungroup() %>% 
  group_by(TEAM_NAME) %>% 
  summarise(NEXT_GAMES=paste(MATCHUP_TYPE,collapse=", ")) %>% 
  ungroup()

## Save data:
write_csv(hnrsc,"hnr-schedule.csv")
rm(list=ls())

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

