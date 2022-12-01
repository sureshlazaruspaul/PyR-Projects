library(tidyverse)
library(data.table)
library(janitor)
library(tidyquant)
library(plotly)
library(gganimate)
library(gifski)
library(patchwork)
library(lubridate)
library(geosphere)
library(aod)

#-------------------------------------------------------------------------------
# path to files ...
#-------------------------------------------------------------------------------

# Github branch path
gitpath <- "https://raw.githubusercontent.com/sureshlazaruspaul/"

# Github path to files
dirpath <- "/BUS662-practice-datasets/main/nba/"











#-------------------------------------------------------------------------------
# games dataset
#-------------------------------------------------------------------------------

games <- fread(paste0(gitpath, dirpath, "games.csv")) %>%
  mutate(
    GAME_DATE_EST = as.Date(GAME_DATE_EST, "%m/%d/%Y")
  )











#-------------------------------------------------------------------------------
# games_details dataset
#-------------------------------------------------------------------------------

games_details1 <- fread(paste0(gitpath, dirpath, "games_details1.csv"))
games_details2 <- fread(paste0(gitpath, dirpath, "games_details2.csv"))
games_details3 <- fread(paste0(gitpath, dirpath, "games_details3.csv"))
games_details4 <- fread(paste0(gitpath, dirpath, "games_details4.csv"))

# create an empty dataframe
games_details = data.frame()

# use rbind() to append the imported data to the empty dataframe
games_details <- rbind(games_details, games_details1)
games_details <- rbind(games_details, games_details2)
games_details <- rbind(games_details, games_details3)
games_details <- rbind(games_details, games_details4)

#-------------------------------------------------------------------------------
# games_details dataset
#   - clean time spent on field (MIN variable)
#-------------------------------------------------------------------------------
# convert MINS:SEC into HH:MINS:SEC format - length = 4
games_details$MINUTES <-
  ifelse(
    nchar(games_details$MIN) == 4 & 
      substring(games_details$MIN,2,2) == ":",
    paste0("00:0", games_details$MIN),
    "00:00:00"
  )

# convert MINS:SEC into HH:MINS:SEC format - length = 5
games_details$MINUTES <-
  ifelse(
    nchar(games_details$MIN) == 5 & 
      substring(games_details$MIN,3,3) == ":",
    paste0("00:", games_details$MIN),
    games_details$MINUTES
  )

# convert MINS:SEC into HH:MINS:SEC format - length = 8
games_details$MINUTES <-
  ifelse(
    nchar(games_details$MIN) == 8 & 
      substring(games_details$MIN,3,3) == ":" & 
        substring(games_details$MIN,6,6) == ":",
    games_details$MIN,
    games_details$MINUTES
  ) 

games_details <- games_details %>%
  mutate(
    # convert MINUTES to SECONDS format
    SECONDS = 
      as.numeric(substring(MINUTES,1,2)) * 3600 +
      as.numeric(substring(MINUTES,4,5)) *   60 +
      as.numeric(substring(MINUTES,7,8))
  ) %>%
  select(
    -sno, -MIN
  ) %>%
  # replace all missing stats to zero
  mutate_at(
    vars(PTS, FGM, FGA, FG3M, FG3A, FTM, FTA, OREB, DREB, BLK),
    ~replace(., is.na(.), 0)
  )

rm("games_details1", "games_details2", "games_details3", "games_details4")











#-------------------------------------------------------------------------------
# players dataset
#-------------------------------------------------------------------------------

players <- fread(paste0(gitpath, dirpath, "players.csv")) %>%
  group_by(
    SEASON, PLAYER_ID, PLAYER_NAME
  ) %>%
  mutate(id = row_number())

# get players who played in multiple teams
# -   how? reshape the players file
players <- dcast(
    setDT(players)[, id := paste0("TEAM_ID", id)], ... ~ id, value.var = "TEAM_ID"
  )






#-------------------------------------------------------------------------------
# ranking dataset
#-------------------------------------------------------------------------------

ranking <- fread(paste0(gitpath, dirpath, "ranking.csv"))











#-------------------------------------------------------------------------------
# teams dataset
#-------------------------------------------------------------------------------

teams <- fread(paste0(gitpath, dirpath, "teams.csv"))
















#===============================================================================
# prepare the games_details dataset
#   - get season id for games
#-------------------------------------------------------------------------------

games_details1 <-
  games_details %>% 
    left_join(
      # keep only SEASON info from games
      games %>%
        select(
          GAME_ID, SEASON
        ) %>%
        # get unique firms
        distinct(),
      # merge by GAME_ID
      by = "GAME_ID"
    ) %>%
    filter(
      # keep only non-missing
      !is.na(GAME_ID) & 
        !is.na(SEASON)
    )

glimpse(games_details1)
rm("games_details")


















#-------------------------------------------------------------------------------
#   - create two sub-datasets
#     1. players stats data
#     2. team stats data
#-------------------------------------------------------------------------------

player_details <-
  games_details1 %>%
    # only keep key variables ...
    select(
      SEASON, PLAYER_ID, SECONDS, PTS, FGM, FGA, FG3M, FG3A, FTM, FTA, OREB, DREB, BLK
    ) %>%
      # group by ...
      group_by(
        SEASON, PLAYER_ID
      ) %>%
        # sum all player stats
        summarise(across(everything(), sum)) %>%
          # get player names etc...
          left_join(
            players,
            by = c("SEASON", "PLAYER_ID")
          ) %>%
            # drop players not in players.csv file
            filter(
              !is.na(PLAYER_NAME)
            ) %>%
              # replace all missing stats to zero
              mutate_at(
                vars(PTS, FGM, FGA, FG3M, FG3A, FTM, FTA, OREB, DREB, BLK),
                  ~replace(., is.na(.), 0)
              ) %>%
                # calculate aggregate stats
                mutate(
                  offensive_stats = FGM + FGA + FG3M + FG3A + FTM + FTA,
                  defensive_stats = OREB + DREB + BLK,
                  overall_stats = FGM + FGA + FG3M + FG3A + FTM + FTA + OREB + DREB + BLK
                ) %>%
                  # drop some variables ...
                  select(
                    -FGM, -FGA, -FG3M, -FG3A, -FTM, -FTA, -OREB, -DREB, -BLK
                  ) %>%
                    # sort the data
                    arrange(
                      desc(SEASON), PLAYER_ID
                    )

rm("players")

#-------------------------------------------------------------------------------
# top 10 offensive players
#-------------------------------------------------------------------------------
top10o <- player_details %>%
    mutate(
      PLAYER_SEASON = factor(paste(paste0(PLAYER_NAME, ","), SEASON))
    ) %>%
      ungroup() %>%
        select(PLAYER_SEASON, offensive_stats) %>%
          arrange(desc(offensive_stats)) %>%
            # keep top 10
            top_n(10)

top10o %>%
  mutate(
    PLAYER_SEASON = fct_reorder(PLAYER_SEASON, desc(offensive_stats))
  ) %>%
    ggplot(
      aes(x=PLAYER_SEASON,y=offensive_stats)
    ) +
      theme_tq() +
        labs(
          title = "Top 10 Most Offensive Players",
          subtitle = "= FGM + FGA + FG3M + FG3A + FTM + FTA",
          caption = paste("Seasons:", min(player_details$SEASON),
                          "through", max(player_details$SEASON)),
          x = "",
          y = "Offensive Statistics"
        ) + 
          geom_point(color = "red") +
            theme(axis.text.x = element_text(angle=45, hjust=1))


#-------------------------------------------------------------------------------
# top 10 defensive players
#-------------------------------------------------------------------------------
top10d <- player_details %>%
  mutate(
    PLAYER_SEASON = factor(paste(paste0(PLAYER_NAME, ","), SEASON))
  ) %>%
  ungroup() %>%
  select(PLAYER_SEASON, defensive_stats) %>%
  arrange(desc(defensive_stats)) %>%
  # keep top 10
  top_n(10)

top10d %>%
  mutate(
    PLAYER_SEASON = fct_reorder(PLAYER_SEASON, desc(defensive_stats))
  ) %>%
  ggplot(
    aes(x=PLAYER_SEASON,y=defensive_stats)
  ) +
  theme_tq() +
  labs(
    title = "Top 10 Most Defensive Players",
    subtitle = "= OREB + DREB + BLK",
    caption = paste("Seasons:", min(player_details$SEASON),
                    "through", max(player_details$SEASON)),
    x = "",
    y = "Defensive Statistics"
  ) + 
  geom_point(color = "red") +
  theme(axis.text.x = element_text(angle=45, hjust=1))


#-------------------------------------------------------------------------------
# top 10 overall players
#-------------------------------------------------------------------------------
top10all <- player_details %>%
  mutate(
    PLAYER_SEASON = factor(paste(paste0(PLAYER_NAME, ","), SEASON))
  ) %>%
  ungroup() %>%
  select(PLAYER_SEASON, overall_stats) %>%
  arrange(desc(overall_stats)) %>%
  # keep top 10
  top_n(10)

top10all %>%
  mutate(
    PLAYER_SEASON = fct_reorder(PLAYER_SEASON, desc(overall_stats))
  ) %>%
  ggplot(
    aes(x=PLAYER_SEASON,y=overall_stats)
  ) +
  theme_tq() +
  labs(
    title = "Top 10 Players (Offensive + Defensive)",
    subtitle = "= FGM + FGA + FG3M + FG3A + FTM + FTA + OREB + DREB + BLK",
    caption = paste("Seasons:", min(player_details$SEASON),
                    "through", max(player_details$SEASON)),
    x = "",
    y = "Offensive & Defensive Statistics"
  ) + 
  geom_point(color = "red") +
  theme(axis.text.x = element_text(angle=45, hjust=1))



#-------------------------------------------------------------------------------
# top 10 overall players by points
#-------------------------------------------------------------------------------
top10pts <- player_details %>%
  mutate(
    PLAYER_SEASON = factor(paste(paste0(PLAYER_NAME, ","), SEASON))
  ) %>%
  ungroup() %>%
  select(PLAYER_SEASON, PTS) %>%
  arrange(desc(PTS)) %>%
  # keep top 10
  top_n(10)

top10pts %>%
  mutate(
    PLAYER_SEASON = fct_reorder(PLAYER_SEASON, desc(PTS))
  ) %>%
  ggplot(
    aes(x=PLAYER_SEASON,y=PTS)
  ) +
  theme_tq() +
  labs(
    title = "Top 10 Players (by points)",
    subtitle = "= PTS",
    caption = paste("Seasons:", min(player_details$SEASON),
                    "through", max(player_details$SEASON)),
    x = "",
    y = "Points"
  ) + 
  geom_point(color = "red") +
  theme(axis.text.x = element_text(angle=45, hjust=1))


#-------------------------------------------------------------------------------
# top 10 players by field playtime
#-------------------------------------------------------------------------------
top10SECONDS <- player_details %>%
  mutate(
    PLAYER_SEASON = factor(paste(paste0(PLAYER_NAME, ","), SEASON))
  ) %>%
  ungroup() %>%
  select(PLAYER_SEASON, SECONDS) %>%
  arrange(desc(SECONDS)) %>%
  # keep top 10
  top_n(10)

top10SECONDS %>%
  mutate(
    PLAYER_SEASON = fct_reorder(PLAYER_SEASON, desc(SECONDS))
  ) %>%
  ggplot(
    aes(x=PLAYER_SEASON,y=SECONDS/3600)
  ) +
  theme_tq() +
  labs(
    title = "Top 10 Players (by Field Playtime)",
    subtitle = "= SECONDS/3600",
    caption = paste("Seasons:", min(player_details$SEASON),
                    "through", max(player_details$SEASON)),
    x = "",
    y = "Hours"
  ) + 
  geom_point(color = "red") +
  theme(axis.text.x = element_text(angle=45, hjust=1))


rm("top10o", "top10d", "top10all", "top10pts", "top10SECONDS")

































#-------------------------------------------------------------------------------
#   - create two sub-datasets
#     1. players stats data
#     2. team stats data
#-------------------------------------------------------------------------------

team_details <-
  games_details1 %>%
  # only keep key variables ...
  select(
    SEASON, TEAM_ID, SECONDS, PTS, FGM, FGA, FG3M, FG3A, FTM, FTA, OREB, DREB, BLK
  ) %>%
  # group by ...
  group_by(
    SEASON, TEAM_ID
  ) %>%
  # sum all player stats
  summarise(across(everything(), sum)) %>%
  # get player names etc...
  left_join(
    teams,
    by = c("TEAM_ID")
  ) %>%
  # drop players not in players.csv file
  filter(
    !is.na(NICKNAME)
  ) %>%
  # replace all missing stats to zero
  mutate_at(
    vars(PTS, FGM, FGA, FG3M, FG3A, FTM, FTA, OREB, DREB, BLK),
    ~replace(., is.na(.), 0)
  ) %>%
  # calculate aggregate stats
  mutate(
    offensive_stats = FGM + FGA + FG3M + FG3A + FTM + FTA,
    defensive_stats = OREB + DREB + BLK,
    overall_stats = FGM + FGA + FG3M + FG3A + FTM + FTA + OREB + DREB + BLK,
    age = MAX_YEAR - MIN_YEAR
  ) %>%
  # drop some variables ...
  select(
    -FGM, -FGA, -FG3M, -FG3A, -FTM, -FTA, -OREB, -DREB, -BLK
  ) %>%
  # sort the data
  arrange(
    desc(SEASON), TEAM_ID
  )

rm("teams")

#-------------------------------------------------------------------------------
# top 10 offensive teams
#-------------------------------------------------------------------------------
top10o <- team_details %>%
  mutate(
    TEAM_SEASON = factor(paste(paste0(NICKNAME, ","), SEASON))
  ) %>%
  ungroup() %>%
  select(TEAM_SEASON, offensive_stats) %>%
  arrange(desc(offensive_stats)) %>%
  # keep top 10
  top_n(10)

top10o %>%
  mutate(
    TEAM_SEASON = fct_reorder(TEAM_SEASON, desc(offensive_stats))
  ) %>%
  ggplot(
    aes(x=TEAM_SEASON,y=offensive_stats)
  ) +
  theme_tq() +
  labs(
    title = "Top 10 Most Offensive Teams",
    subtitle = "= FGM + FGA + FG3M + FG3A + FTM + FTA",
    caption = paste("Seasons:", min(team_details$SEASON),
                    "through", max(team_details$SEASON)),
    x = "",
    y = "Offensive Statistics"
  ) + 
  geom_point(color = "red") +
  theme(axis.text.x = element_text(angle=45, hjust=1))


#-------------------------------------------------------------------------------
# top 10 defensive teams
#-------------------------------------------------------------------------------
top10d <- team_details %>%
  mutate(
    TEAM_SEASON = factor(paste(paste0(NICKNAME, ","), SEASON))
  ) %>%
  ungroup() %>%
  select(TEAM_SEASON, defensive_stats) %>%
  arrange(desc(defensive_stats)) %>%
  # keep top 10
  top_n(10)

top10d %>%
  mutate(
    TEAM_SEASON = fct_reorder(TEAM_SEASON, desc(defensive_stats))
  ) %>%
  ggplot(
    aes(x=TEAM_SEASON,y=defensive_stats)
  ) +
  theme_tq() +
  labs(
    title = "Top 10 Most Defensive Teams",
    subtitle = "= OREB + DREB + BLK",
    caption = paste("Seasons:", min(team_details$SEASON),
                    "through", max(team_details$SEASON)),
    x = "",
    y = "Defensive Statistics"
  ) + 
  geom_point(color = "red") +
  theme(axis.text.x = element_text(angle=45, hjust=1))


#-------------------------------------------------------------------------------
# top 10 overall teams
#-------------------------------------------------------------------------------
top10all <- team_details %>%
  mutate(
    TEAM_SEASON = factor(paste(paste0(NICKNAME, ","), SEASON))
  ) %>%
  ungroup() %>%
  select(TEAM_SEASON, overall_stats) %>%
  arrange(desc(overall_stats)) %>%
  # keep top 10
  top_n(10)

top10all %>%
  mutate(
    TEAM_SEASON = fct_reorder(TEAM_SEASON, desc(overall_stats))
  ) %>%
  ggplot(
    aes(x=TEAM_SEASON,y=overall_stats)
  ) +
  theme_tq() +
  labs(
    title = "Top 10 Teams (Offensive + Defensive)",
    subtitle = "= FGM + FGA + FG3M + FG3A + FTM + FTA + OREB + DREB + BLK",
    caption = paste("Seasons:", min(team_details$SEASON),
                    "through", max(team_details$SEASON)),
    x = "",
    y = "Offensive & Defensive Statistics"
  ) + 
  geom_point(color = "red") +
  theme(axis.text.x = element_text(angle=45, hjust=1))



#-------------------------------------------------------------------------------
# top 10 overall teams by points
#-------------------------------------------------------------------------------
top10pts <- team_details %>%
  mutate(
    TEAM_SEASON = factor(paste(paste0(NICKNAME, ","), SEASON))
  ) %>%
  ungroup() %>%
  select(TEAM_SEASON, PTS) %>%
  arrange(desc(PTS)) %>%
  # keep top 10
  top_n(10)

top10pts %>%
  mutate(
    TEAM_SEASON = fct_reorder(TEAM_SEASON, desc(PTS))
  ) %>%
  ggplot(
    aes(x=TEAM_SEASON,y=PTS)
  ) +
  theme_tq() +
  labs(
    title = "Top 10 Teams (by points)",
    subtitle = "= PTS",
    caption = paste("Seasons:", min(team_details$SEASON),
                    "through", max(team_details$SEASON)),
    x = "",
    y = "Points"
  ) + 
  geom_point(color = "red") +
  theme(axis.text.x = element_text(angle=45, hjust=1))


#-------------------------------------------------------------------------------
# top 10 teams by field playtime
#-------------------------------------------------------------------------------
top10SECONDS <- team_details %>%
  mutate(
    TEAM_SEASON = factor(paste(paste0(NICKNAME, ","), SEASON))
  ) %>%
  ungroup() %>%
  select(TEAM_SEASON, SECONDS) %>%
  arrange(desc(SECONDS)) %>%
  # keep top 10
  top_n(10)

top10SECONDS %>%
  mutate(
    TEAM_SEASON = fct_reorder(TEAM_SEASON, desc(SECONDS))
  ) %>%
  ggplot(
    aes(x=TEAM_SEASON,y=SECONDS/3600)
  ) +
  theme_tq() +
  labs(
    title = "Top 10 Teams (by Field Playtime)",
    subtitle = "= SECONDS/3600",
    caption = paste("Seasons:", min(team_details$SEASON),
                    "through", max(team_details$SEASON)),
    x = "",
    y = "Hours"
  ) + 
  geom_point(color = "red") +
  theme(axis.text.x = element_text(angle=45, hjust=1))


rm("top10o", "top10d", "top10all", "top10pts", "top10SECONDS")


























#-------------------------------------------------------------------------------
# Home versus Away Analysis
#-------------------------------------------------------------------------------

team_details <- team_details %>%
  select(
    TEAM_ID, SEASON, CITY, offensive_stats, defensive_stats, overall_stats, age
  ) %>%
  left_join(
    fread(paste0(gitpath, dirpath, "latlong.csv")),
    by = c("CITY")
  ) %>%
  select(-STATE, -CITY_STATE, -CITY)

games1 <- games %>%
  select(
    -GAME_STATUS_TEXT, -TEAM_ID_home, -TEAM_ID_away
  ) %>%
    # merge home team statistics ...
    left_join(
      team_details,
      by=c("HOME_TEAM_ID" = "TEAM_ID", "SEASON")
    ) %>%
      # rename variables ...
      rename(
        offensive_stats_home = offensive_stats, 
        defensive_stats_home = defensive_stats, 
        overall_stats_home = overall_stats,
        age_home = age,
        lat_home = lat,
        long_home = long
      ) %>%
        # merge away team statistics ...
        left_join(
          team_details,
          by=c("VISITOR_TEAM_ID" = "TEAM_ID", "SEASON")
        ) %>% 
          # rename variables ...
          rename(
            offensive_stats_away = offensive_stats, 
            defensive_stats_away = defensive_stats, 
            overall_stats_away = overall_stats,
            age_away = age,
            lat_away = lat,
            long_away = long
          ) %>%
            rowwise() %>%
              # distance between home and away in kilometers ...
              mutate(
                dist = (distm(c(long_home, lat_home),
                              c(long_away, lat_away),
                              fun = distHaversine)[1])/1000
              ) %>% ungroup() %>%
                # compute an entire array of home minus away stats ...
                mutate(
                  diff_PTS = PTS_home - PTS_away,
                  diff_FG_PCT = FG_PCT_home - FG_PCT_away,
                  diff_FT_PCT = FT_PCT_home - FT_PCT_away,
                  diff_FG3_PCT = FG3_PCT_home - FG3_PCT_away,
                  diff_AST = AST_home - AST_away,
                  diff_REB = REB_home - REB_away
                )

#-------------------------------------------------------------------------------
# Does travel affect winning?
#-------------------------------------------------------------------------------
games1 %>%
  group_by(dist) %>%
  summarise(
    mean_home_wins = mean(HOME_TEAM_WINS)
  ) %>% ungroup %>% 
  ggplot(
    aes(x=dist,y=mean_home_wins)
  ) +
  theme_classic() +
  labs(
    title = "Home Wins and Distance travelled by Visiting Team",
    subtitle = "Distance in kms",
    caption = paste("Seasons:", min(games1$SEASON),
                    "through", max(games1$SEASON)),
    x = "Distance (kms)",
    y = "Mean(Home Wins Percentage)"
  ) + geom_point() + geom_smooth()



#-------------------------------------------------------------------------------
# Does travel affect points scored?
#-------------------------------------------------------------------------------
games1 %>%
  group_by(dist) %>%
  summarise(
    mean_diff_pts = mean(diff_PTS)
  ) %>% ungroup %>% 
  ggplot(
    aes(x=dist,y=mean_diff_pts)
  ) +
  theme_classic() +
  labs(
    title = "Points Scored and Distance travelled by Visiting Team",
    subtitle = "Distance in kms",
    caption = paste("Seasons:", min(games1$SEASON),
                    "through", max(games1$SEASON)),
    x = "Distance (kms)",
    y = "Home Points (minus) Away Points"
  ) + geom_point() + geom_smooth()



#-------------------------------------------------------------------------------
# Does travel affect defensive performance?
#-------------------------------------------------------------------------------

games1 %>%
  group_by(dist) %>%
  summarise(
    mean_diff_def = mean(defensive_stats_home - defensive_stats_away)
  ) %>% ungroup %>% 
  ggplot(
    aes(x=dist,y=mean_diff_def)
  ) +
  theme_classic() +
  labs(
    title = "Defensive Stats and Distance travelled by Visiting Team",
    subtitle = "Distance in kms",
    caption = paste("Seasons:", min(games1$SEASON),
                    "through", max(games1$SEASON)),
    x = "Distance (kms)",
    y = "Home Defensive Stats (minus) Away Defensive Stats"
  ) + geom_point() + geom_smooth()



#-------------------------------------------------------------------------------
# Does travel affect offensive performance?
#-------------------------------------------------------------------------------
games1 %>%
  group_by(dist) %>%
  summarise(
    mean_diff_off = mean(offensive_stats_home - offensive_stats_away)
  ) %>% ungroup %>% 
  ggplot(
    aes(x=dist,y=mean_diff_off)
  ) +
  theme_classic() +
  labs(
    title = "Offensive Stats and Distance travelled by Visiting Team",
    subtitle = "Distance in kms",
    caption = paste("Seasons:", min(games1$SEASON),
                    "through", max(games1$SEASON)),
    x = "Distance (kms)",
    y = "Home Offensive Stats (minus) Away Offensive Stats"
  ) + geom_point() + geom_smooth()



#-------------------------------------------------------------------------------
# Does travel affect overall performance?
#-------------------------------------------------------------------------------

games1 %>%
  group_by(dist) %>%
  summarise(
    mean_diff_ov = mean(overall_stats_home - overall_stats_away)
  ) %>% ungroup %>% 
  ggplot(
    aes(x=dist,y=mean_diff_ov)
  ) +
  theme_classic() +
  labs(
    title = "Overall Stats and Distance travelled by Visiting Team",
    subtitle = "Distance in kms",
    caption = paste("Seasons:", min(games1$SEASON),
                    "through", max(games1$SEASON)),
    x = "Distance (kms)",
    y = "Home Overall Stats (minus) Away Overall Stats"
  ) + geom_point() + geom_smooth()




#-------------------------------------------------------------------------------
# Logistic Regression:
# - what affects the probability of winning?
#-------------------------------------------------------------------------------

games1 <- games1 %>%
  mutate(
    HOME_TEAM_WINS = as.factor(HOME_TEAM_WINS)
  )

mylogit <- glm(HOME_TEAM_WINS ~ 
                 dist + diff_AST + diff_REB + diff_FG_PCT + 
                 diff_FT_PCT + diff_FG3_PCT, 
               data= games1, 
               family = "binomial")

summary(mylogit)
