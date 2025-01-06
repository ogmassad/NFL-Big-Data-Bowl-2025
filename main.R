```{r}
#reading all the data 
player_play <- read.csv("https://github.com/ogmassad/NFL-Big-Data-Bowl-2025/releases/download/NFL_BDB_Data/player_play.csv")
games <- read.csv("https://github.com/ogmassad/NFL-Big-Data-Bowl-2025/releases/download/NFL_BDB_Data/games.csv")
play <- read.csv("https://github.com/ogmassad/NFL-Big-Data-Bowl-2025/releases/download/NFL_BDB_Data/plays.csv")
players <- read.csv("https://github.com/ogmassad/NFL-Big-Data-Bowl-2025/releases/download/NFL_BDB_Data/players.csv")
week1 <- read.csv("https://github.com/ogmassad/NFL-Big-Data-Bowl-2025/releases/download/NFL_BDB_Data/tracking_week_1.csv")
week2 <- read.csv("https://github.com/ogmassad/NFL-Big-Data-Bowl-2025/releases/download/NFL_BDB_Data/tracking_week_2.csv
")
week3 <- read.csv("https://github.com/ogmassad/NFL-Big-Data-Bowl-2025/releases/download/NFL_BDB_Data/tracking_week_3.csv")
week4 <- read.csv("https://github.com/ogmassad/NFL-Big-Data-Bowl-2025/releases/download/NFL_BDB_Data/tracking_week_4.csv")
week5 <- read.csv("https://github.com/ogmassad/NFL-Big-Data-Bowl-2025/releases/download/NFL_BDB_Data/tracking_week_5.csv")
week6 <- read.csv("https://github.com/ogmassad/NFL-Big-Data-Bowl-2025/releases/download/NFL_BDB_Data/tracking_week_6.csv")
week7 <- read.csv("https://github.com/ogmassad/NFL-Big-Data-Bowl-2025/releases/download/NFL_BDB_Data/tracking_week_7.csv")
week8 <- read.csv("https://github.com/ogmassad/NFL-Big-Data-Bowl-2025/releases/download/NFL_BDB_Data/tracking_week_8.csv")
week9 <- read.csv("https://github.com/ogmassad/NFL-Big-Data-Bowl-2025/releases/download/NFL_BDB_Data/tracking_week_9.csv")

#results of the plays
addmargins(table(play$passResult))

#players who are rushing the qb
rushers <- player_play %>%
  filter(wasInitialPassRusher == 1) %>%
  group_by(gameId, playId) %>%
  summarise(num_rushers = n(), .groups = "drop")

#plays where there are blitzes
rushers_5 <- rushers %>% filter(num_rushers >= 5)

#play data when there is a blitz
detailed <- rushers_5 %>%
  inner_join(play, by = c("gameId", "playId"))

#player play data when there is a blitz
player_play_blitz <- rushers_5 %>%
  inner_join(player_play, by = c("gameId", "playId"))
player_play_blitz <- players_pos %>%
  inner_join(player_play_blitz, by = c("nflId"))
```


```{r}
#subsetting blitz plays for those with rushers from the secondary
#secondary players 
secondary <- c("CB", "FS", "SS")

#blitzers
player_play_blitzers <-subset(player_play_blitz, wasInitialPassRusher == 1)

# Filter for blitz plays with secondary rushers
plays_with_secondary_rusher <- player_play_blitzers %>%
  filter(position %in% secondary) %>%
  dplyr::select(gameId, playId) %>%
  distinct()

#play data with secondary rusher
plays_second <- plays_with_secondary_rusher %>%
  inner_join(play, by = c("gameId", "playId"))

#player_play data with secondary rusher
player_play_second <- plays_with_secondary_rusher %>%
  inner_join(player_play, by = c("gameId", "playId"))

plays_second <- subset(plays_second, passResult != '')
#results of blitz plays with secondary rushers
table_second <- table(plays_second$passResult)



#subsetting blitz plays for those that don't have rushers from the secondary
# Rows not included in the subset (no secondary rusher)
plays_without_secondary_rusher <- detailed %>%
  anti_join(plays_with_secondary_rusher, by = c("gameId", "playId"))

#player_play data without secondary rusher
player_play_nosecond <- plays_without_secondary_rusher %>%
  inner_join(player_play, by = c("gameId", "playId"))


plays_without_secondary_rusher <- subset(plays_without_secondary_rusher, passResult != '')

#results of blitz plays without secondary rushers
table_nosecond <- table(plays_without_secondary_rusher$passResult)

table(player_play_blitz$pff_defensiveCoverageAssignment)

prop.table(table_nosecond)*100
prop.table(table_second)*100
```

```{r}
#adding distance from ball for each player

#week1
# Identify rows corresponding to the ball (where nflId is NA)
ball_data1 <- week1 %>%
  filter(is.na(nflId)) %>%
  arrange(gameId, playId, frameId) %>%
  group_by(gameId, playId) %>%
  mutate(
    ball_x_diff = x - lag(x),  # Change in x position
    o_ball = case_when(
      playDirection == "right" ~ 0,        # Moving right
      playDirection == "left" ~ 180,       # Moving left
      ball_x_diff > 0 ~ 0,                 # Fallback for right movement
      ball_x_diff < 0 ~ 180,               # Fallback for left movement
      TRUE ~ NA_real_                      # If no movement detected
    )
  ) %>%
  dplyr::select(gameId, playId, frameId, ball_x = x, ball_y = y, o_ball)


# Join the ball's coordinates back to the full dataset
week1 <- week1 %>%
  left_join(ball_data1, by = c("gameId", "playId", "frameId"))
remove(ball_data1)

# Calculate the distance of each player from the ball
week1 <- week1 %>%
  mutate(distance_from_ball = ifelse(is.na(nflId), NA, sqrt((x - ball_x)^2 + (y - ball_y)^2)))

week1 <- week1 %>%
  mutate(
    # Calculate angle from ball to player using ball_x and ball_y
    angle_to_ball = atan2(y - ball_y, x - ball_x) * 180 / pi,
    angle_to_ball = ifelse(angle_to_ball < 0, angle_to_ball + 360, angle_to_ball),  # Normalize to [0, 360)

    # Adjust player's orientation relative to the ball
    adjusted_player_orientation = (o - o_ball + 180) %% 360,  
    adjusted_player_orientation = ifelse(adjusted_player_orientation > 180, 
                                         adjusted_player_orientation - 360, 
                                         adjusted_player_orientation),

    # Relative Orientation where 180 means facing the ball directly
    relative_orientation = abs(180 - adjusted_player_orientation),
    relative_orientation = ifelse(relative_orientation > 180, 360 - relative_orientation, relative_orientation)
  )

#scientific notation to standard
options(scipen = 999)


#week2
# Identify rows corresponding to the ball (where nflId is NA)
ball_data2 <- week2 %>%
  filter(is.na(nflId)) %>%
  arrange(gameId, playId, frameId) %>%
  group_by(gameId, playId) %>%
  mutate(
    ball_x_diff = x - lag(x),  # Change in x position
    o_ball = case_when(
      playDirection == "right" ~ 0,        # Moving right
      playDirection == "left" ~ 180,       # Moving left
      ball_x_diff > 0 ~ 0,                 # Fallback for right movement
      ball_x_diff < 0 ~ 180,               # Fallback for left movement
      TRUE ~ NA_real_                      # If no movement detected
    )
  ) %>%
  dplyr::select(gameId, playId, frameId, ball_x = x, ball_y = y, o_ball)


# Join the ball's coordinates back to the full dataset
week2 <- week2 %>%
  left_join(ball_data2, by = c("gameId", "playId", "frameId"))
remove(ball_data2)

# Calculate the distance of each player from the ball
week2 <- week2 %>%
  mutate(distance_from_ball = ifelse(is.na(nflId), NA, sqrt((x - ball_x)^2 + (y - ball_y)^2)))

week2 <- week2 %>%
  mutate(
    # Calculate angle from ball to player using ball_x and ball_y
    angle_to_ball = atan2(y - ball_y, x - ball_x) * 180 / pi,
    angle_to_ball = ifelse(angle_to_ball < 0, angle_to_ball + 360, angle_to_ball),  # Normalize to [0, 360)

    # Adjust player's orientation relative to the ball
    adjusted_player_orientation = (o - o_ball + 180) %% 360,  
    adjusted_player_orientation = ifelse(adjusted_player_orientation > 180, 
                                         adjusted_player_orientation - 360, 
                                         adjusted_player_orientation),

    # Relative Orientation where 180 means facing the ball directly
    relative_orientation = abs(180 - adjusted_player_orientation),
    relative_orientation = ifelse(relative_orientation > 180, 360 - relative_orientation, relative_orientation)
  )

#scientific notation to standard
options(scipen = 999)


#week3
# Identify rows corresponding to the ball (where nflId is NA)
ball_data3 <- week3 %>%
  filter(is.na(nflId)) %>%
  arrange(gameId, playId, frameId) %>%
  group_by(gameId, playId) %>%
  mutate(
    ball_x_diff = x - lag(x),  # Change in x position
    o_ball = case_when(
      playDirection == "right" ~ 0,        # Moving right
      playDirection == "left" ~ 180,       # Moving left
      ball_x_diff > 0 ~ 0,                 # Fallback for right movement
      ball_x_diff < 0 ~ 180,               # Fallback for left movement
      TRUE ~ NA_real_                      # If no movement detected
    )
  ) %>%
  dplyr::select(gameId, playId, frameId, ball_x = x, ball_y = y, o_ball)


# Join the ball's coordinates back to the full dataset
week3 <- week3 %>%
  left_join(ball_data3, by = c("gameId", "playId", "frameId"))
remove(ball_data3)

# Calculate the distance of each player from the ball
week3 <- week3 %>%
  mutate(distance_from_ball = ifelse(is.na(nflId), NA, sqrt((x - ball_x)^2 + (y - ball_y)^2)))

week3 <- week3 %>%
  mutate(
    # Calculate angle from ball to player using ball_x and ball_y
    angle_to_ball = atan2(y - ball_y, x - ball_x) * 180 / pi,
    angle_to_ball = ifelse(angle_to_ball < 0, angle_to_ball + 360, angle_to_ball),  # Normalize to [0, 360)

    # Adjust player's orientation relative to the ball
    adjusted_player_orientation = (o - o_ball + 180) %% 360,  
    adjusted_player_orientation = ifelse(adjusted_player_orientation > 180, 
                                         adjusted_player_orientation - 360, 
                                         adjusted_player_orientation),

    # Relative Orientation where 180 means facing the ball directly
    relative_orientation = abs(180 - adjusted_player_orientation),
    relative_orientation = ifelse(relative_orientation > 180, 360 - relative_orientation, relative_orientation)
  )

#scientific notation to standard
options(scipen = 999)


#week4
# Identify rows corresponding to the ball (where nflId is NA)
ball_data4 <- week4 %>%
  filter(is.na(nflId)) %>%
  arrange(gameId, playId, frameId) %>%
  group_by(gameId, playId) %>%
  mutate(
    ball_x_diff = x - lag(x),  # Change in x position
    o_ball = case_when(
      playDirection == "right" ~ 0,        # Moving right
      playDirection == "left" ~ 180,       # Moving left
      ball_x_diff > 0 ~ 0,                 # Fallback for right movement
      ball_x_diff < 0 ~ 180,               # Fallback for left movement
      TRUE ~ NA_real_                      # If no movement detected
    )
  ) %>%
  dplyr::select(gameId, playId, frameId, ball_x = x, ball_y = y, o_ball)


# Join the ball's coordinates back to the full dataset
week4 <- week4 %>%
  left_join(ball_data4, by = c("gameId", "playId", "frameId"))
remove(ball_data4)

# Calculate the distance of each player from the ball
week4 <- week4 %>%
  mutate(distance_from_ball = ifelse(is.na(nflId), NA, sqrt((x - ball_x)^2 + (y - ball_y)^2)))

week4 <- week4 %>%
  mutate(
    # Calculate angle from ball to player using ball_x and ball_y
    angle_to_ball = atan2(y - ball_y, x - ball_x) * 180 / pi,
    angle_to_ball = ifelse(angle_to_ball < 0, angle_to_ball + 360, angle_to_ball),  # Normalize to [0, 360)

    # Adjust player's orientation relative to the ball
    adjusted_player_orientation = (o - o_ball + 180) %% 360,  
    adjusted_player_orientation = ifelse(adjusted_player_orientation > 180, 
                                         adjusted_player_orientation - 360, 
                                         adjusted_player_orientation),

    # Relative Orientation where 180 means facing the ball directly
    relative_orientation = abs(180 - adjusted_player_orientation),
    relative_orientation = ifelse(relative_orientation > 180, 360 - relative_orientation, relative_orientation)
  )

#scientific notation to standard
options(scipen = 999)


#week5
# Identify rows corresponding to the ball (where nflId is NA)
ball_data5 <- week5 %>%
  filter(is.na(nflId)) %>%
  arrange(gameId, playId, frameId) %>%
  group_by(gameId, playId) %>%
  mutate(
    ball_x_diff = x - lag(x),  # Change in x position
    o_ball = case_when(
      playDirection == "right" ~ 0,        # Moving right
      playDirection == "left" ~ 180,       # Moving left
      ball_x_diff > 0 ~ 0,                 # Fallback for right movement
      ball_x_diff < 0 ~ 180,               # Fallback for left movement
      TRUE ~ NA_real_                      # If no movement detected
    )
  ) %>%
  dplyr::select(gameId, playId, frameId, ball_x = x, ball_y = y, o_ball)


# Join the ball's coordinates back to the full dataset
week5 <- week5 %>%
  left_join(ball_data5, by = c("gameId", "playId", "frameId"))
remove(ball_data5)

# Calculate the distance of each player from the ball
week5 <- week5 %>%
  mutate(distance_from_ball = ifelse(is.na(nflId), NA, sqrt((x - ball_x)^2 + (y - ball_y)^2)))

week5 <- week5 %>%
  mutate(
    # Calculate angle from ball to player using ball_x and ball_y
    angle_to_ball = atan2(y - ball_y, x - ball_x) * 180 / pi,
    angle_to_ball = ifelse(angle_to_ball < 0, angle_to_ball + 360, angle_to_ball),  # Normalize to [0, 360)

    # Adjust player's orientation relative to the ball
    adjusted_player_orientation = (o - o_ball + 180) %% 360,  
    adjusted_player_orientation = ifelse(adjusted_player_orientation > 180, 
                                         adjusted_player_orientation - 360, 
                                         adjusted_player_orientation),

    # Relative Orientation where 180 means facing the ball directly
    relative_orientation = abs(180 - adjusted_player_orientation),
    relative_orientation = ifelse(relative_orientation > 180, 360 - relative_orientation, relative_orientation)
  )

#scientific notation to standard
options(scipen = 999)


#week6
# Identify rows corresponding to the ball (where nflId is NA)
ball_data6 <- week6 %>%
  filter(is.na(nflId)) %>%
  arrange(gameId, playId, frameId) %>%
  group_by(gameId, playId) %>%
  mutate(
    ball_x_diff = x - lag(x),  # Change in x position
    o_ball = case_when(
      playDirection == "right" ~ 0,        # Moving right
      playDirection == "left" ~ 180,       # Moving left
      ball_x_diff > 0 ~ 0,                 # Fallback for right movement
      ball_x_diff < 0 ~ 180,               # Fallback for left movement
      TRUE ~ NA_real_                      # If no movement detected
    )
  ) %>%
  dplyr::select(gameId, playId, frameId, ball_x = x, ball_y = y, o_ball)


# Join the ball's coordinates back to the full dataset
week6 <- week6 %>%
  left_join(ball_data6, by = c("gameId", "playId", "frameId"))
remove(ball_data6)

# Calculate the distance of each player from the ball
week6 <- week6 %>%
  mutate(distance_from_ball = ifelse(is.na(nflId), NA, sqrt((x - ball_x)^2 + (y - ball_y)^2)))

week6 <- week6 %>%
  mutate(
    # Calculate angle from ball to player using ball_x and ball_y
    angle_to_ball = atan2(y - ball_y, x - ball_x) * 180 / pi,
    angle_to_ball = ifelse(angle_to_ball < 0, angle_to_ball + 360, angle_to_ball),  # Normalize to [0, 360)

    # Adjust player's orientation relative to the ball
    adjusted_player_orientation = (o - o_ball + 180) %% 360,  
    adjusted_player_orientation = ifelse(adjusted_player_orientation > 180, 
                                         adjusted_player_orientation - 360, 
                                         adjusted_player_orientation),

    # Relative Orientation where 180 means facing the ball directly
    relative_orientation = abs(180 - adjusted_player_orientation),
    relative_orientation = ifelse(relative_orientation > 180, 360 - relative_orientation, relative_orientation)
  )

#scientific notation to standard
options(scipen = 999)


#week7
# Identify rows corresponding to the ball (where nflId is NA)
ball_data7 <- week7 %>%
  filter(is.na(nflId)) %>%
  arrange(gameId, playId, frameId) %>%
  group_by(gameId, playId) %>%
  mutate(
    ball_x_diff = x - lag(x),  # Change in x position
    o_ball = case_when(
      playDirection == "right" ~ 0,        # Moving right
      playDirection == "left" ~ 180,       # Moving left
      ball_x_diff > 0 ~ 0,                 # Fallback for right movement
      ball_x_diff < 0 ~ 180,               # Fallback for left movement
      TRUE ~ NA_real_                      # If no movement detected
    )
  ) %>%
  dplyr::select(gameId, playId, frameId, ball_x = x, ball_y = y, o_ball)


# Join the ball's coordinates back to the full dataset
week7 <- week7 %>%
  left_join(ball_data7, by = c("gameId", "playId", "frameId"))
remove(ball_data7)

# Calculate the distance of each player from the ball
week7 <- week7 %>%
  mutate(distance_from_ball = ifelse(is.na(nflId), NA, sqrt((x - ball_x)^2 + (y - ball_y)^2)))

week7 <- week7 %>%
  mutate(
    # Calculate angle from ball to player using ball_x and ball_y
    angle_to_ball = atan2(y - ball_y, x - ball_x) * 180 / pi,
    angle_to_ball = ifelse(angle_to_ball < 0, angle_to_ball + 360, angle_to_ball),  # Normalize to [0, 360)

    # Adjust player's orientation relative to the ball
    adjusted_player_orientation = (o - o_ball + 180) %% 360,  
    adjusted_player_orientation = ifelse(adjusted_player_orientation > 180, 
                                         adjusted_player_orientation - 360, 
                                         adjusted_player_orientation),

    # Relative Orientation where 180 means facing the ball directly
    relative_orientation = abs(180 - adjusted_player_orientation),
    relative_orientation = ifelse(relative_orientation > 180, 360 - relative_orientation, relative_orientation)
  )

#scientific notation to standard
options(scipen = 999)


#week8
# Identify rows corresponding to the ball (where nflId is NA)
ball_data8 <- week8 %>%
  filter(is.na(nflId)) %>%
  arrange(gameId, playId, frameId) %>%
  group_by(gameId, playId) %>%
  mutate(
    ball_x_diff = x - lag(x),  # Change in x position
    o_ball = case_when(
      playDirection == "right" ~ 0,        # Moving right
      playDirection == "left" ~ 180,       # Moving left
      ball_x_diff > 0 ~ 0,                 # Fallback for right movement
      ball_x_diff < 0 ~ 180,               # Fallback for left movement
      TRUE ~ NA_real_                      # If no movement detected
    )
  ) %>%
  dplyr::select(gameId, playId, frameId, ball_x = x, ball_y = y, o_ball)


# Join the ball's coordinates back to the full dataset
week8 <- week8 %>%
  left_join(ball_data8, by = c("gameId", "playId", "frameId"))
remove(ball_data9)

# Calculate the distance of each player from the ball
week8 <- week8 %>%
  mutate(distance_from_ball = ifelse(is.na(nflId), NA, sqrt((x - ball_x)^2 + (y - ball_y)^2)))

week8 <- week8 %>%
  mutate(
    # Calculate angle from ball to player using ball_x and ball_y
    angle_to_ball = atan2(y - ball_y, x - ball_x) * 180 / pi,
    angle_to_ball = ifelse(angle_to_ball < 0, angle_to_ball + 360, angle_to_ball),  # Normalize to [0, 360)

    # Adjust player's orientation relative to the ball
    adjusted_player_orientation = (o - o_ball + 180) %% 360,  
    adjusted_player_orientation = ifelse(adjusted_player_orientation > 180, 
                                         adjusted_player_orientation - 360, 
                                         adjusted_player_orientation),

    # Relative Orientation where 180 means facing the ball directly
    relative_orientation = abs(180 - adjusted_player_orientation),
    relative_orientation = ifelse(relative_orientation > 180, 360 - relative_orientation, relative_orientation)
  )

#scientific notation to standard
options(scipen = 999)


#week9
# Identify rows corresponding to the ball (where nflId is NA)
ball_data9 <- week9 %>%
  filter(is.na(nflId)) %>%
  arrange(gameId, playId, frameId) %>%
  group_by(gameId, playId) %>%
  mutate(
    ball_x_diff = x - lag(x),  # Change in x position
    o_ball = case_when(
      playDirection == "right" ~ 0,        # Moving right
      playDirection == "left" ~ 180,       # Moving left
      ball_x_diff > 0 ~ 0,                 # Fallback for right movement
      ball_x_diff < 0 ~ 180,               # Fallback for left movement
      TRUE ~ NA_real_                      # If no movement detected
    )
  ) %>%
  dplyr::select(gameId, playId, frameId, ball_x = x, ball_y = y, o_ball)


# Join the ball's coordinates back to the full dataset
week9 <- week9 %>%
  left_join(ball_data9, by = c("gameId", "playId", "frameId"))
remove(ball_data9)

# Calculate the distance of each player from the ball
week9 <- week9 %>%
  mutate(distance_from_ball = ifelse(is.na(nflId), NA, sqrt((x - ball_x)^2 + (y - ball_y)^2)))

week9 <- week9 %>%
  mutate(
    # Calculate angle from ball to player using ball_x and ball_y
    angle_to_ball = atan2(y - ball_y, x - ball_x) * 180 / pi,
    angle_to_ball = ifelse(angle_to_ball < 0, angle_to_ball + 360, angle_to_ball),  # Normalize to [0, 360)

    # Adjust player's orientation relative to the ball
    adjusted_player_orientation = (o - o_ball + 180) %% 360,  
    adjusted_player_orientation = ifelse(adjusted_player_orientation > 180, 
                                         adjusted_player_orientation - 360, 
                                         adjusted_player_orientation),

    # Relative Orientation where 180 means facing the ball directly
    relative_orientation = abs(180 - adjusted_player_orientation),
    relative_orientation = ifelse(relative_orientation > 180, 360 - relative_orientation, relative_orientation)
  )

#scientific notation to standard
options(scipen = 999)


```

```{r}
# Calculate the distance between each defender and their primary matchup

week1 <- week1 %>%
  left_join(player_play %>% 
              dplyr::select(gameId, playId, nflId, pff_primaryDefensiveCoverageMatchupNflId),
            by = c("gameId", "playId", "nflId"))

# Join the matchup player coordinates back to the main dataset
week1 <- week1 %>%
  left_join(week1 %>%
              dplyr::select(gameId, playId, nflId, frameId, x, y) %>%
              rename(matchup_x = x, matchup_y = y),
            by = c("gameId", "playId", "frameId", 
                   "pff_primaryDefensiveCoverageMatchupNflId" = "nflId"))
week1 <- week1 %>%
  mutate(
    distance_to_matchup = ifelse(!is.na(pff_primaryDefensiveCoverageMatchupNflId),
                                 sqrt((x - matchup_x)^2 + (y - matchup_y)^2),
                                 NA_real_)
  )


week2 <- week2 %>%
  left_join(player_play %>% 
              dplyr::select(gameId, playId, nflId, pff_primaryDefensiveCoverageMatchupNflId),
            by = c("gameId", "playId", "nflId"))

# Join the matchup player coordinates back to the main dataset
week2 <- week2 %>%
  left_join(week2 %>%
              dplyr::select(gameId, playId, nflId, frameId, x, y) %>%
              rename(matchup_x = x, matchup_y = y),
            by = c("gameId", "playId", "frameId", 
                   "pff_primaryDefensiveCoverageMatchupNflId" = "nflId"))
week2 <- week2 %>%
  mutate(
    distance_to_matchup = ifelse(!is.na(pff_primaryDefensiveCoverageMatchupNflId),
                                 sqrt((x - matchup_x)^2 + (y - matchup_y)^2),
                                 NA_real_)
  )


week3 <- week3 %>%
  left_join(player_play %>% 
              dplyr::select(gameId, playId, nflId, pff_primaryDefensiveCoverageMatchupNflId),
            by = c("gameId", "playId", "nflId"))

# Join the matchup player coordinates back to the main dataset
week3 <- week3 %>%
  left_join(week3 %>%
              dplyr::select(gameId, playId, nflId, frameId, x, y) %>%
              rename(matchup_x = x, matchup_y = y),
            by = c("gameId", "playId", "frameId", 
                   "pff_primaryDefensiveCoverageMatchupNflId" = "nflId"))
week3 <- week3 %>%
  mutate(
    distance_to_matchup = ifelse(!is.na(pff_primaryDefensiveCoverageMatchupNflId),
                                 sqrt((x - matchup_x)^2 + (y - matchup_y)^2),
                                 NA_real_)
  )


week4 <- week4 %>%
  left_join(player_play %>% 
              dplyr::select(gameId, playId, nflId, pff_primaryDefensiveCoverageMatchupNflId),
            by = c("gameId", "playId", "nflId"))

# Join the matchup player coordinates back to the main dataset
week4 <- week4 %>%
  left_join(week4 %>%
              dplyr::select(gameId, playId, nflId, frameId, x, y) %>%
              rename(matchup_x = x, matchup_y = y),
            by = c("gameId", "playId", "frameId", 
                   "pff_primaryDefensiveCoverageMatchupNflId" = "nflId"))
week4 <- week4 %>%
  mutate(
    distance_to_matchup = ifelse(!is.na(pff_primaryDefensiveCoverageMatchupNflId),
                                 sqrt((x - matchup_x)^2 + (y - matchup_y)^2),
                                 NA_real_)
  )

week5 <- week5 %>%
  left_join(player_play %>% 
              dplyr::select(gameId, playId, nflId, pff_primaryDefensiveCoverageMatchupNflId),
            by = c("gameId", "playId", "nflId"))

# Join the matchup player coordinates back to the main dataset
week5 <- week5 %>%
  left_join(week5 %>%
              dplyr::select(gameId, playId, nflId, frameId, x, y) %>%
              rename(matchup_x = x, matchup_y = y),
            by = c("gameId", "playId", "frameId", 
                   "pff_primaryDefensiveCoverageMatchupNflId" = "nflId"))
week5 <- week5 %>%
  mutate(
    distance_to_matchup = ifelse(!is.na(pff_primaryDefensiveCoverageMatchupNflId),
                                 sqrt((x - matchup_x)^2 + (y - matchup_y)^2),
                                 NA_real_)
  )


week6 <- week6 %>%
  left_join(player_play %>% 
              dplyr::select(gameId, playId, nflId, pff_primaryDefensiveCoverageMatchupNflId),
            by = c("gameId", "playId", "nflId"))

# Join the matchup player coordinates back to the main dataset
week6 <- week6 %>%
  left_join(week6 %>%
              dplyr::select(gameId, playId, nflId, frameId, x, y) %>%
              rename(matchup_x = x, matchup_y = y),
            by = c("gameId", "playId", "frameId", 
                   "pff_primaryDefensiveCoverageMatchupNflId" = "nflId"))
week6 <- week6 %>%
  mutate(
    distance_to_matchup = ifelse(!is.na(pff_primaryDefensiveCoverageMatchupNflId),
                                 sqrt((x - matchup_x)^2 + (y - matchup_y)^2),
                                 NA_real_)
  )


week7 <- week7 %>%
  left_join(player_play %>% 
              dplyr::select(gameId, playId, nflId, pff_primaryDefensiveCoverageMatchupNflId),
            by = c("gameId", "playId", "nflId"))

# Join the matchup player coordinates back to the main dataset
week7 <- week7 %>%
  left_join(week7 %>%
              dplyr::select(gameId, playId, nflId, frameId, x, y) %>%
              rename(matchup_x = x, matchup_y = y),
            by = c("gameId", "playId", "frameId", 
                   "pff_primaryDefensiveCoverageMatchupNflId" = "nflId"))
week7 <- week7 %>%
  mutate(
    distance_to_matchup = ifelse(!is.na(pff_primaryDefensiveCoverageMatchupNflId),
                                 sqrt((x - matchup_x)^2 + (y - matchup_y)^2),
                                 NA_real_)
  )


week8 <- week8 %>%
  left_join(player_play %>% 
              dplyr::select(gameId, playId, nflId, pff_primaryDefensiveCoverageMatchupNflId),
            by = c("gameId", "playId", "nflId"))

# Join the matchup player coordinates back to the main dataset
week8 <- week8 %>%
  left_join(week8 %>%
              dplyr::select(gameId, playId, nflId, frameId, x, y) %>%
              rename(matchup_x = x, matchup_y = y),
            by = c("gameId", "playId", "frameId", 
                   "pff_primaryDefensiveCoverageMatchupNflId" = "nflId"))
week8 <- week8 %>%
  mutate(
    distance_to_matchup = ifelse(!is.na(pff_primaryDefensiveCoverageMatchupNflId),
                                 sqrt((x - matchup_x)^2 + (y - matchup_y)^2),
                                 NA_real_)
  )


week9 <- week9 %>%
  left_join(player_play %>% 
              dplyr::select(gameId, playId, nflId, pff_primaryDefensiveCoverageMatchupNflId),
            by = c("gameId", "playId", "nflId"))

# Join the matchup player coordinates back to the main dataset
week9 <- week9 %>%
  left_join(week9 %>%
              dplyr::select(gameId, playId, nflId, frameId, x, y) %>%
              rename(matchup_x = x, matchup_y = y),
            by = c("gameId", "playId", "frameId", 
                   "pff_primaryDefensiveCoverageMatchupNflId" = "nflId"))
week9 <- week9 %>%
  mutate(
    distance_to_matchup = ifelse(!is.na(pff_primaryDefensiveCoverageMatchupNflId),
                                 sqrt((x - matchup_x)^2 + (y - matchup_y)^2),
                                 NA_real_)
  )
```

```{r}

```
