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

# Calculate cumulative movement towards the ball before the snap
week1 <- week1 %>%
  group_by(gameId, playId, nflId) %>%
  mutate(
    # Change in distance from ball over consecutive frames
    movement_towards_ball = ifelse(frameType == "BEFORE_SNAP",
                                   lag(distance_from_ball) - distance_from_ball,
                                   NA_real_),

    # Cumulative movement towards the ball before the snap
    cumulative_movement_towards_ball = ifelse(frameType == "BEFORE_SNAP",
                                              cumsum(ifelse(is.na(movement_towards_ball), 0, movement_towards_ball)),
                                              NA_real_)
  ) %>%
  ungroup()


week2 <- week2 %>%
  group_by(gameId, playId, nflId) %>%
  mutate(
    # Change in distance from ball over consecutive frames
    movement_towards_ball = ifelse(frameType == "BEFORE_SNAP",
                                   lag(distance_from_ball) - distance_from_ball,
                                   NA_real_),

    # Cumulative movement towards the ball before the snap
    cumulative_movement_towards_ball = ifelse(frameType == "BEFORE_SNAP",
                                              cumsum(ifelse(is.na(movement_towards_ball), 0, movement_towards_ball)),
                                              NA_real_)
  ) %>%
  ungroup()


week3 <- week3 %>%
  group_by(gameId, playId, nflId) %>%
  mutate(
    # Change in distance from ball over consecutive frames
    movement_towards_ball = ifelse(frameType == "BEFORE_SNAP",
                                   lag(distance_from_ball) - distance_from_ball,
                                   NA_real_),

    # Cumulative movement towards the ball before the snap
    cumulative_movement_towards_ball = ifelse(frameType == "BEFORE_SNAP",
                                              cumsum(ifelse(is.na(movement_towards_ball), 0, movement_towards_ball)),
                                              NA_real_)
  ) %>%
  ungroup()


week4 <- week4 %>%
  group_by(gameId, playId, nflId) %>%
  mutate(
    # Change in distance from ball over consecutive frames
    movement_towards_ball = ifelse(frameType == "BEFORE_SNAP",
                                   lag(distance_from_ball) - distance_from_ball,
                                   NA_real_),

    # Cumulative movement towards the ball before the snap
    cumulative_movement_towards_ball = ifelse(frameType == "BEFORE_SNAP",
                                              cumsum(ifelse(is.na(movement_towards_ball), 0, movement_towards_ball)),
                                              NA_real_)
  ) %>%
  ungroup()


week5 <- week5 %>%
  group_by(gameId, playId, nflId) %>%
  mutate(
    # Change in distance from ball over consecutive frames
    movement_towards_ball = ifelse(frameType == "BEFORE_SNAP",
                                   lag(distance_from_ball) - distance_from_ball,
                                   NA_real_),

    # Cumulative movement towards the ball before the snap
    cumulative_movement_towards_ball = ifelse(frameType == "BEFORE_SNAP",
                                              cumsum(ifelse(is.na(movement_towards_ball), 0, movement_towards_ball)),
                                              NA_real_)
  ) %>%
  ungroup()


week6 <- week6 %>%
  group_by(gameId, playId, nflId) %>%
  mutate(
    # Change in distance from ball over consecutive frames
    movement_towards_ball = ifelse(frameType == "BEFORE_SNAP",
                                   lag(distance_from_ball) - distance_from_ball,
                                   NA_real_),

    # Cumulative movement towards the ball before the snap
    cumulative_movement_towards_ball = ifelse(frameType == "BEFORE_SNAP",
                                              cumsum(ifelse(is.na(movement_towards_ball), 0, movement_towards_ball)),
                                              NA_real_)
  ) %>%
  ungroup()


week7 <- week7 %>%
  group_by(gameId, playId, nflId) %>%
  mutate(
    # Change in distance from ball over consecutive frames
    movement_towards_ball = ifelse(frameType == "BEFORE_SNAP",
                                   lag(distance_from_ball) - distance_from_ball,
                                   NA_real_),

    # Cumulative movement towards the ball before the snap
    cumulative_movement_towards_ball = ifelse(frameType == "BEFORE_SNAP",
                                              cumsum(ifelse(is.na(movement_towards_ball), 0, movement_towards_ball)),
                                              NA_real_)
  ) %>%
  ungroup()


week8 <- week8 %>%
  group_by(gameId, playId, nflId) %>%
  mutate(
    # Change in distance from ball over consecutive frames
    movement_towards_ball = ifelse(frameType == "BEFORE_SNAP",
                                   lag(distance_from_ball) - distance_from_ball,
                                   NA_real_),

    # Cumulative movement towards the ball before the snap
    cumulative_movement_towards_ball = ifelse(frameType == "BEFORE_SNAP",
                                              cumsum(ifelse(is.na(movement_towards_ball), 0, movement_towards_ball)),
                                              NA_real_)
  ) %>%
  ungroup()


week9 <- week9 %>%
  group_by(gameId, playId, nflId) %>%
  mutate(
    # Change in distance from ball over consecutive frames
    movement_towards_ball = ifelse(frameType == "BEFORE_SNAP",
                                   lag(distance_from_ball) - distance_from_ball,
                                   NA_real_),

    # Cumulative movement towards the ball before the snap
    cumulative_movement_towards_ball = ifelse(frameType == "BEFORE_SNAP",
                                              cumsum(ifelse(is.na(movement_towards_ball), 0, movement_towards_ball)),
                                              NA_real_)
  ) %>%
  ungroup()

#Merging pass results and positions 

#week1
week1 <- week1 %>%
  left_join(play %>% dplyr::select(gameId, playId, passResult), by = c("gameId", "playId"))
week1 <- week1 %>%
  left_join(players %>% dplyr::select(nflId, position), by = "nflId")

#week2
week2 <- week2 %>%
  left_join(play %>% dplyr::select(gameId, playId, passResult), by = c("gameId", "playId"))
week2 <- week2 %>%
  left_join(players %>% dplyr::select(nflId, position), by = "nflId")

#week3
week3 <- week3 %>%
  left_join(play %>% dplyr::select(gameId, playId, passResult), by = c("gameId", "playId"))
week3 <- week3 %>%
  left_join(players %>% dplyr::select(nflId, position), by = "nflId")

#week4
week4 <- week4 %>%
  left_join(play %>% dplyr::select(gameId, playId, passResult), by = c("gameId", "playId"))
week4 <- week4 %>%
  left_join(players %>% dplyr::select(nflId, position), by = "nflId")

#week5
week5 <- week5 %>%
  left_join(play %>% dplyr::select(gameId, playId, passResult), by = c("gameId", "playId"))
week5 <- week5 %>%
  left_join(players %>% dplyr::select(nflId, position), by = "nflId")

#week6
week6 <- week6 %>%
  left_join(play %>% dplyr::select(gameId, playId, passResult), by = c("gameId", "playId"))
week6 <- week6 %>%
  left_join(players %>% dplyr::select(nflId, position), by = "nflId")

#week7
week7 <- week7 %>%
  left_join(play %>% dplyr::select(gameId, playId, passResult), by = c("gameId", "playId"))
week7 <- week7 %>%
  left_join(players %>% dplyr::select(nflId, position), by = "nflId")

#week8
week8 <- week8 %>%
  left_join(play %>% dplyr::select(gameId, playId, passResult), by = c("gameId", "playId"))
week8 <- week8 %>%
  left_join(players %>% dplyr::select(nflId, position), by = "nflId")

#week9
week9 <- week9 %>%
  left_join(play %>% dplyr::select(gameId, playId, passResult), by = c("gameId", "playId"))
week9 <- week9 %>%
  left_join(players %>% dplyr::select(nflId, position), by = "nflId")

#blitzers for each week

#blitzers
player_play_blitzers <-subset(player_play_blitz, wasInitialPassRusher == 1)


#week 1 blitzers
week1_blitzers <- player_play_blitzers %>%
  inner_join(week1, by = c("gameId", "playId", "nflId"))
week1_detailed <- week1_blitzers %>%
  inner_join(detailed, by = c("gameId", "playId"))
remove(week1_blitzers)

#week 2 blitzers
week2_blitzers <- player_play_blitzers %>%
  inner_join(week2, by = c("gameId", "playId", "nflId"))
week2_detailed <- week2_blitzers %>%
  inner_join(detailed, by = c("gameId", "playId"))
remove(week2_blitzers)

#week 3 blitzers
week3_blitzers <- player_play_blitzers %>%
  inner_join(week3, by = c("gameId", "playId", "nflId"))
week3_detailed <- week3_blitzers %>%
  inner_join(detailed, by = c("gameId", "playId"))
remove(week3_blitzers)

#week 4 blitzers
week4_blitzers <- player_play_blitzers %>%
  inner_join(week4, by = c("gameId", "playId", "nflId"))
week4_detailed <- week4_blitzers %>%
  inner_join(detailed, by = c("gameId", "playId"))
remove(week4_blitzers)

#week 5 blitzers
week5_blitzers <- player_play_blitzers %>%
  inner_join(week5, by = c("gameId", "playId", "nflId"))
week5_detailed <- week5_blitzers %>%
  inner_join(detailed, by = c("gameId", "playId"))
remove(week5_blitzers)

#week 6 blitzers
week6_blitzers <- player_play_blitzers %>%
  inner_join(week6, by = c("gameId", "playId", "nflId"))
week6_detailed <- week6_blitzers %>%
  inner_join(detailed, by = c("gameId", "playId"))
remove(week6_blitzers)

#week 7 blitzers
week7_blitzers <- player_play_blitzers %>%
  inner_join(week7, by = c("gameId", "playId", "nflId"))
week7_detailed <- week7_blitzers %>%
  inner_join(detailed, by = c("gameId", "playId"))
remove(week7_blitzers)

#week 8 blitzers
week8_blitzers <- player_play_blitzers %>%
  inner_join(week8, by = c("gameId", "playId", "nflId"))
week8_detailed <- week8_blitzers %>%
  inner_join(detailed, by = c("gameId", "playId"))
remove(week8_blitzers)

#week 9 blitzers
week9_blitzers <- player_play_blitzers %>%
  inner_join(week9, by = c("gameId", "playId", "nflId"))
week9_detailed <- week9_blitzers %>%
  inner_join(detailed, by = c("gameId", "playId"))
remove(week9_blitzers)

#secondary players only
desired_positions <- c("CB", "SS", "FS")  

week1_filtered <- week1 %>%
  filter(position %in% desired_positions)


week2_filtered <- week2 %>%
  filter(position %in% desired_positions)
remove(week2)

week3_filtered <- week3 %>%
  filter(position %in% desired_positions)
remove(week3)

week4_filtered <- week4 %>%
  filter(position %in% desired_positions)
remove(week4)

week5_filtered <- week5 %>%
  filter(position %in% desired_positions)


week6_filtered <- week6 %>%
  filter(position %in% desired_positions)
remove(week6)

week7_filtered <- week7 %>%
  filter(position %in% desired_positions)
remove(week7)

week8_filtered <- week8 %>%
  filter(position %in% desired_positions)
remove(week8)

week9_filtered <- week9 %>%
  filter(position %in% desired_positions)
remove(week9)

#removing blitzers from other data set 
week1_filtered <- week1_filtered %>%
  anti_join(week1_detailed %>% dplyr::select(gameId, playId, nflId), 
            by = c("gameId", "playId", "nflId"))

week2_filtered <- week2_filtered %>%
  anti_join(week2_detailed %>% dplyr::select(gameId, playId, nflId), 
            by = c("gameId", "playId", "nflId"))

week3_filtered <- week3_filtered %>%
  anti_join(week3_detailed %>% dplyr::select(gameId, playId, nflId), 
            by = c("gameId", "playId", "nflId"))

week4_filtered <- week4_filtered %>%
  anti_join(week4_detailed %>% dplyr::select(gameId, playId, nflId), 
            by = c("gameId", "playId", "nflId"))

week5_filtered <- week5_filtered %>%
  anti_join(week5_detailed %>% dplyr::select(gameId, playId, nflId), 
            by = c("gameId", "playId", "nflId"))

week6_filtered <- week6_filtered %>%
  anti_join(week6_detailed %>% dplyr::select(gameId, playId, nflId), 
            by = c("gameId", "playId", "nflId"))

week7_filtered <- week7_filtered %>%
  anti_join(week7_detailed %>% dplyr::select(gameId, playId, nflId), 
            by = c("gameId", "playId", "nflId"))

week8_filtered <- week8_filtered %>%
  anti_join(week8_detailed %>% dplyr::select(gameId, playId, nflId), 
            by = c("gameId", "playId", "nflId"))

week9_filtered <- week9_filtered %>%
  anti_join(week9_detailed %>% dplyr::select(gameId, playId, nflId), 
            by = c("gameId", "playId", "nflId"))


#removing columns from detailed data set
week1_detailed <- week1_detailed %>%
  dplyr::select(-hadRushAttempt, -rushingYards, -passingYards, -receivingYards, -wasTargettedReceiver, -yardageGainedAfterTheCatch)

week2_detailed <- week2_detailed %>%
  dplyr::select(-hadRushAttempt, -rushingYards, -passingYards, -receivingYards, -wasTargettedReceiver, -yardageGainedAfterTheCatch)

week3_detailed <- week3_detailed %>%
  dplyr::select(-hadRushAttempt, -rushingYards, -passingYards, -receivingYards, -wasTargettedReceiver, -yardageGainedAfterTheCatch)

week4_detailed <- week4_detailed %>%
  dplyr::select(-hadRushAttempt, -rushingYards, -passingYards, -receivingYards, -wasTargettedReceiver, -yardageGainedAfterTheCatch)

week5_detailed <- week5_detailed %>%
  dplyr::select(-hadRushAttempt, -rushingYards, -passingYards, -receivingYards, -wasTargettedReceiver, -yardageGainedAfterTheCatch)

week6_detailed <- week6_detailed %>%
  dplyr::select(-hadRushAttempt, -rushingYards, -passingYards, -receivingYards, -wasTargettedReceiver, -yardageGainedAfterTheCatch)

week7_detailed <- week7_detailed %>%
  dplyr::select(-hadRushAttempt, -rushingYards, -passingYards, -receivingYards, -wasTargettedReceiver, -yardageGainedAfterTheCatch)

week8_detailed <- week8_detailed %>%
  dplyr::select(-hadRushAttempt, -rushingYards, -passingYards, -receivingYards, -wasTargettedReceiver, -yardageGainedAfterTheCatch)

week9_detailed <- week9_detailed %>%
  dplyr::select(-hadRushAttempt, -rushingYards, -passingYards, -receivingYards, -wasTargettedReceiver, -yardageGainedAfterTheCatch)

# Remove columns where all values are NA

week1_detailed <- week1_detailed[, colSums(is.na(week1_detailed)) != nrow(week1_detailed)]

week2_detailed <- week2_detailed[, colSums(is.na(week2_detailed)) != nrow(week2_detailed)]

week3_detailed <- week3_detailed[, colSums(is.na(week3_detailed)) != nrow(week3_detailed)]

week4_detailed <- week4_detailed[, colSums(is.na(week4_detailed)) != nrow(week4_detailed)]

week5_detailed <- week5_detailed[, colSums(is.na(week5_detailed)) != nrow(week5_detailed)]

week6_detailed <- week6_detailed[, colSums(is.na(week6_detailed)) != nrow(week6_detailed)]

week7_detailed <- week7_detailed[, colSums(is.na(week7_detailed)) != nrow(week7_detailed)]

week8_detailed <- week8_detailed[, colSums(is.na(week8_detailed)) != nrow(week8_detailed)]

week9_detailed <- week9_detailed[, colSums(is.na(week9_detailed)) != nrow(week9_detailed)]

#filtering for only post snap data
week1_detailed <- subset(week1_detailed, frameType != "AFTER_SNAP")
week2_detailed <- subset(week2_detailed, frameType != "AFTER_SNAP")
week3_detailed <- subset(week3_detailed, frameType != "AFTER_SNAP")
week4_detailed <- subset(week4_detailed, frameType != "AFTER_SNAP")
week5_detailed <- subset(week5_detailed, frameType != "AFTER_SNAP")
week6_detailed <- subset(week6_detailed, frameType != "AFTER_SNAP")
week7_detailed <- subset(week7_detailed, frameType != "AFTER_SNAP")
week8_detailed <- subset(week8_detailed, frameType != "AFTER_SNAP")
week9_detailed <- subset(week9_detailed, frameType != "AFTER_SNAP")


week1_filtered <- subset(week1_filtered, frameType != "AFTER_SNAP")
week2_filtered <- subset(week2_filtered, frameType != "AFTER_SNAP")
week3_filtered <- subset(week3_filtered, frameType != "AFTER_SNAP")
week4_filtered <- subset(week4_filtered, frameType != "AFTER_SNAP")
week5_filtered <- subset(week5_filtered, frameType != "AFTER_SNAP")
week6_filtered <- subset(week6_filtered, frameType != "AFTER_SNAP")
week7_filtered <- subset(week7_filtered, frameType != "AFTER_SNAP")
week8_filtered <- subset(week8_filtered, frameType != "AFTER_SNAP")
week9_filtered <- subset(week9_filtered, frameType != "AFTER_SNAP")


#adding blitzers data into one data set

blitzers_track <- bind_rows(week1_detailed, week2_detailed, week3_detailed, week4_detailed, week5_detailed, week6_detailed, week7_detailed, week8_detailed, week9_detailed)

blitzers_track <- subset(blitzers_track, select = -position.y)

blitzers_track_second <- blitzers_track[blitzers_track$position.x %in% secondary_positions,]

names(blitzers_track_second)[names(blitzers_track_second) == "position.x"] <- "position"

# Filter to get frames exactly 3 frames before "ball_snap"
blitzers_track_second <- blitzers_track_second %>%
  group_by(gameId, playId, nflId) %>%
  mutate(
    # Identify the frame where "ball_snap" occurs, handling missing values correctly
    ball_snap_frame = if(any(!is.na(event) & event == "ball_snap")) {
      min(frameId[event == "ball_snap"], na.rm = TRUE)
    } else {
      NA_real_  # Return NA if ball_snap is missing
    }
  ) %>%
  # Filter to get exactly 3 frames before ball_snap
  filter(!is.na(ball_snap_frame) & frameId == (ball_snap_frame - 3)) %>%
  ungroup()

blitzers_track_second$sack <- blitzers_track_second$passResult.x == "S"
blitzers_track_second$blitz <- TRUE

#subsetting filtered data to have only secondary players
secondary_filtered <- bind_rows(week1_filtered, week2_filtered, week3_filtered, week4_filtered, week5_filtered, week6_filtered, week7_filtered, week8_filtered, week9_filtered)
secondary_filtered <- secondary_filtered[secondary_filtered$position %in% secondary_positions,]



# Filter to get frames exactly 3 frames before "ball_snap"
secondary_filtered <- secondary_filtered %>%
  group_by(gameId, playId, nflId) %>%
  mutate(
    # Identify the frame where "ball_snap" occurs, handling missing values correctly
    ball_snap_frame = if(any(!is.na(event) & event == "ball_snap")) {
      min(frameId[event == "ball_snap"], na.rm = TRUE)
    } else {
      NA_real_  # Return NA if ball_snap is missing
    }
  ) %>%
  # Filter to get exactly 3 frames before ball_snap
  filter(!is.na(ball_snap_frame) & frameId == (ball_snap_frame - 3)) %>%
  ungroup()

secondary_filtered$blitz <- FALSE


#hypothesis tests 
print(t.test(blitzers_track_second$distance_from_ball, secondary_filtered$distance_from_ball))

print(t.test(blitzers_track_second$relative_orientation, secondary_filtered$relative_orientation))

print(t.test(blitzers_track_second$a, secondary_filtered$a))

print(t.test(blitzers_track_second$cumulative_movement_towards_ball, secondary_filtered$cumulative_movement_towards_ball))

print(t.test(blitzers_track_second$s, secondary_filtered$s))

print(t.test(blitzers_track_second$dir, secondary_filtered$dir))

print(t.test(blitzers_track_second$angle_to_ball, secondary_filtered$angle_to_ball))

#data set of logistic regression
log_data <- bind_rows(secondary_filtered, blitzers_track_second)
library(dplyr)
log_data$relative_orientation <- (180 - log_data$relative_orientation)
# Ensure all relevant variables are included
log_data <- bind_rows(secondary_filtered_2, blitzers_track_second) %>%
  dplyr::select(blitz, s, a, distance_from_ball, dir, relative_orientation, cumulative_movement_towards_ball) %>%
  mutate(blitz = factor(blitz, levels = c(FALSE, TRUE))) %>%
  na.omit()
set.seed(2025)
train_index <- createDataPartition(log_data$blitz, p = 0.7, list = FALSE)
train <- log_data[train_index,]
test <- log_data[-train_index,]
# Logistic Regression with probability output
log_model <- glm(blitz ~ s + a + distance_from_ball  + cumulative_movement_towards_ball, 
                 data = train, family = binomial())

# Predict probabilities
log_prob <- predict(log_model, test, type = "response")

# Convert probabilities to binary classification (using a 0.5 threshold)
log_pred <- ifelse(log_prob > 0.5, TRUE, FALSE)

# Confusion Matrix and Evaluation
library(caret)
conf_matrix_log <- confusionMatrix(as.factor(log_pred), test$blitz)
print(conf_matrix_log)
summary(log_model)

library(ggplot2)
players <- data.frame(
  Player = c(
    week5[week5$gameId == 2022100907 & week5$playId == 2806 & week5$frameId == 57, "jerseyNumber"]),
  x = c(
    week5[week5$gameId == 2022100907 & week5$playId == 2806 & week5$frameId == 57,  "x"]-10),
  y = c(
    week5[week5$gameId == 2022100907 & week5$playId == 2806 & week5$frameId == 57,  "y"]),
  Team = c(
    week5[week5$gameId == 2022100907 & week5$playId == 2806 & week5$frameId == 57,  "club"])
)
field_width <- 53.3
hash_distance <- 22  
hash_spacing <- seq(55, 85, by = 1) 
hash_marks <- data.frame(
  x = rep(hash_spacing, 2),
  y_start = c(rep(hash_distance, length(hash_spacing)), rep(field_width - hash_distance, length(hash_spacing))),
  y_end = c(rep(hash_distance + 1, length(hash_spacing)), rep(field_width - hash_distance - 1, length(hash_spacing)))
)
# Create the football field plot with an endzone and yard lines
ggplot() +
  # Draw the main field area (65 to 100 yards)
  geom_rect(aes(xmin = 55, xmax = 85, ymin = 0, ymax = field_width),
            fill = "palegreen1", color = "white") +

  geom_vline(xintercept = seq(55, 85, by = 5), linetype = "dotted", color = "white") +
  geom_vline(xintercept = 100, linetype = "solid", color = "white") + # Endzone line
   geom_segment(aes(x = 74.58, xend = 74.58, y = 0, yend = 53.3), 
               linetype = "solid", color = "red", linewidth = 0.8) +
  geom_segment(data = hash_marks, 
               aes(x = x, xend = x, y = y_start, yend = y_end), 
               color = "white", linewidth = 0.8) +
    geom_text(aes(x = 60, y = 2, label = "40"), size = 6, color = "black") +
  geom_text(aes(x = 70, y =  2, label = "30"), size = 6, color = "black") +
  geom_text(aes(x = 80, y =  2, label = "20"), size = 6, color = "black") +

  geom_point(data = players, aes(x = x, y = y, color = Team), size = 5) +
  

  scale_color_manual(values = c("MIA" = "turquoise3", "NYJ" = "gray")) +

  geom_text(data = players, aes(x = x, y = y, label = Player), vjust = 0.5, hjust = 0.5, color = "black") +

  xlim(55, 85) +
  ylim(0, field_width) +
  theme_void() +  
  theme(legend.position = "right")

players2 <- data.frame(
  Player = c(
    week1[week1$gameId == 2022091200 & week1$playId == 286 & week1$frameId == 134, "jerseyNumber"]),
  x = c(
    week1[week1$gameId == 2022091200 & week1$playId == 286 & week1$frameId == 134,  "x"]-10),
  y = c(
    week1[week1$gameId == 2022091200 & week1$playId == 286 & week1$frameId == 134,  "y"]),
  Team = c(
    week1[week1$gameId == 2022091200 & week1$playId == 286 & week1$frameId == 134,  "club"])
)
field_width <- 53.3
hash_distance <- 22 
hash_spacing <- seq(65, 100, by = 1)  
hash_marks <- data.frame(
  x = rep(hash_spacing, 2),
  y_start = c(rep(hash_distance, length(hash_spacing)), rep(field_width - hash_distance, length(hash_spacing))),
  y_end = c(rep(hash_distance + 1, length(hash_spacing)), rep(field_width - hash_distance - 1, length(hash_spacing)))
)

ggplot() +
  # Draw the main field area (65 to 100 yards)
  geom_rect(aes(xmin = 65, xmax = 100, ymin = 0, ymax = field_width),
            fill = "palegreen1", color = "white") +

  geom_rect(aes(xmin = 100, xmax = 110, ymin = 0, ymax = field_width),
            fill = "darkgray", color = "white") +
  geom_vline(xintercept = seq(65, 100, by = 5), linetype = "dotted", color = "white") +
  geom_vline(xintercept = 100, linetype = "solid", color = "white") + # Endzone line
   geom_segment(aes(x = 89.16, xend = 89.16, y = 0, yend = 53.3), 
               linetype = "solid", color = "red", linewidth = 0.8) +
  geom_segment(data = hash_marks, 
               aes(x = x, xend = x, y = y_start, yend = y_end), color = "white") +
    geom_text(aes(x = 70, y = 2, label = "30"), size = 6, color = "black") +
  geom_text(aes(x = 80, y =  2, label = "20"), size = 6, color = "black") +
  geom_text(aes(x = 90, y =  2, label = "10"), size = 6, color = "black") +
  geom_text(aes(x = 100, y = 2, label = "G"), size = 6, color = "black") +


  geom_point(data = players2, aes(x = x, y = y, color = Team), size = 5) +
  

  scale_color_manual(values = c("SEA" = "lightblue3", "DEN" = "orange")) +
  
  geom_text(data = players2, aes(x = x, y = y, label = Player), vjust = 0.5, hjust = 0.5, color = "black") +
  
  
  xlim(65, 110) +
  ylim(0, field_width) +
  theme_void() +  
  theme(legend.position = "right")
