library(tidyverse)

data <- read.csv("/Users/justinchoi/statcast_2024.csv") 

main_df <- data %>% 
  mutate(
    is_swing = case_when(
      description %in% c('foul','foul_tip','hit_into_play','swinging_strike','swinging_strike_blocked') ~ 1, 
      .default = 0), 
    is_whiff = case_when(
      description %in% c('swinging_strike','foul_tip','swinging_strike_blocked') ~ 1, 
      .default = 0), 
    pfx_x = if_else(p_throws == 'L', -pfx_x, pfx_x), 
    plate_x = if_else(p_throws == 'L', -plate_x, plate_x), 
    count = factor(paste(balls, strikes, sep = "-")) 
  ) %>% 
  group_by(game_date, inning, pitcher, batter) %>% 
  mutate(
    x_prev = lag(plate_x), 
    z_prev = lag(plate_z), 
    x_diff = x_prev - plate_x, 
    z_diff = z_prev - plate_z, 
    velo_diff = lag(release_speed) - release_speed, 
    x_mov_diff = lag(pfx_x) - pfx_x, 
    z_mov_diff = lag(pfx_z) - pfx_z
  ) %>% 
  filter(pitch_type == 'SL', 
         lag(pitch_type) == 'FF') %>% 
  ungroup() %>% 
  select(release_speed, count, plate_x, plate_z, pfx_x, pfx_z, x_prev, z_prev, 
         x_diff, z_diff, velo_diff, x_mov_diff, z_mov_diff, is_swing, is_whiff) 








