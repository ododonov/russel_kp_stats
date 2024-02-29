players <- read.csv('kp_players.csv', header = T, sep = ',')
games <- read.csv('kp_stats.csv', header = T, sep = ',')

games$game_type <- as.factor(games$game_type)

games$points[games$points == 'NA']
games$points <- as.numeric(games$points)

player_id <- '8'
hist(
  games$points[grepl(player_id, games$team) & games$game_type != 'final'], 
  main = players$name[players$id == player_id]
  )
?hist
