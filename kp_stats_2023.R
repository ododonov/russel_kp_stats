library(ggplot2)
library(stringr)

#Считываем файлы
players <- read.csv('russel_kp_stats/kp_players.csv', header = T, sep = ';')
games <- read.csv('russel_kp_stats/kp_games.csv', header = T, sep = ';')

#Тип даты преобразуем в дату
games$date <- as.Date(games$date, format = "%d.%m.%y")
#Тип очков преобразуем в число
games$points <- as.numeric(str_replace(games$points, ',', '.'))

#Рассчитываем сложность каждой игры
games$points_1p <- as.numeric(str_replace(games$points_1p, ',', '.'))
games$points_2p <- as.numeric(str_replace(games$points_2p, ',', '.'))
games$points_3p <- as.numeric(str_replace(games$points_3p, ',', '.'))
games$difficulty <- (3*games$points_max)/(games$points_1p + games$points_2p + games$points_3p)

#Тип игр преобразуем в фактор
games$type <- factor(games$type, labels = c('Классика', 'Финал'))

#Баллы и сложность преобразуем в рейтинг
games$rating <- ifelse(11 - games$place > 0, 11 - games$place, 0)
games$rating <- games$rating * (games$points / games$points_max) * games$difficulty

#Составы команд преобразуем в вектор
games$team <- lapply(strsplit(games$team, ','), function(x) as.integer(x))

#средний рейтинг команды среди игр каждого игрока
for (player in players$id) {
  players$games_number[player] <- length(games$date[unlist(lapply(games$team, function(x) player %in% x))])
  players$mean_rating[player] <- mean(games$rating[unlist(lapply(games$team, function(x) player %in% x))])
  players$mean_points[player] <- mean(games$points[unlist(lapply(games$team, function(x) player %in% x))])
}

veterans <- players[players$games_number >= 10 , ]

#Игры, где играли игроки с players_id
players_id <- c(7,8)
players_in_team <- unlist(lapply(games$team, function(x) all(players_id %in% x)))

#Тенденция игр
ggplot(games, aes(x = date, y = rating))+
  geom_line()+
  geom_smooth()+
  geom_point(size = 3, aes(col = players_in_team, shape = type))+
  theme(legend.position = 'bottom')

#Прогноз на игру
team <- c(1, 3, 6, 7, 8, 11, 21, 26)
team_mean_rating <- mean(players$mean_rating[players$id %in% team])
team_mean_points <- mean(players$mean_points[players$id %in% team])
mean(games$points_max) / mean(games$difficulty)

