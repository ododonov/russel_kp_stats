library(ggplot2)
library(stringr)

#Считываем файлы
players <- read.csv('russel_kp_stats/kp_players.csv', header = T, sep = ';')
games <- read.csv('russel_kp_stats/kp_games.csv', header = T, sep = ';')

#Тип даты преобразуем в дату
games$date <- as.Date(games$date, format = "%d.%m.%y")
#Тип очков преобразуем в число
games$points <- as.numeric(str_replace(games$points, ',', '.'))

#Тип сложности преобразуем в число и пересчитываем
games$difficulty <- 1/as.numeric(str_replace(games$difficulty, ',', '.'))

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

#Игры, где играл игрок с player_id
player_id <- 1
player_in_team <- unlist(lapply(games$team, function(x) player_id %in% x))
ggplot(games, aes(x = date, y = rating))+
  geom_line()+
  geom_point(aes(col = player_in_team, size = type))+
  theme(legend.position = 'bottom')

#Прогноз на игру
team <- c(2)
team_mean_rating <- mean(players$mean_rating[players$id %in% team])


