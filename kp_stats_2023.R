library(ggplot2)
library(stringr)
library(reshape2)
library(googlesheets4)

###
#Импорт и преобразования
###

#Считываем файлы
#gs4_auth()
table_url <- 'https://docs.google.com/spreadsheets/d/1jfi1wYi7Ojlx6vsCUEfvrUOx3UvOzB5Lm4ljqGK27Vs/edit#gid=0'
players <- read_sheet(table_url, sheet = 'players')
games <- read_sheet(table_url, sheet = 'games')
rm(table_url)

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

###
#Расчеты
###

#средний рейтинг команды среди игр каждого игрока
for (player in players$id) {
  players$games_number[player] <- length(games$date[unlist(lapply(games$team, function(x) player %in% x))])
  players$mean_rating[player] <- mean(games$rating[unlist(lapply(games$team, function(x) player %in% x))])
  players$mean_points[player] <- mean(games$points[unlist(lapply(games$team, function(x) player %in% x))])
}
rm(player)

players$rating_cat <- cut(players$mean_rating, breaks = c(0,5,6,7,8,9,10,Inf), labels = c('F','D','C','B','A','S','S+'))
players$name <- factor(players$name, levels = players$name[order(players$mean_rating)])
veterans <- players[players$games_number >= 10 , ]

#создание палитры цветов для рейтинга
color_palette <- colorRampPalette(c('#FF5030', '#FFFF90', '#008030', '#1188FF'))
colors <- color_palette(7)
named_colors <- setNames(colors, levels(players$rating_cat))

ggplot(players, aes(x = name, y = mean_rating, fill = rating_cat))+
  geom_bar(stat = "identity", width = 0.5)+
  scale_fill_manual(values = colors)+
  coord_flip()

#Игры, где играли игроки с players_id
players_id <- c(7)
players_in_team <- unlist(lapply(games$team, function(x) all(players_id %in% x)))
##ТЕСТ
unlist(lapply(games$team, function(x) all(players_id %in% x)))
mean(games$rating[players_in_team])
games$date[players_in_team]

#Тенденция игр
ggplot(games, aes(x = date, y = rating))+
  geom_line()+
  geom_smooth()+
  geom_point(size = 3, aes(col = players_in_team, shape = type))+
  theme(legend.position = 'bottom')

##Расчет лучших пар
m_size <- length(veterans$id)
rating_mtx <- matrix(NA, nrow = m_size, ncol = m_size, dimnames = list(veterans$id, veterans$id))

for (i in unlist(dimnames(rating_mtx)[1])) {
  for (j in unlist(dimnames(rating_mtx)[2])) {
    if (i != j) {
      played_games <- unlist(lapply(games$team, function(x) all(c(i, j) %in% x)))
      if (any(played_games) > 0) {
        pair_rating <- mean(games$rating[played_games])
        rating_mtx[i,j] <- pair_rating
      }
    }
  }
}
rm(i,j, m_size)

id_to_name <- setNames(players$name, players$id)
rownames(rating_mtx) <- id_to_name[rownames(rating_mtx)]
colnames(rating_mtx) <- id_to_name[colnames(rating_mtx)]
rating_mtx <- rating_mtx[order(rownames(rating_mtx)), order(colnames(rating_mtx))]

rating_df <- melt(rating_mtx)
colnames(rating_df) <- c('player1', 'player2', 'rating')

ggplot(rating_df, aes(x = player1, y = player2, fill = rating))+
  geom_raster() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + # Поворот текста на оси X для лучшей читаемости
  labs(fill = "Рейтинг", x = "Игрок 1", y = "Игрок 2") + # Подписи
  scale_fill_gradient(low = "#FF5030", high = "#98FB98")

rating_mtx[lower.tri(rating_mtx)] <- NA #для удобства табличного представления
rating_df <- melt(rating_mtx)
rm(rating_mtx)

#Прогноз на игру
team <- c(1, 2, 3, 4, 5, 6, 7, 8, 11)
team_mean_rating <- mean(players$mean_rating[players$id %in% team])
team_mean_points <- mean(players$mean_points[players$id %in% team])
mean(games$points_max) / mean(games$difficulty)
