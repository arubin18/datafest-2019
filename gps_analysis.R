library(dplyr)
library(ggplot2)
library(animation)

#gps.data <- read.csv('gps.csv')

player2.data <- gps.data %>% filter(GameID == 1, PlayerID == 2)
#top.speed <- max(player2.data$Speed)

top.speed <- function (game.id, player.id) {
  player.game.data <- gps.data %>% select(GameID, PlayerID, Speed) %>%
    filter(GameID == game.id, PlayerID == player.id)
  return(max(player.game.data$Speed))
}

average.speed <- function (game.id, player.id) {
  player.game.data <- gps.data %>% select(GameID, PlayerID, Speed) %>%
    filter(GameID == game.id, PlayerID == player.id)
  return(mean(player.game.data$Speed))
}

distance.traveled <- function (game.id, player.id) {
  player.game.data <- gps.data %>% select(GameID, PlayerID, Speed) %>%
    filter(GameID == game.id, PlayerID == player.id)
  return(sum(player.game.data$Speed*0.1))
}

duration.ingame <- function (game.id, player.id) {
  player.game.data <- gps.data %>% select(GameID, PlayerID) %>%
    filter(GameID == game.id, PlayerID == player.id)
  return(nrow(player.game.data)*0.1)
}

average.accel <- function (game.id, player.id) {
  player.game.data <- gps.data %>% select(GameID, PlayerID, AccelImpulse) %>%
    filter(GameID == game.id, PlayerID == player.id)
  return(mean(player.game.data$AccelImpulse))
}

average.load <- function (game.id, player.id) {
  player.game.data <- gps.data %>% select(GameID, PlayerID, AccelLoad) %>%
    filter(GameID == game.id, PlayerID == player.id)
  return(mean(player.game.data$AccelLoad))
}

total.load <- function (game.id, player.id) {
  player.game.data <- gps.data %>% select(GameID, PlayerID, AccelLoad) %>%
    filter(GameID == game.id, PlayerID == player.id)
  return(sum(player.game.data$AccelLoad))
}

high.accel.events <- function (game.id, player.id, threshold=5) {
  player.game.data <- gps.data %>% select(GameID, PlayerID, AccelImpulse) %>%
    filter(GameID == game.id, PlayerID == player.id)
  return(sum(player.game.data$AccelImpulse >= threshold))
}

high.load.events <- function (game.id, player.id, threshold) {
  player.game.data <- gps.data %>% select(GameID, PlayerID, AccelLoad) %>%
    filter(GameID == game.id, PlayerID == player.id)
  return(sum(player.game.data$AccelLoad >= threshold))
}

draw.heatmap <- function (game.id, player.id) {
  player.game.data <- gps.data %>% select(GameID, PlayerID, Longitude, Latitude) %>%
    filter(GameID == game.id, PlayerID == player.id)
  df <- data.frame(player.game.data$Longitude, player.game.data$Latitude)
  
  
  colnames(df) <- c("Long", "Lat")
 
  acceleration.data <- gps.data %>% select(GameID, PlayerID, AccelImpulse) %>%
    filter(GameID == game.id, PlayerID == player.id)

  iter <- function() {
    lapply(seq(1, nrow(df), length.out=10), function(i) {
      print(ggplot(df[1:i,, drop=FALSE], aes(y = Lat, x = Long, color=acceleration.data$AccelImpulse[1:i, drop=FALSE])) + 
              scale_colour_gradient(low = "black", high = "red") +
              geom_point() + #colour = "blue") + 
              xlim(c(min(df$Long), max(df$Long))) + 
              ylim(c(min(df$Lat), max(df$Lat))) + 
              labs(color = "Acceleration"))
              
    
      #animation::ani.pause()
    })
  }
  #FUN()
  
  saveGIF(iter(), interval = 0.1, outdir = getwd(), cmd.fun = shell)
}

player.average <- function (player.id, fun) {
  unique.pairs <- unique(gps.data[c("GameID", "PlayerID")])
  game.ids <- unique.pairs[unique.pairs$PlayerID==1, ]$GameID
  total <- 0
  for (g.id in game.ids) {
    total <- total + fun(g.id, player.id)
  }
  return(total/length(game.ids))
}