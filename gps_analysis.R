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

high.load.events <- function (game.id, player.id, threshold=0.2) {
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

csv.data <- read.csv("data.csv")
avg.accels <- c()
avg.loads <- c()
avg.speeds <- c()
avg.dists <- c()
avg.times <- c()
avg.high.accels <- c()
avg.high.loads <- c()

for (i in 1:nrow(csv.data)) {
  pid <- csv.data$PlayerID[i]
  gameid1 <- csv.data$game1[i]
  gameid2 <- csv.data$game2[i]
  gameid3 <- csv.data$game3[i]
  
  avg.accel <- 0
  avg.load <- 0
  avg.speed <- 0
  avg.dist <- 0
  avg.time <- 0
  avg.high.accel.events <- 0
  avg.high.load.events <- 0
  
  total <- 0
  if (!is.na(gameid1)) {
    avg.accel <- average.accel(gameid1, pid)
    avg.load <- average.load(gameid1, pid)
    avg.speed <- average.speed(gameid1, pid)
    avg.dist <- distance.traveled(gameid1, pid)
    avg.time <- duration.ingame(gameid1, pid)
    avg.high.accel.events <- high.accel.events(gameid1, pid)
    avg.high.load.events <- high.load.events(gameid1, pid)
    total <- 1
  }
  if (!is.na(gameid2)) {
    avg.accel <- avg.accel + average.accel(gameid2, pid)
    avg.load <- avg.load + average.load(gameid2, pid)
    avg.speed <- avg.speed + average.speed(gameid2, pid)
    avg.dist <- avg.dist + distance.traveled(gameid2, pid)
    avg.time <- avg.time + duration.ingame(gameid2, pid)
    avg.high.accel.events <- avg.high.accel.events + high.accel.events(gameid2, pid)
    avg.high.load.events <- avg.high.load.events + high.load.events(gameid2, pid)
    total <- 2
  }
  if (!is.na(gameid3)) {
    avg.accel <- avg.accel + average.accel(gameid3, pid)
    avg.load <- avg.load + average.load(gameid3, pid)
    avg.speed <- avg.speed + average.speed(gameid3, pid)
    avg.dist <- avg.dist + distance.traveled(gameid3, pid)
    avg.time <- avg.time + duration.ingame(gameid3, pid)
    avg.high.accel.events <- avg.high.accel.events + high.accel.events(gameid3, pid)
    avg.high.load.events <- avg.high.load.events + high.load.events(gameid3, pid)
    total <- 3
  }
  avg.accels[i] <- avg.accel/total
  avg.loads[i] <- avg.load/total
  avg.speeds[i] <- avg.speed/total
  avg.dists[i] <- avg.dist/total
  avg.times[i] <- avg.time/total
  avg.high.accels[i] <- avg.high.accel.events/total
  avg.high.loads[i] <- avg.high.load.events/total
}

new.csv.data <- cbind(csv.data, avg.accels, avg.loads, avg.speeds, avg.dists, avg.times,
                      avg.high.accels, avg.high.loads)
write.csv(new.csv.data, "data_w_gps.csv")