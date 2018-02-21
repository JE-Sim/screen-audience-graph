library(plyr)
rawlist <- function(x){
  setwd("C:/Users/정은/Desktop/API/csv")
  fileList <- dir()[grep("_dailyBoxOffice",dir())]
  rawList <- list()
  for(i in 1:length(fileList)){
    temp <- read.csv(fileList[i], header = T)
    temp <- temp[,-which(colnames(temp) == 'rnum')]
    a <- sort(colnames(temp)[-c(1, 4, 7, 8, 9, 13, 12)])
    col.index <- vector(length = length(a))
    for (j in 1:length(a)){
      col.index[j] <- which(colnames(temp) == a[j])
    }
    temp1 <- temp[, c(7:9, 1, 4, 13:12, col.index)]
    temp2 <- ddply(temp1, .(movieCd))
    temp2$date <- as.factor(temp2$date)
    rawList[[i]] <- temp2
  }
  return(rawList)
}
raw.name <- function(x){
  rawList <- rawlist(x)
  raw.byName <- rawList[[1]]
  for(i in 2:length(rawList)){
    raw.byName <- rbind(raw.byName, rawList[[i]])
  }
  return(raw.byName)
}
name.sorting <- function(x){
  raw.byName <- raw.name(x)
  name <- unique(raw.byName['movieNm'])
  name.1 <- as.character(unlist(name))
  
  nameList <- list()
  for(i in 1:length(name.1)){
    nameList[[i]] <- raw.byName[which(raw.byName$movieNm == name.1[i]),]
  }
  names(nameList) <- name.1
  return(nameList)
}
raw.byName <- raw.name(x)
nameList <- name.sorting(x)
graph <- function(Date, audience, screen, main, range.a, range.s, check, k, k1){
  plot(Date, audience, pch = 22, las = 2, axes = F, type = "o", xlab = "", ylab="",
       main = main, col = "blue", ylim = range.a)
  box()
  if(check | (!check & k == 1)) {
    axis(2, col = "blue", col.axis = "blue", las = 2, )
    mtext("Audience Count (in 1k)", side = 2, col = "blue", line = 3)
    legend("topright", inset = 0.01, bg = terrain.colors(1, alpha = 0.5), box.lty = 0,
           bty != "n", legend = c("audience", "screen"), col = c("blue", "red"), 
           pch = c(22, 21))
  }
  a <- ifelse(length(Date) > 55, 0.7, 0.8)
  axis(1, Date, format(Date, "%y . %m . %d"), cex.axis = a, las = 2, col = "black")
  par(new = T)
  plot(Date, screen, pch = 21, las = 2, axes = F, ylab = "", xlab="",
       type = "o", col = "red", lty = 2, ylim = range.s)
  if(check | (!check & k == k1)){
    axis(4, las = 2, col = "red", col.axis = "red", )
    mtext("Screen Count", side = 4, col = "red", line = 3)
  }
  if(check | (!check & k == 1)) {mtext("date", 1, line = 4.7)}
}
plot.1 <- function(i){
  Date <- as.Date(nameList[[i]]$date, format ="%Y%m%d")
  screen <- as.numeric(nameList[[i]]$scrnCnt)
  temp <- as.numeric(nameList[[i]]$audiCnt)
  audience <- temp/1000
  main <- paste(names(nameList)[i], paste(length(Date), "days"), sep = " / ")
  range.a <- range(audience); range.s <- range(screen)
  par(mar = c(6.5, 5, 3, 5)+0.1)
  par(mfrow = c(1, 1))
  graph(Date, audience, screen, main, range.a, range.s, T, 0, 0)
}
plot.2 <- function(i, term){
  Date <- as.Date(nameList[[i]]$date, format ="%Y%m%d")
  screen <- as.numeric(nameList[[i]]$scrnCnt)
  temp <- as.numeric(nameList[[i]]$audiCnt)
  audience <- temp/1000
  if(length(which(diff(Date) > term)) > 0){
    check <- F
    key <- c(which(diff(Date) > term),length(Date))
    date <- list()
    last <- 0
    for(j in 1:length(key)){
      start <- last + 1
      last <- key[j]
      date[[j]] <- start:last
    }
  }
  k1 <- length(date)
  if(!check) {
    if(length(date) == 2) {
      if(length(date[[1]]) > length(date[[2]])) {
        layout(matrix(c(1, 2), 1, 2, byrow = T), width = c(1.3, 0.7))
      } else layout(matrix(c(1, 2), 1, 2, byrow = T), width = c(0.7, 1.3))
    } else {
      c <- sapply(date, length) / sum(sapply(date, length))
      index <- which(c/max(c) > 0.45)
      if(max(c) > 0.5){
        c.1 <- vector(length = length(c))
        c.1[index] <- 0.4; c.1[which(c == max(c))] <- 0.5; c.1[-index] <- 0.2
      } else {c.1 <- c}
      layout(matrix(1:length(date), 1, length(date), byrow = T), width = c.1)
    }
    for(k in 1:k1) {
      date.1 <- Date[date[[k]]]
      audience.1 <- audience[date[[k]]]
      screen.1 <- screen[date[[k]]]
      main <- paste(names(nameList)[i], paste(length(date.1), "d"), sep = " / ")
      range.a <- range(audience); range.s <- range(screen)
      if(length(date) == 2) {
        if(k == 1) par(mar = c(6.5, 5, 3, 0)) else par(mar = c(6.5, 0.3, 3, 5))
      } else {
        if(k == 1) par(mar = c(6.5, 5, 3, 0)) else {
          if(k == length(date)) par(mar = c(6.5, 0, 3, 5)) else par(mar = c(6.5, 0.3, 3, 0.3))
        }
      }
      graph(date.1, audience.1, screen.1, main, range.a, range.s, check, k, k1)
    }
  }
}
plot.3 <- function(i, term){
  Date <- as.Date(nameList[[i]]$date, format ="%Y%m%d")
  screen <- as.numeric(nameList[[i]]$scrnCnt)
  temp <- as.numeric(nameList[[i]]$audiCnt)
  audience <- temp/1000
  if(length(which(diff(Date) > term)) > 0){
    check <- F
    key <- c(which(diff(Date) > term),length(Date))
    date <- list()
    last <- 0
    for(j in 1:length(key)){
      start <- last + 1
      last <- key[j]
      date[[j]] <- start:last
    }
  }
  k1 <- as.numeric(length(date))
  d <- sapply(date, length) / sum(sapply(date, length))
  d[which(d < 0.05)] <- 0.05
  d[k1] <- d[k1] + 0.105
  if(!check) {
    layout(matrix(1:k1, 1, k1, byrow = T), width = d)
  }
  for(k in 1:k1) {
    date.1 <- Date[date[[k]]]
    audience.1 <- audience[date[[k]]]
    screen.1 <- screen[date[[k]]]
    if(k == 1){
      main <- paste(names(nameList)[i], paste(length(date.1), "days"), sep = " / ")
    } else main <- paste(length(date.1), "d")
    range.a <- range(audience); range.s <- range(screen)
    if(length(date) == 2) {
      if(k == 1) par(mar = c(6.5, 5, 3, 0)) else par(mar = c(6.5, 0.3, 3, 5))
    } else {
      if(k == 1) par(mar = c(6.5, 5, 3, 0)) else {
        if(k == length(date)) par(mar = c(6.5, 0, 3, 5)) else par(mar = c(6.5, 0.3, 3, 0.3))
      }
    }
    graph(date.1, audience.1, screen.1, main, range.a, range.s, check, k, k1)
  }
}


name <- paste(1:length(nameList), names(nameList), sep=" : ")
name.sort <- paste(order(names(nameList)), names(nameList)[order(names(nameList))], sep = " : ")
setwd("C:/Users/정은/Desktop/API/graphs")
name.0 <- paste(1:length(nameList), "png", sep=".")

write(name, "movie_Names_number.txt", sep = "\n")
write(name.sort, "movie_Names_sorting.txt", sep ="\n")

#기본
for(i in 1:length(nameList)){
  name.1 <- name.0[i]
  windows(800, 600)
  plot.1(i)
  savePlot(name.1, type = "png")
  graphics.off()
}
#재개봉 new
for(i in 1:length(nameList)){
  term <- 21
  Date <- as.Date(nameList[[i]]$date, format ="%Y%m%d")
  if(length(which(diff(Date) > term)) > 0){
    name.b <- paste(paste(1:length(nameList),"1", sep = "_"), "png", sep=".")
    name.1 <- name.b[i]
    windows(800, 600)
    plot.3(i, term)
    savePlot(name.1, type = "png")
    graphics.off()
  }
}

plot.1(107)
plot.2(107, 21)
plot.3(107, 21)

#재개봉 old
for(i in 1:length(nameList)){
  term <- 21
  Date <- as.Date(nameList[[i]]$date, format ="%Y%m%d")
  if(length(which(diff(Date) > term)) > 0){
    name.b <- paste(paste(1:length(nameList),"1", sep = "_"), "png", sep=".")
    name.1 <- name.b[i]
    windows(800, 600)
    plot.2(i, term)
    savePlot(name.1, type = "png")
    graphics.off()
  }
}







###########png()로 저장#########
for(i in 1:length(nameList)){
  name.1 <- name[i]
  png(filename = name.1, type = "cairo-png", width = 800, height = 600)
  ploting(i)
  dev.off()
}
checking <- function(x){
  lostGraph <- dir()[-grep(".png",dir())]
  a <- strsplit(lostGraph, "_")
  rpyNm <- sapply(a, "[", 1)
  b <- strsplit(dir(),"_")
  c <- 1:length(name)
  lostNm <- c[-which(c %in% sort(as.numeric(sapply(b, "[", 1))))]
  replay <- sort(as.integer(c(rpyNm, lostNm)))
  return(replay)
}
replay <- checking(x)
for(i in length(replay)){
  a <- replay[i]
  windows(800, 600)
  ploting(a)
  savePlot(name[a], type = "png")
  graphics.off()
}
########outlier : 너의 이름은.###########
i <- 49
Date <- as.Date(nameList[[i]]$date, format ="%Y%m%d")
nameList[[i]]$date[c(1,length(nameList[[i]]$date))]
range(Date)[2]-range(Date)[1]
diff(Date)
Date[which(diff(Date) > 90)]


if(length(which(diff(Date) > 90)) > 0){
  key <- c(which(diff(Date) > 90),length(Date))
  d.1 <- list()
  last <- 0
  for(j in 1:length(key)){
    start <- last + 1
    last <- key[j]
    d.1[[j]] <- start:last
  }
}


i <- 78
Date <- as.Date(nameList[[i]]$date, format ="%Y%m%d")
nameList[[i]]$date[c(1,length(nameList[[i]]$date))]
range(Date)[2]-range(Date)[1]
diff(Date)
Date[which(diff(Date) > 90)]

i <- 107
Date <- as.Date(nameList[[i]]$date, format ="%Y%m%d")
nameList[[i]]$date[c(1,length(nameList[[i]]$date))]
range(Date)[2]-range(Date)[1]
diff(Date)
Date[which(diff(Date) > 90)]
###################################################################
raw <- read.csv("dailyBoxOffice.csv", header = T)
raw <- raw[,-which(colnames(raw) == 'rnum')]

a <- sort(colnames(raw)[-c(4, 1, 6, 3, 11, 12)])
col.index <- vector(length = length(a))
for(i in 1:length(a)){
  col.index[i] <- which(colnames(raw) == a[i])
}
raw.1 <- raw[,c(4, 1, 6, 3, 11:12, col.index)]


name <- unique(raw.1['movieNm'])
name.1 <- as.character(unlist(name))
raw.byName <- ddply(raw.1, .(movieNm))
raw.byName$date <- as.factor(raw.byName$date)
#####################################################################
'''
i <- 1L
colnames(nameList[[i]])
x <- as.Date(nameList[[i]]$date, format ="%Y%m%d")
s.d <- as.character(x[1])
e.d <- as.character(x[length(x)])
audiCnt.ts <- ts(nameList[[i]]$audiCnt/1000)
scrnCnt.ts <- ts(nameList[[i]]$scrnCnt)

par(mfrow = c(2, 1))
plot(audiCnt.ts, type = "o", xlab = "date", ylab = "audience Count (in 1,000)", 
     main = names(nameList)[i], las = 2)
plot(scrnCnt.ts, type = "o", xlab = "date", ylab = "screen Count", 
     main = names(nameList)[i], las = 2)

class(nameList[[i]]$audiCnt/1000)
class(nameList[[i]]$date)
'''



ploting <- function(i){
  screen <- as.numeric(nameList[[i]]$scrnCnt)
  temp <- as.numeric(nameList[[i]]$audiCnt)
  audience <- temp/1000 
  Date <- as.Date(nameList[[i]]$date, format ="%Y%m%d")
  par(mar = c(6.5, 5, 3, 5)+0.1)
  par(mfrow = c(1, 1))
  main <- paste(names(nameList)[i], paste(length(Date), "days"), sep = " / ")
  plot(Date, audience, pch = 22, las = 2, axes = F, type = "o", xlab = "", ylab="",
       main = main, col = "blue")
  box()
  axis(2, col = "blue", col.axis = "blue", las = 2, )
  mtext("Audience Count (in 1k)", side = 2, col = "blue", line = 3)
  a <- ifelse(length(Date) > 55, 0.7, 0.8)
  axis(1, Date, format(Date, "%y . %m . %d"), cex.axis = a, las = 2, col = "black")
  par(new = T)
  plot(Date, screen, pch = 21, las = 2, axes = F, ylab = "", xlab="",
       type = "o", col = "red", lty = 2)
  axis(4, las = 2, col = "red", col.axis = "red", )
  mtext("Screen Count", side = 4, col = "red", line = 3)
  mtext("date", 1, line = 4.7)
  legend("topright", bty = "n", legend = c("audience", "screen"), col = c("blue", "red"),
         pch = c(22, 21))
}