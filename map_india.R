library(raster)
library(rgdal)
library(rgeos)
library(ggplot2)
library(dplyr)
library(animation)
### Q1: Assam only

### Get data
india <- getData("GADM", country = "India", level = 1)

### Choose Assam
assam <- subset(india, NAME_1 == "Assam")



map <- fortify(india)
map$id <- as.integer(map$id)



dat <- data.frame(id = 1:(length(india@data$NAME_1)), state = india@data$NAME_1)

map_df <- inner_join(map, dat, by = "id")

centers <- data.frame(gCentroid(india, byid = TRUE))
centers$state <- dat$state

### This is hrbrmstr's own function
theme_map <- function (base_size = 12, base_family = "") {
  theme_gray(base_size = base_size, base_family = base_family) %+replace% 
    theme(
      axis.line=element_blank(),
      axis.text.x=element_blank(),
      axis.text.y=element_blank(),
      axis.ticks=element_blank(),
      axis.ticks.length=unit(0.3, "lines"),
      axis.ticks.margin=unit(0.5, "lines"),
      axis.title.x=element_blank(),
      axis.title.y=element_blank(),
      legend.background=element_rect(fill="white", colour=NA),
      legend.key=element_rect(colour="white"),
      legend.key.size=unit(1.5, "lines"),
      legend.position="right",
      legend.text=element_text(size=rel(1.2)),
      legend.title=element_text(size=rel(1.4), face="bold", hjust=0),
      panel.background=element_blank(),
      panel.border=element_blank(),
      panel.grid.major=element_blank(),
      panel.grid.minor=element_blank(),
      panel.margin=unit(0, "lines"),
      plot.background=element_blank(),
      plot.margin=unit(c(1, 1, 0.5, 0.5), "lines"),
      plot.title=element_text(size=rel(1.8), face="bold", hjust=0.5),
      strip.background=element_rect(fill="grey90", colour="grey50"),
      strip.text.x=element_text(size=rel(0.8)),
      strip.text.y=element_text(size=rel(0.8), angle=-90) 
    )   
}

#TO ADD STATE NAMES 
# geom_text(data = centers, aes(label = state, x = x, y = y), size = 2) +

#EMPTY INDIA MAP

#ggplot() +
#  geom_map(data = map_df, map = map_df,
#           aes(map_id = id, x = long, y = lat, group = group),
#           color = "#ffffff", fill = "#bbbbbb", size = 0.25) +
#  coord_map() +
#  labs(x = "", y = "", title = "India States") +
#  theme_map()
#  theme(legend.position = "none")

accidents <- read.csv(file="C:/Users/DELL/Desktop/DataProjectR/initialaccident.csv")

map_df <- left_join(map_df,accidents,by=c("state" = "States"))

map_df2 <- transform(map_df)

library("magick")

oopt<-animation::ani.options(interval=0.01)

t <- c("0000-0300h","0300-0600h","0600-0900h","0900-1200h","1200-1500h","1500-1800h","1800-2100h","2100-2400h")

createmap <- function(j){
  tempcol <- j+8
  temptitle<- t[eval(j)]
  df <- mutate(map_df2,"Count" = map_df2[,eval(tempcol)])
  ggplot() +
    geom_polygon(data = df, map = aes(x = long, y = lat, group = state,fill=Count)
                 , color = "black",size = 0.25) +
    coord_map() +
    labs(x = "", y = "", title = temptitle) +
    theme_map()
}
FUN2 <- function() {
  v = c(1,2,3,4,5,6,7,8)
  lapply(v, function(i) {
    print(createmap(i))
    #ani.pause()
  })
}



#f <-FUN2()
saveHTML(FUN2(), autoplay = FALSE, loop = FALSE, verbose = FALSE,htmlfile="index.html",
         single.opts = "'controls': ['first', 'previous', 'play', 'next', 'last', 'loop', 'speed'], 'delayMin': 0")
