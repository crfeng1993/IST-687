load("R-Project.RData")
Sati_flight_1 <- na.omit(Sati_flight)
install.packages("ggmap")
install.packages("geosphere")
install.packages("jpeg")
install.packages("grid")
install.packages("plyr")
library(ggmap)
library(geosphere)
library(jpeg)
library(grid)
library(plyr)

library(pacman)
p_load(tidyverse, data.table, geosphere, grid, jpeg, plyr,dbplyr)

#Download the map
download.file("https://www.nasa.gov/specials/blackmarble/2016/globalmaps/BlackMarble_2016_01deg.jpg",destfile = "BlackMarble_2016_01deg.jpg", mode = "wb")

#render the map

earth <- readJPEG("BlackMarble_2016_01deg.jpg", native = TRUE)
earth <- rasterGrob(earth, interpolate = TRUE)


# Get the point of airport
Allcity <- factor(c(Sati_flight_1$Orgin.City,Sati_flight_1$Destination.City))
Allevel <- levels(Allcity)

latlon_Org <- geocode(source = "dsk",Allevel)
View(latlon_Org)

latlon_Org <- data.frame("City Name"=Allevel,"Lat"=latlon_Org[1],"Long"=latlon_Org[2])

match_Org <- match(Sati_flight_1$Orgin.City,latlon_Org$City.Name)
match_Des <- match(Sati_flight_1$Destination.City,latlon_Org$City.Name)

Flightpath <- data.frame(Sati_flight_1$Satisfaction,Sati_flight_1$Airline.Name,
                         Sati_flight_1$Orgin.City,"Orgin_lat"=latlon_Org$lat[match_Org],"Orgin_lon"=latlon_Org$lon[match_Org],
                         Sati_flight_1$Destination.City,"Dest_lat"=latlon_Org$lat[match_Des],"Dest_lon"=latlon_Org$lon[match_Des])

Flightpath_all <- as.matrix(data.frame(Flightpath$Orgin.lat,Flightpath$Orgin.lon,Flightpath$Dest.lat,Flightpath$Dest.lon))
Flightpath_all <- t(Flightpath_all)
Flightpath_all <- matrix(as.vector(Flightpath_all),ncol = 2,byrow = T)
group <- as.vector(t(as.matrix(data.frame(1:length(Flightpath$Dest.lat),1:length(Flightpath$Dest.lat)))))

Flightpath_all <- cbind(group,data.frame(Flightpath_all))
colnames(Flightpath_all) <- c("long","lat","id")
str(Flightpath_all)


flightpath_split <- split(Flightpath, Flightpath$Sati_flight_1.Airline.Name)

flights_all <- lapply(flightpath_split, function(x) gcIntermediate(x[, c("Orgin_lon", "Orgin_lat")], x[, c("Dest_lon", "Dest_lat")], n=100, breakAtDateLine = FALSE, addStartEnd = TRUE, sp = TRUE))


# convert it into dataframe

flightpath_fortified <- lapply(flights_all, function(x) ldply(x@lines, fortify))


# Unsplit lists

flightpath_fortified <- do.call("rbind", flightpath_fortified)



# Add and clean column with airline names

flightpath_fortified$name <- rownames(flightpath_fortified)
flightpath_fortified$name <- gsub("\\..*", "", flightpath_fortified$name)

levels(factor(flightpath_fortified$name))

# Extract first and last observations for plotting source and destination points (i.e., airports)

flightpath_points <- flightpath_fortified %>%
  
  group_by(group) %>%
  
  filter(row_number() == 1 | row_number() == n())
#Calculate intermediate points between each two locations


Fmap <- ggplot() +
  
  annotation_custom(earth, xmin = -180, xmax = 180, ymin = -90, ymax = 90) +
  
  geom_path(aes(long, lat, group = id, color = name), alpha = 0.0, size = 0.0, data = flightpath_fortified) +
  
  geom_path(aes(long, lat, group = id), alpha = 0.2, size = 0.3, color = "#f9ba00", data = flightpath_fortified[flightpath_fortified$name=="West Airways Inc",]) +
  
  geom_path(aes(long, lat, group = id), alpha = 0.2, size = 0.3, color = "#ff0000", data = flightpath_fortified[flightpath_fortified$name == "Southeast Airlines Co", ]) +
  
  geom_path(aes(long, lat, group = id), alpha = 0.2, size = 0.3, color = "#075aaa", data = flightpath_fortified[flightpath_fortified$name == "Northwest Business Airlines Inc", ]) +
  
  geom_point(data = flightpath_points, aes(long, lat), alpha = 0.8, size = 0.1, colour = "white") +
  
  theme(panel.background = element_rect(fill = "#05050f", colour = "#05050f"),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),axis.title = element_blank(),axis.text = element_blank(),axis.ticks.length = unit(0, "cm"),legend.position = "none") +
  
  annotate("text", x = -150, y = 13, hjust = 0, size = 14,label = paste("Northwest Business Airlines Inc"), color = "#075aaa") +
  
  annotate("text", x = -150, y = 10, hjust = 0, size = 14,label = paste("West Airways Inc"), color = "#f9ba00") +
  
  annotate("text", x = -150, y = 7, hjust = 0, size = 14,label = paste("Southeast Airlines Co"), color = "#ff0000") +
  
  annotate("text", x = -150, y = 4, hjust = 0, size = 8,
  
           label = paste("Flight routes"), color = "white") +
  
  annotate("text", x = -150, y = 2, hjust = 0, size = 7,
  
           label = paste("Ruifeng Chen || NASA.gov || IST687 Final Project"), color = "white", alpha = 0.5) +

  coord_equal()


# Divide the flight by Satisfaction 


Flightpath$Sati_flight_1.Satisfaction <- ceiling(Flightpath$Sati_flight_1.Satisfaction)
hist(Flightpath$Sati_flight_1.Satisfaction)
index <- sample(c(TRUE,FALSE),length(Flightpath$Sati_flight_1.Satisfaction),replace=T,prob=c(0.2,0.8))
Flightpath_sample <- Flightpath[index,]

Sampleit <- function(df,n){
  a <- n/nrow(df)
  index01 <- sample(c(T,F),nrow(df),replace = T, prob = c(a,1-a))
  df1 <- df[index01,]
  return(df1)
}

num <- nrow(Flightpath_sample[Flightpath_sample$Sati_flight_1.Satisfaction=="1",])
Flightpath1_sample <- rbind(Flightpath_sample[Flightpath_sample$Sati_flight_1.Satisfaction=="1",],
                            Sampleit(Flightpath_sample[Flightpath_sample$Sati_flight_1.Satisfaction=="2",],num),
                            Sampleit(Flightpath_sample[Flightpath_sample$Sati_flight_1.Satisfaction=="3",],num),
                            Sampleit(Flightpath_sample[Flightpath_sample$Sati_flight_1.Satisfaction=="4",],num),
                            Sampleit(Flightpath_sample[Flightpath_sample$Sati_flight_1.Satisfaction=="5",],num) )

flightpath1_split <- split(Flightpath1_sample, Flightpath1_sample$Sati_flight_1.Satisfaction)
flights1_all <- lapply(flightpath1_split, function(x) gcIntermediate(x[, c("Orgin_lon", "Orgin_lat")], x[, c("Dest_lon", "Dest_lat")], n=50, breakAtDateLine = FALSE, addStartEnd = TRUE, sp = TRUE))

flightpath1_fortified <- lapply(flights1_all, function(x) ldply(x@lines, fortify))
flightpath1_fortified <- do.call("rbind", flightpath1_fortified)

flightpath1_fortified$satisfaction <- rownames(flightpath1_fortified)
flightpath1_fortified$satisfaction <- gsub("\\..*", "", flightpath1_fortified$satisfaction)

flightpath1_points <- flightpath1_fortified %>%
  group_by(group) %>%
  filter(row_number() == 1 | row_number() == n())


ggplot() +
  annotation_custom(earth, xmin = -180, xmax = 180, ymin = -90, ymax = 90) +
  geom_path(aes(long, lat, group = id, color = satisfaction), alpha = 0, size = 0, data = flightpath1_fortified) +
  #geom_path(aes(long, lat, group = id), alpha = 0.2, size = 0.3, color = "#f9ba00", data = flightpath1_fortified[flightpath1_fortified$satisfaction=="1",]) +
  #geom_path(aes(long, lat, group = id), alpha = 0.2, size = 0.3, color = "#ff0000", data = flightpath1_fortified[flightpath1_fortified$satisfaction=="2",]) +
  #geom_path(aes(long, lat, group = id), alpha = 0.2, size = 0.3, color = "#00bfff", data = flightpath1_fortified[flightpath1_fortified$satisfaction=="3",]) +
  geom_path(aes(long, lat, group = id), alpha = 0.2, size = 0.3, color = "#2bde73", data = flightpath1_fortified[flightpath1_fortified$satisfaction=="4",]) +
  #geom_path(aes(long, lat, group = id), alpha = 0.2, size = 0.3, color = "#dedcee", data = flightpath1_fortified[flightpath1_fortified$satisfaction=="5",]) +
  geom_point(data = flightpath1_points, aes(long, lat), alpha = 0.8, size = 0.1, colour = "white") +
  theme(panel.background = element_rect(fill = "#05050f", colour = "#05050f"),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),axis.title = element_blank(),axis.text = element_blank(),axis.ticks.length = unit(0, "cm"),legend.position = "none") +
  #annotate("text", x = -150, y = 13, hjust = 0, size = 14,label = paste("Northwest Business Airlines Inc"), color = "#075aaa") +
  #annotate("text", x = -150, y = 10, hjust = 0, size = 14,label = paste("West Airways Inc"), color = "#f9ba00") +
  annotate("text", x = -150, y = 7, hjust = 0, size = 14,label = paste("Satisfaction=4"), color = "#2bde73") +
  annotate("text", x = -150, y = 4, hjust = 0, size = 8,
           label = paste("Flight routes vs Satisfaction"), color = "white") +
  annotate("text", x = -150, y = 2, hjust = 0, size = 7,
           label = paste("Ruifeng Chen || NASA.gov || IST687 Final Project"), color = "white", alpha = 0.5) +
  coord_equal()


#Airpot vs satisfaction
Flightairport_group <- group_by(Flightpath,Sati_flight_1.Orgin.City)
airports <- summarize(Flightairport_group,count=length())

View(airports)

Airport_Origin <- data.frame(tapply(Flightpath$Sati_flight_1.Satisfaction,Flightpath$Sati_flight_1.Orgin.City,mean))
colnames(Airport_Origin) <- "SatisAsOri"
Airport_Origin$Origin_city <- rownames(Airport_Origin)
#View(Airport_Origin)

Airport_Dest <- data.frame(tapply(Flightpath$Sati_flight_1.Satisfaction,Flightpath$Sati_flight_1.Destination.City,mean))
colnames(Airport_Dest) <- "SatisAsDest"
Airport_Dest$Dest_City <- rownames(Airport_Dest)
#View(Airport_Dest)
Airport_Dest <- Airport_Dest[-211,]


Airport_Popularity02 <- data.frame(table(Flightpath$Sati_flight_1.Destination.City))
Airport_Popularity01 <- data.frame(table(Flightpath$Sati_flight_1.Orgin.City))
Airport_Popularity02 <- Airport_Popularity02[-211,]

length(Flightpath$Sati_flight_1.Orgin.City=="Orlando, FL")

match01_Org <- match(Airport_Origin$Origin_city,latlon_Org$City.Name)
match01_Des <- match(Airport_Origin$Origin_city,Airport_Dest$Dest_City)
Airport_Origin[209:212,]
Airport_Dest[209:212,]
Airport_Popularity02[209:212,]

Airport_Sat <- data.frame("City"=Airport_Origin$Origin_city,"long"=latlon_Org$lon[match01_Org],"lat"=latlon_Org$lat[match01_Org],
                         "SatisfactionAsOrigin"=Airport_Origin$SatisAsOri,"SatisfactionAsDest"=Airport_Dest$SatisAsDest[match01_Des])
View(Airport_Sat)


Airport_Sat$RankAsOrigin <- rank(Airport_Sat$SatisfactionAsOrigin-MinAsO)
Airport_Sat$RankAsDest <- rank(Airport_Sat$SatisfactionAsDest-MinAsD)
Airport_Sat$Comparison <- Airport_Sat$RankAsOrigin>Airport_Sat$RankAsDest
Airport_Sat$Popularity <- (Airport_Popularity01$Freq+Airport_Popularity02$Freq)

hist(Airport_Sat$SatisfactionAsOrigin)


ggplot() +
  annotation_custom(earth, xmin = -180, xmax = 180, ymin = -90, ymax = 90) +
  geom_path(aes(long, lat, group = id, color = satisfaction), alpha = 0, size = 0, data = flightpath1_fortified) +
  geom_point(data = Airport_Sat[Airport_Sat$Comparison,], aes(long, lat, size=Popularity), alpha = 0.8,color="#ED5485") +
  geom_point(data = Airport_Sat[!Airport_Sat$Comparison,], aes(long, lat, size=Popularity), alpha = 0.8,color="#f9ba00") +
  theme(panel.background = element_rect(fill = "#05050f", colour = "#05050f"),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),axis.title = element_blank(),axis.text = element_blank(),axis.ticks.length = unit(0, "cm"),legend.position = "none") +
  #annotate("text", x = -150, y = 13, hjust = 0, size = 14,label = paste("Northwest Business Airlines Inc"), color = "#075aaa") +
  #annotate("text", x = -150, y = 10, hjust = 0, size = 14,label = paste("West Airways Inc"), color = "#f9ba00") +
  annotate("text", x = -150, y = 4, hjust = 0, size = 7,label = paste("Airport in Destination City"), color = "#dedcee") +
  annotate("text", x = -150, y = 2, hjust = 0, size = 4,
           label = paste("Flight Airport vs Satisfaction"), color = "white") +
  annotate("text", x = -150, y = 1, hjust = 0, size = 3.5,
           label = paste("Ruifeng Chen || NASA.gov || IST687 Final Project"), color = "white", alpha = 0.5) +
  coord_equal()

MaxQ <- quantile(Airport_Sat$Popularity,probs = 0.95)
ggplot(data=Airport_Sat[Airport_Sat$Popularity < MaxQ,],aes(x=SatisfactionAsOrigin,y=SatisfactionAsDest))+
  geom_point(aes(size=Popularity,color=Popularity),alpha=0.7)+
  geom_abline(color="red")+geom_abline(slope = 1,color="blue") +coord_equal()
