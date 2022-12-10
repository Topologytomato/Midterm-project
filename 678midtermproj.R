setwd("/Users/willowwu/Documents/678midtermprojectdata")
# the data is downloaded from hneighbourhood_bostonp://insideairbnb.com/get-the-data
# load the data
library(tidyverse)
library(ggplot2)
library(sf)
library(lme4)
library(geojson)

# read Boston data
listings_boston <- read.csv("BOSTON/listings 2.csv")
# neighbourhood_boston <- st_read("BOSTON/neighbourhoods.geojson")
neighbourhood_boston <- st_read("BOSTON/neighbourhoods.geojson")

# read Chicago data
listings_chicago <- read.csv("CHICAGO/listings 2.csv")
neighbourhood_chicago <- st_read("CHICAGO/neighbourhoods.geojson")
# neighbourhood_chicago_list <- read.csv("CHICAGO/neighbourhoods.csv")

# read Hawaii data
listings_hawaii <- read.csv("HAWAII/listings 2.csv")
neighbourhood_hawaii <- st_read("HAWAII/neighbourhoods.geojson")
# neighbourhood_hawaii_list <- read.csv("HAWAII/neighbourhoods.csv")

# add a column of city
listings_boston$city <- "Boston"
listings_chicago$city <- "Chicago"
listings_hawaii$city <- "Hawaii"

# EDA with plot then find out some points are not in the geojson
data_boston <- listings_boston
points <- data.frame("x" = data_boston$longitude,"y" = data_boston$latitude)
points_sf <- st_as_sf(points, coords = c('x', 'y'), crs = st_crs(4326))
points_trans <- st_transform(points_sf, 2163)
neighbourhood_boston_trans <- st_transform(neighbourhood_boston, 2163)
points_trans <- points_sf %>% 
  mutate(intersection = as.integer(st_intersects(points_trans,neighbourhood_boston_trans)))
# get the data who is in the polygons of geojson file
row_number <- which(!is.na(points_trans$intersection))
data_boston <- data_boston[row_number,]

# test if there any other points are not in the geojson file
# CHICAGO
data_chicago <- listings_chicago
points <- data.frame("x" = data_chicago$longitude,"y" = data_chicago$latitude)
points_sf <- st_as_sf(points, coords = c('x', 'y'), crs = st_crs(4326))
points_trans <- st_transform(points_sf, 2163)
neighbourhood_chicago_trans <- st_transform(neighbourhood_chicago, 2163)
points_trans <- points_sf %>% 
  mutate(intersection = as.integer(st_intersects(points_trans,neighbourhood_chicago_trans)))
# get the data who is in the polygons of geojson file
row_number <- which(!is.na(points_trans$intersection))
data_chicago <- data_chicago[row_number,]

# HAWAII
data_hawaii <- listings_hawaii
points <- data.frame("x" = data_hawaii$longitude,"y" = data_hawaii$latitude)
points_sf <- st_as_sf(points, coords = c('x', 'y'), crs = st_crs(4326))
points_trans <- st_transform(points_sf, 2163)
neighbourhood_hawaii_trans <- st_transform(neighbourhood_hawaii, 2163)
points_trans <- points_sf %>% 
  mutate(intersection = as.integer(st_intersects(points_trans,neighbourhood_hawaii_trans)))
# get the data who is in the polygons of geojson file
row_number <- which(!is.na(points_trans$intersection))
data_hawaii <- data_hawaii[row_number,]

# combine the three data
col_need <- c("host_id", "city", "neighbourhood_cleansed", "latitude", 
              "longitude", "room_type", "accommodates",
              "bathrooms_text", "bedrooms", "beds", "price")
data_bind <- bind_rows(data_boston, data_chicago, data_hawaii)[,col_need]

# deal with bedrooms and bedrooms_text
data_clean <- data_bind %>% separate(
  col = `bathrooms_text`,
  into = c("bathroom_numbers", "bathroom_type"),
  sep = " ",
  fill = "right"
)

# clean the data
# deal with half-bathroom
data_clean[which(data_clean$bathroom_type == "baths"), "bathroom_type"] <- "bath"
row_halfbath <- which(data_clean$bathroom_type == "half-bath")
data_clean[row_halfbath,"bathroom_type"] <- data_clean[row_halfbath,"bathroom_numbers"]
data_clean[row_halfbath,"bathroom_numbers"] <- 0.5
data_clean$bathroom_type <- sapply(data_clean$bathroom_type,tolower)

row_halfbath2 <- which(data_clean$bathroom_numbers == "Half-bath")
data_clean[row_halfbath2,"bathroom_type"] <- data_clean[row_halfbath2,"bathroom_numbers"]
data_clean[row_halfbath2,"bathroom_numbers"] <- 0.5
data_clean$bathroom_numbers <- sapply(data_clean$bathroom_numbers, as.numeric)

# deal with other numeric variables
data_clean$accommodates <- sapply(data_clean$accommodates, as.numeric)
data_clean$price <- gsub("\\$", "", data_clean$price)
data_clean[,10:12] <- sapply(data_clean[,10:12], as.numeric)
names(data_clean)[3]<-paste("neighbourhood")

# data cleaning finished

# EDA with different region and the house price
col1 <- c("city", "neighbourhood","price")
data_1 <- data_clean[,col1]
ggplot(data_1, aes(x=city, y=price)) + geom_boxplot(fill='lightblue') +
labs(x = "city", y = "price", title = "Price vs City") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20L,
                              hjust = 0.5),
    axis.title.y = element_text(size = 15L),
    axis.title.x = element_text(size = 15L)
  )

# continue to EDA with the geojson file

price_map_boston <- ggplot()+
  geom_sf(data=neighbourhood_boston,colour='black',fill=NA)+
  geom_point(mapping = aes(x = longitude, y = latitude, 
                           color = price, size = price),
             data = data_clean[which(data_clean$city == "Boston"),])+
  scale_color_gradientn(limits = c(20, 1000),
                         breaks = c(100, 150, 200, 250,300, 400, 750),
                         colors = c(rev(RColorBrewer::brewer.pal(11, "Spectral")))) + 
  scale_size_continuous(range=c(0.2,2))+
  labs(title = "Boston ")+
  theme_classic() +
  theme(plot.title = element_text(size = 20L, hjust = 0.5),
        legend.text = element_text(size = 12L))+
  guides(color = guide_legend(override.aes = list(size = 9)))

price_map_chicago <- ggplot()+
  geom_sf(data=neighbourhood_chicago,colour='black',fill=NA)+
  geom_point(mapping = aes(x = longitude, y = latitude, 
                           color = price, size = price),
             data = data_clean[which(data_clean$city == "Chicago"),])+
  scale_color_gradientn(limits = c(20, 1000),
                        breaks = c(100, 150, 200, 250,300, 400, 750),
                        colors = c(rev(RColorBrewer::brewer.pal(11, "Spectral")))) + 
  scale_size_continuous(range=c(0.2,2))+
  labs(title = "Chicago")+
  theme_classic() +
  theme(plot.title = element_text(size = 20L, hjust = 0.5),
        legend.text = element_text(size = 12L))+
  guides(color = guide_legend(override.aes = list(size = 9)))

price_map_hawaii <- ggplot()+
  geom_sf(data=neighbourhood_hawaii,colour='black',fill=NA)+
  geom_point(mapping = aes(x = longitude, y = latitude, 
                           color = price, size = price),
             data = data_clean[which(data_clean$city == "Hawaii"),])+
  scale_color_gradientn(limits = c(20, 1000),
                        breaks = c(100, 150, 200, 250,300, 400, 750),
                        colors = c(rev(RColorBrewer::brewer.pal(11, "Spectral")))) + 
  scale_size_continuous(range=c(0.2,1))+
  labs(title = "Hawaii")+
  theme_classic() +
  theme(plot.title = element_text(size = 20L, hjust = 0.5),
        legend.text = element_text(size = 12L))+
  guides(color = guide_legend(override.aes = list(size = 9)))

rm(neighbourhood_boston_trans, neighbourhood_chicago_trans, neighbourhood_hawaii_trans)
rm(points,points_sf,points_trans,col_need,row_number,row_halfbath,row_halfbath2)

# pool with neighbourhoods2
col2 <- c("city", "neighbourhood","price")

# BOSTON
data_boston <- data_clean[which(data_clean$city == "Boston"),col2]
data_boston[is.na(data_boston$price),] <- 0
pool_boston <-data_boston %>% group_by(neighbourhood) %>% summarise(price = mean(price))
pool_neighbourhood_boston <- neighbourhood_boston %>% left_join(pool_boston, by = "neighbourhood")

ggplot()+
  geom_sf(data=pool_neighbourhood_boston,colour='black', mapping =  aes(fill = price))+
  scale_fill_gradientn(limits = c(0, 450),
                       breaks = c(50, 100, 150, 200, 250, 300, 350, 400),
                       colors = c(rev(RColorBrewer::brewer.pal(11, "Spectral"))))+
  labs(title = "Boston average price")+
  theme(plot.title = element_text(size = 20L, hjust = 0.5),
        legend.text = element_text(size = 12L))+
  guides(color = guide_legend(override.aes = list(size = 9)))
  
# CHICAGO
data_chicago <- data_clean[which(data_clean$city == "Chicago"),col2]
data_chicago[is.na(data_chicago$price),] <- 0
pool_chicago <-data_chicago %>% group_by(neighbourhood) %>% summarise(price = mean(price))
pool_neighbourhood_chicago <- neighbourhood_chicago %>% left_join(pool_chicago, by = "neighbourhood")

ggplot()+
  geom_sf(data=pool_neighbourhood_chicago,colour='black', mapping =  aes(fill = price))+
  scale_fill_gradientn(limits = c(0, 450),
                       breaks = c(50, 100, 150, 200, 250, 300, 350, 400),
                       colors = c(rev(RColorBrewer::brewer.pal(11, "Spectral"))))+
  labs(title = "Chicago average price")+
  theme(plot.title = element_text(size = 20L, hjust = 0.5),
        legend.text = element_text(size = 12L))+
  guides(color = guide_legend(override.aes = list(size = 9)))

# HAWAII
data_hawaii <- data_clean[which(data_clean$city == "Hawaii"),col2]
data_hawaii[is.na(data_hawaii$price),] <- 0
pool_hawaii <-data_hawaii %>% group_by(neighbourhood) %>% summarise(price = mean(price))
pool_neighbourhood_hawaii <- neighbourhood_hawaii %>% left_join(pool_hawaii, by = "neighbourhood")

ggplot()+
  geom_sf(data=pool_neighbourhood_hawaii,colour='black', mapping =  aes(fill = price))+
  scale_fill_gradientn(limits = c(0, 450),
                       breaks = c(50, 100, 150, 200, 250, 300, 350, 400),
                       colors = c(rev(RColorBrewer::brewer.pal(11, "Spectral"))))+
  labs(title = "hawaii average price")+
  theme(plot.title = element_text(size = 20L, hjust = 0.5),
        legend.text = element_text(size = 12L))+
  guides(color = guide_legend(override.aes = list(size = 9)))


# fitting model
# Boston
col3 <- c("city", "neighbourhood","room_type","accommodates",
          "bathroom_numbers", "bathroom_type","bedrooms", "beds","price")
data_boston <- data_clean[which(data_clean$city == "Boston"), col3]

M1 <- lmer(price ~ room_type + accommodates + bathroom_numbers +
             bathroom_type + bedrooms + beds + (1 | neighbourhood),
           data=data_boston)
fixef (M1)
ranef (M1)

# CHICAGO
data_chicago <- data_clean[which(data_clean$city == "Chicago"), col3]
M2 <- lmer(price ~ (room_type | neighbourhood) + accommodates + bathroom_numbers +
             bathroom_type + bedrooms + beds + (1 | neighbourhood),
           data=data_chicago)
fixef(M2)
ranef(M2)

# HAWAII
col4 <- c("city", "longitude", "latitude", "neighbourhood","room_type","accommodates",
          "bathroom_numbers", "bathroom_type","bedrooms", "beds","price")
data_hawaii <- data_clean[which(data_clean$city == "Hawaii"), col4]
data_hawaii[which(data_hawaii$price > 500),]$high_value <- 1

M3 <- lmer(price ~ (room_type | neighbourhood) + accommodates + bathroom_numbers +
             bathroom_type + bedrooms + beds + (1 | neighbourhood),
           data=data_hawaii)
fixef(M2)
ranef(M2)


# fit multilevel model with price data and room variables

# fit the model with group of city
# fit the model with group of households

M1 <- lm(price ~ room_type + accommodates + bathroom_numbers +
           bathroom_type + bedrooms + beds + neighbourhood,
         data=data_1)

M2 <- lmer(price ~ room_type + accommodates + bathroom_numbers +
             bathroom_type + bedrooms + beds + (1 | neighbourhood),
           data=data_1)

# M3 <- lmer(price ~ room_type + accommodates + bathroom_numbers +
#              bathroom_type + bedrooms + beds + (neighbourhood | city),
#            data=data_1)

# fit model with price change and time

ggplot() +
  geom_polygon(data = spdf_fortified, aes(x=long, y = lat, group = group), color="black", fill = NA) +
  geom_point(data = data_chicago, aes(x=longitude, y=latitude, size=price, color=price), shape=20, stroke=FALSE)+
  theme_bw()

