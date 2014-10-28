library(gtools) # permutations
library(plyr) # column names
library(ggplot2)
library(ggmap)
library(shiny)

## Read in data
pubs <- read.csv("data/pubs.csv", stringsAsFactors=F)  
  pubs_all_list <- pubs$name
google_distances <- read.csv("data/google_distances.csv", stringsAsFactors=F) 
load("data/basemap_13.Rda")
#load("data/basemap_14.Rda")

## Tidy data
# Add merge variable to pubs
  pubs$latlon <- paste0(pubs$lat,",",pubs$lon)
# Add google distances reversed to end
  latlon_2 <- google_distances$latlon_1
  latlon_1 <- google_distances$latlon_2
  minutes <- google_distances$minutes
  metres <- google_distances$metres
  reversed <- data.frame(latlon_1,latlon_2,minutes,metres)
  google_distances <- rbind(google_distances, reversed)