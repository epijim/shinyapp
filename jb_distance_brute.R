library(gtools) # permutations
library(plyr) # column names
library(ggplot2)
library(ggmap)

# Changeable variables
setwd("~/funwithcode_local/pub_crawl/shinyapp")

basemap_13 <- get_map(location="Cambridge, UK", 
                   zoom=14, source='google', 
                   maptype="terrain", color="bw")

save(basemap_13,file="basemap_14.Rda")

load("basemap_14.Rda")

  v_pubs=c("The Cambridge Brew House","The Cambridge Blue",
           "King Street Run P.H.","The Elm Tree","Bath House","Cambridge Blue")
  v_location="Cambridge, UK"
  units="minutes"

  ## Read in data
  pubs <- read.csv("data/pubs.csv", stringsAsFactors=F)  
  google_distances <- read.csv("data/google_distances.csv", stringsAsFactors=F) 
  
  ## Tidy data
    # Add merge variable to pubs
    pubs$latlon <- paste0(pubs$lat,",",pubs$lon)
    pubs_locations <- pubs[pubs$name %in% v_pubs,"latlon"]
    # Add google distances reversed to end
    latlon_2 <- google_distances$latlon_1
    latlon_1 <- google_distances$latlon_2
    minutes <- google_distances$minutes
    metres <- google_distances$metres
    reversed <- data.frame(latlon_1,latlon_2,minutes,metres)
    google_distances <- rbind(google_distances, reversed)
    
  
## Total combinations
  pubs_locations <- pubs[pubs$name %in% v_pubs,"latlon"]
  all_combinations <-  permutations(n=length(v_pubs),
                                    r=length(v_pubs),
                                    v=pubs_locations,
                                    repeats.allowed=F)


  # distances
  output <- all_combinations
  output <- cbind(output,NA)
  for(i_row in 1:nrow(output)){
    temp_row <- output[i_row,1:length(v_pubs)]
    
    # clear row value
    i_row_value <- 0
    for(i_col in 1:(length(v_pubs)-1)){
      # get pair
      temp_pair_distance <- temp_row[i_col:(i_col+1)]
      # Look up walking time for this pair
        pair_cost <- subset(google_distances, 
               latlon_1==temp_pair_distance[1] & 
                 latlon_2==temp_pair_distance[2],
               select=minutes)
      pair_cost <- as.numeric(pair_cost[1,]) # a few dups in my google data
      # add to row
      i_row_value <- pair_cost + i_row_value
    }
    output[i_row,ncol(output)] <- i_row_value
  }

  # List of distances
  df_output <- data.frame(output, stringsAsFactors=F)
  names(df_output)[ncol(df_output)] <- "cost"
  df_output$cost <- as.numeric(df_output$cost)
  
  ggplot(df_output, aes(x=cost))+
  geom_histogram(binwidth=1)+
  theme_bw()+
  xlab("Total walking time in minutes")+
  ylab("Number of pub crawls")+
  ggtitle(paste0("Histogram of all possible walking times for all ",
                 nrow(df_output),
                "\npotential pub crawls"))

  # Get shortest route
  cost <- subset(df_output,df_output$cost==min(df_output$cost),
                 select=cost)[1,]
  shortest_route <- subset(df_output,df_output$cost==min(df_output$cost))[1,1:length(v_pubs)]
    # transpose
    shortest_route <- data.frame(t(shortest_route), stringsAsFactors=F)
    names(shortest_route)[1] <- "latlon"
    # add data
    shortest_route <- merge(shortest_route,pubs,by="latlon")
    shortest_route <- shortest_route[shortest_route$name %in% v_pubs,]

ggmap(basemap) +
  geom_point(aes(x=lon, y=lat),
               data=shortest_route)+
  geom_line(aes(x=lon, y=lat),
            data=shortest_route)
    
  ############################################################## CURRENTLY WORKING ON LOOP ABOVE!
##############################################################   ##############################################################   ##############################################################   ############################################################## 
############################################################## 
############################################################## 
  
 
    
    map <- ggmap(basemap) +
      geom_segment(aes(xend=c(tail(lon, n=-1), NA), 
                       yend=c(tail(lat, n=-1), NA)),
                   data=pubs_inorder)+
      xlab("Latitude")+
      ylab("Longitude")
    
    result <- list(distance=cost1[M],
                   pubs_inorder=pubs_inorder,
                   fittingprocess=fitting,
                   temperature=temperature)
  }
  
  print(map)
  return(result)
  
}


#jb_pubdistance(listpubs=T)

results <- jb_pubdistance()
results2 <- jb_pubdistance(units="metres")

results$distance
results$pubs_inorder
results2$distance
results2$pubs_inorder

# 42.5 minutes
# 

#results2 <- jb_pubdistance(cam_pubs=F,v_pubs=dataset)

#results3 <- jb_pubdistance(v_pubs=c("The Maypole P.H.","The Eagle Public House",
#                                    "Pickerel Inn","Baron Of Beef"))
