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

shinyServer(function(input, output, session) {
  
df_output <- reactive({
  
  validate(
    need(!is.null(input$v_pubs),
         "Please select some pubs for the crawl"),
    need(length((input$v_pubs)<8),
         "Maximum of seven pubs sorry!")
    )
  
  v_pubs <- input$v_pubs
  pubs_locations <- pubs[pubs$name %in% input$v_pubs,"latlon"]
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
  df_output
})


output$plot1 <- renderPlot({
  df_output <- df_output()
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
  })

output$plot2 <- renderPlot({
  df_output <- df_output()
  v_pubs <- input$v_pubs
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
  
  ggmap(basemap_13) +
    geom_point(aes(x=lon, y=lat),
               data=shortest_route)+
    geom_line(aes(x=lon, y=lat),
              data=shortest_route)+
    xlab(paste0("Total walking time of crawl is ",cost," minutes."))+ylab("")
})
  
})