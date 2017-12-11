flights=read.csv('flights.csv')
airports=read.csv('airports.csv')
airlines=read.csv('airlines.csv')
data <- read.csv('clean.csv')
library(ggplot2)
library(igraph)
library(dplyr)

#Data Cleaning
data$delaytype=ifelse(data$flights.ARRIVAL_DELAY<5, 'On time',ifelse(data$flights.ARRIVAL_DELAY>45,'Huge Delay','Slight Delay'))
flights$Date <- paste(flights$YEAR,flights$MONTH,flights$DAY, sep="/" )
flights <- data.frame(flights$Date, flights$AIRLINE, flights$SCHEDULED_DEPARTURE, flights$DEPARTURE_TIME,flights$DEPARTURE_DELAY,flights$ORIGIN_AIRPORT, flights$DESTINATION_AIRPORT, flights$SCHEDULED_ARRIVAL, flights$ARRIVAL_TIME, flights$ARRIVAL_DELAY, flights$delaytype
                      )
flights$flights.Date <- as.Date(flights$flights.Date, "%Y/%m/%d")
data <- na.omit(data)

#Exploratory Analysis 
#Results were visualized in Tableau
meandelay <- data %>% group_by(flights.AIRLINE) %>% summarise(delay=mean(flights.ARRIVAL_DELAY))
plot(meandelay$flights.AIRLINE, meandelay$delay,type="b", xlab='Airlines', ylab='Mean Delay', title='Mean delay at destination')
write.csv(meandelay, 'Delay.csv')
meandelayairport <- data %>% group_by(flights.DESTINATION_AIRPORT) %>% summarise(delay=mean(flights.ARRIVAL_DELAY))
plot(meandelayairport$flights.DESTINATION_AIRPORT, meandelayairport$delay)
write.csv(meandelayairport,'delayatairport.csv')
flightcount <- data %>% group_by(flights.AIRLINE) %>% distinct(flights.DESTINATION_AIRPORT) %>% summarise(n())
flightsana <- data %>% group_by(flights.AIRLINE, flights.delaytype) %>% summarise(n())
write.csv(flightsana, 'vis.csv')

#Counting number of distict destination airport for each airlines
distintdest <- flights %>% group_by(AIRLINE) %>% distinct(DESTINATION_AIRPORT) %>% summarise(n())
plot(distintdest$AIRLINE, distintdest$`n()`, col="red" )

cleanedflights <- read.csv('clean.csv')
#Network Graph
edgelist <- as.matrix(cleanedflights[c("flights.ORIGIN_AIRPORT", "flights.DESTINATION_AIRPORT")])
#Using only 10000 samples
edgelistsample <- as.matrix(edgelist[1:1000,])
g <- graph_from_edgelist(edgelist, directed = TRUE)
g <- simplify(g)
plot.igraph(g, 
            edge.arrow.size=0,
            edge.color="grey",
            edge.width=0.3,
            vertex.size=1,
            vertex.color=NA, 
            vertex.label=V(g)$name,
            vertex.label.color='blue',
            vertex.label.size=0.01,
            vertex.label.cex=1,
            layout=layout.circle(g)
)


#Mean Shortest Distance
mean_distance(g, directed = TRUE)

#Clustering
datacluster <- data %>% group_by(flights.DESTINATION_AIRPORT) %>% summarise(mean(flights.ARRIVAL_DELAY))
dendrogram = hclust(d = dist(datacluster, method = 'euclidean'), method = 'centroid')
plot(dendrogram,
     main = paste('Dendrogram'),
     xlab = 'Airport',
     ylab = 'Euclidean distances')

hc = hclust(d = dist(datacluster, method = 'euclidean'), method = 'ward.D')
y_hc = cutree(hc, 3)

library(cluster)
clusplot(datacluster,
         y_hc,
         lines = 0,
         shade = T,
         color = T,
         labels= 1,
         plotchar = T,
         span = T,
         main = paste('Clusters of airports based on average arrival delay'),
         ylab = 'Mean Delay')

