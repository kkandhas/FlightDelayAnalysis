install.packages('maps')
install.packages('geosphere')
install.packages('rworldxtra')
install.packages('rworldmap')
install.packages('ggrepel')
install.packages('magrittr')
install.packages('visNetwork')
install.packages('data.table')
library(magrittr)
library(visNetwork)
library(data.table)
library(maps)
library(Metrics)
library(igraph)
library(geosphere)
library(dplyr)
library(rworldxtra)
library(rworldmap)
library(ggmap)
library(ggrepel)
library(grid)
library(dplyr)

## Import data 
latandlong1 <- read.csv("latandlong.csv")
latandlong <- read.csv("latandlongofnodes.csv")
delayflights <- read.csv("delayatairport.csv")

###Data Cleaning
## Make Longitudes multiply with -1 as it is positive in Data
latandlong1$Longitude <- latandlong1$Longitude*(-1)
latandlong$startlong <- latandlong$startlong*(-1)
latandlong$destlong <- latandlong$destlong*(-1)

latandlong <- na.omit(latandlong)
summary(latandlong) <- latandlong

## Filtering only lattitudes and longitudes in USA
latandlong <- latandlong %>% filter(latandlong$startlat<48.5)
latandlong <- latandlong %>% filter(latandlong$destlat<48.5)
latandlong <- latandlong %>% filter(latandlong$destlong>-130)
latandlong <- latandlong %>% filter(latandlong$startlong>-130)

latandlong1 = latandlong1[(which(latandlong1$Latitude < 65)), ]
latandlong1 = latandlong1[(which(latandlong1$Longitude > -130)), ]

## Create a matrix of edgelist 
edgelist <- as.matrix(latandlong[c("flights.ORIGIN_AIRPORT", "flights.DESTINATION_AIRPORT")])

## Creatint a graph from edgelist
g <- graph_from_edgelist(edgelist[1:10000,], directed = FALSE)
simplify(g)
View(g)

#########################################################################

## Making a network graph by plotting the Airports and routes on USA Map.
map("world",fill=T, col="grey8", bg="white", ylim=c(21.0,50.0), xlim=c(-130.0,-65.0))
#overlay airports
points(latandlong1$Longitude,latandlong1$Latitude, pch=3, cex=0.1, col="chocolate1")

for (i in (1:dim(latandlong)[1])) { 
  inter <- gcIntermediate(c(latandlong$startlong[i], latandlong$startlat[i]), c(latandlong$destlong[i], latandlong$destlat[i]), n=200)
  lines(inter, lwd=0.1, col="turquoise2")    
}

plot(g)
summary(latandlong)
##################################################################################################

##### Centrality measures for the graph
################ Closeness
  
V(g)$closeness<- centr_clo(g)$res
nodesclo <- get.data.frame(g, what = "vertices")
nodesclo <- data.frame(id = nodesclo$name, title = nodesclo$name, group = nodesclo$closeness, gcloseness = nodesclo$closeness)
setnames(nodesclo, "gcloseness", "Closeness centrality")
nodesclo <- nodesclo[order(nodesclo$id, decreasing = F),]
?setnames()
edgesclo <- get.data.frame(g, what="edges")[1:2]
visNetwork(nodesclo, edgesclo, height = "3000px", width = "100%") %>%
  visOptions(selectedBy = "Closeness centrality", highlightNearest = TRUE, nodesIdSelection = TRUE)%>%
  visPhysics(stabilization = FALSE)

################# Betweeness

V(g)$betweeness <- centr_betw(g)$res
nodesbtw <- get.data.frame(g, what = "vertices")
nodesbtw <- data.frame(id = nodesbtw$name, title = nodesbtw$name, group = nodesbtw$betweeness, gbetweeness = nodesbtw$betweeness)
setnames(nodesbtw, "gbetweeness", "btw centrality")
nodesbtw <- nodesbtw[order(nodesbtw$id, decreasing = F),]
?setnames()
edgesbtw <- get.data.frame(g, what="edges")[1:2]

visNetwork(nodesbtw, edgesbtw, height = "1200px", width = "100%") %>%
  visOptions(selectedBy = "btw centrality", highlightNearest = TRUE, nodesIdSelection = TRUE)%>%
  visPhysics(stabilization = FALSE)


################## Indegree

V(g)$indegree <- centr_degree(g, mode = "in")$res
nodes <- get.data.frame(g, what="vertices")
nodes <- data.frame(id = nodes$name, title = nodes$name, group = nodes$indegree, indegree = nodes$indegree)
setnames(nodes, "indegree", "in-degree centrality")
nodes <- nodes[order(nodes$id, decreasing = F),]

edges <- get.data.frame(g, what="edges")[1:2]
visNetwork(nodes, edges, height = "1000px", width = "100%") %>%
  visOptions(selectedBy = "in-degree centrality", highlightNearest = TRUE, nodesIdSelection = TRUE)%>%
  visPhysics(stabilization = FALSE)



############################################### END ####################################################
