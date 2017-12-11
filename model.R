
flights <- read.csv('flights.csv')
#Data Preparation for Regression Model
library(dplyr)
#Considering flights to Chicago ORD airport during Nov, Dec and Jan
flights2 <- flights %>% filter(flights$MONTH == 1)
flights3 <- flights %>% filter(flights$MONTH == 11)
flights4 <- flights %>% filter(flights$MONTH == 12)
flightsbinded <- rbind(flights2, flights3, flights4)
flightsbinded <- flightsbinded %>% filter(flightsbinded$DESTINATION_AIRPORT=='ORD')
#Removing flights that were cancelled or diverted
flightsbinded <- flightsbinded %>% filter(CANCELLED==0) 
flightsbinded <- flightsbinded %>% filter(DIVERTED ==0)
#Converting to date standard format
flightsbinded$Date <- paste(flightsbinded$YEAR,flightsbinded$MONTH,flightsbinded$DAY, sep="/" )
flightsbinded$Date <- as.Date(flightsbinded$Date, "%Y/%m/%d")
flightsbindedfinal <- data.frame(flightsbinded$Date, flightsbinded$AIRLINE, flightsbinded$ORIGIN_AIRPORT,flightsbinded$DESTINATION_AIRPORT, flightsbinded$DISTANCE,flightsbinded$DEPARTURE_DELAY,flightsbinded$TAXI_OUT, flightsbinded$WHEELS_OFF, flightsbinded$TAXI_IN, flightsbinded$ARRIVAL_DELAY)
#write.csv(flightsbindedfinal, "flightsORD.csv")

#Network Graph for Regression Data
library(igraph)
edgelist <- as.matrix(flightsbindedfinal[c("flightsbinded.ORIGIN_AIRPORT", "flightsbinded.DESTINATION_AIRPORT")])
g <- graph_from_edgelist(edgelist, directed = TRUE)
g <- simplify(g)
plot(g, edge.arrow.size=0, edge.color="grey",
     vertex.color="red", vertex.frame.color="black", vertex.size=12,
     vertex.label=V(g)$name,vertex.label.size=0.1 ,vertex.label.color="black")


#Regression model
model <- read.csv('regression.csv')
model <- model[,6:13 ]
model$Climate_type = factor(model$Climate_type,
                         levels = c('Clear', 'Light', 'snow','Partly','Overcast','Sunny','Cloudy','rain','Heavy'),
                         labels = c(1, 2, 3, 4, 5, 6, 7, 8, 9))
model$flightsbinded.DEPARTURE_DELAY = ifelse(is.na(model$flightsbinded.DEPARTURE_DELAY),
                     ave(model$flightsbinded.DEPARTURE_DELAY, FUN = function(x) mean(x, na.rm = TRUE)),
                     model$flightsbinded.DEPARTURE_DELAY)
model$flightsbinded.ARRIVAL_DELAY = ifelse(is.na(model$flightsbinded.ARRIVAL_DELAY),
                                    ave(model$flightsbinded.ARRIVAL_DELAY, FUN = function(x) mean(x, na.rm = TRUE)),
                                    model$flightsbinded.ARRIVAL_DELAY)

#Distribution Graph
library(ggplot2)
ggplot(model, aes(x=flightsbinded.ARRIVAL_DELAY)) +
  geom_histogram(binwidth=.5, colour="red", fill="white") +
  xlim(-50,100)+
  geom_vline(aes(xintercept=mean(flightsbinded.ARRIVAL_DELAY, na.rm=T)), 
             color="blue", linetype="dashed", size=1)
mean(model$flightsbinded.ARRIVAL_DELAY)

#Splitting data into training set and test set
library(caTools)
split = sample.split(model$flightsbinded.ARRIVAL_DELAY, SplitRatio = 3/4)
training_set = subset(model, split == TRUE)
test_set = subset(model, split == FALSE)

#Random Forest Model
library(randomForest)
regressor = randomForest(formula = flightsbinded.ARRIVAL_DELAY ~ flightsbinded.DEPARTURE_DELAY + flightsbinded.DISTANCE + flightsbinded.TAXI_OUT +  High_temp + Climate_type,
                         data = training_set,
                         ntree = 10)
y_pred = predict(regressor, test_set)

#Linear Regression Model
linearregressor= lm(formula=flightsbinded.ARRIVAL_DELAY ~ flightsbinded.DEPARTURE_DELAY + flightsbinded.DISTANCE + flightsbinded.TAXI_OUT +  High_temp + Climate_type,
                    data = training_set)
y_linpred = predict(linearregressor, test_set)
predact <- data.frame(test_set$flightsbinded.ARRIVAL_DELAY, y_pred, y_linpred)

#SVM Regression Model
library(e1071)
svmregressor = svm(formula = flightsbinded.ARRIVAL_DELAY ~ flightsbinded.DEPARTURE_DELAY + flightsbinded.DISTANCE + flightsbinded.TAXI_OUT +  High_temp + Climate_type,
                data = training_set,
                type = 'eps-regression',
                kernel = 'radial')
svmpred=predict(svmregressor, test_set)
predact = data.frame(predact, svmpred)

#Model Evaluation
library(Metrics)
rmse(predact$test_set.flightsbinded.ARRIVAL_DELAY, predact$y_linpred)
rmse(predact$test_set.flightsbinded.ARRIVAL_DELAY, predact$y_pred)
rmse(predact$test_set.flightsbinded.ARRIVAL_DELAY, predact$svmpred)

