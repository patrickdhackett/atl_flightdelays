#Flight Delay Times Linear Regression Model and GLM
library(tidyr) #graph presentation
library(ggplot2) #graph presentation, visualization
library(MASS) #stdres
library(lars) #variable selection
library(caret) #cross validation
library(car) #qqplots
library(randomForest) #random forest model
library(e1071) #random forest model
library(ROCR) #confusion matrix
library(grid) #confusion matrix
library(broom) #cleaning
library(dplyr) #data manipulation
library(scales) #data manipulation
library(ggthemes) #visualization
library(gridExtra) #visualization
library(data.table) #visualization

#Clear variables and setwd
rm(list = ls())
getwd()
setwd("C:/Files/Coding/School/ISYE6414/project")
#Cleaned Flights
flights <- read.csv("data_12_1.csv",header=TRUE) # has 0,1 for weather events
#Only has data for flights delayed 1 minute or more
pos_delay <- read.csv('pos_arrival_12_1.csv', header = TRUE) 
#Only has data for flights delayed over 15 mins
over15 <- read.csv('over_15min_delay_12_1.csv', header = TRUE) 
# has 0,1 for weather events
logdata <- read.csv("log_data.csv",header=TRUE) 
#data for random forest
random_forest = read.csv("log_data_1.csv",header=TRUE)
#data for cancelled flights
can_flights = read.csv("cancelled_flights.csv",header=TRUE)
#first dataset for first linear model
flightsold = read.csv("atl_dropdelaytypes.csv",header=TRUE)
#EXPLORATORY ANALYSIS -- over15  (delay > 15 min)

#looking at temperature
boxplot(over15$ARRIVAL_DELAY~as.factor(over15$below_32), xlab = 'Temperature Below 32F', ylab = 'Departure Delay (minutes)', main = 'Boxplot Temperature being below 32F vs. Arrival Delay', ylim = c(-50,50))
#looking at flight events(rain, t-storm, snow, fog) 
#change y-lim to make axis bigger
boxplot(over15$ARRIVAL_DELAY~as.factor(over15$events), xlab = 'Type of Weather Event', ylab = 'Arrival Delay (minutes)', main = 'Boxplot of Type of Weather Event vs. Arrival Delay', ylim = c(15,150))
#Airline carrier
#boxplot(flights$DEPARTURE_DELAY~as.factor(flights$AIRLINE), xlab = 'Carrier', ylab = 'Departure Delay (minutes)', main= 'Boxplot of Carriers vs. Departure Delay', ylim = c(-50,50))
boxplot(over15$ARRIVAL_DELAY~as.factor(over15$AIRLINE), xlab = 'Carrier', ylab = 'Arrival Delay (minutes)', main= 'Boxplot of Carriers vs. Arrival Delay', ylim = c(15,200))
#precipitation
plot(over15$precip, over15$ARRIVAL_DELAY, xlab = "Precipitation (inches)", ylab = 'Arrival Delay (minutes)', main = 'Amount of Percipitation vs. Arrival Delay')
#distance traveled
plot(over15$DISTANCE, over15$ARRIVAL_DELAY, xlab = 'Distance of Flight (miles)', ylab = 'Arrival Delay (minutes)', main = 'Flight Distance vs. Arrival Delay')
#day of week
boxplot(over15$ARRIVAL_DELAY~over15$DAY_OF_WEEK, xlab = 'Day of Week of Flight', ylab = 'Arrival Delay', main = 'Boxplot of Day of Week vs. Arrival Delay', ylim = c(15,170))
#Scheduled Time
plot(over15$SCHEDULED_TIME, over15$ARRIVAL_DELAY, xlab = 'Length of Flight (mins)', ylab = 'Arrival Delay', main = 'Length of Flight vs. Arrival Delay')
#temp average
plot(over15$temp_avg, over15$ARRIVAL_DELAY, xlab = 'Average Temperature (degrees F)', ylab = 'Arrival Delay', main = 'Average Temperature vs. Arrival Delay')
#visibility
plot(over15$vis_avg,  over15$ARRIVAL_DELAY, xlab = 'Average Visibility', ylab = 'Arrival Delay', main = 'Average Visibility vs. Arrival Delay')
##NEED to RUN these and add pics
#boxplot for Fog, Rain, Thunderstorm, Snow
boxplot(over15$ARRIVAL_DELAY~over15$Fog, xlab = 'Fog', ylab = 'Arrival Delay', main = 'Boxplot of Fog vs. Arrival Delay', ylim = c(15,200))
boxplot(over15$ARRIVAL_DELAY~over15$Rain, xlab = 'Rain', ylab = 'Arrival Delay', main = 'Boxplot of Rain vs. Arrival Delay', ylim = c(15,200))
boxplot(over15$ARRIVAL_DELAY~over15$Thunderstorm, xlab = 'Thunderstorm', ylab = 'Arrival Delay', main = 'Boxplot of Thunderstorm vs. Arrival Delay', ylim = c(15,200))
boxplot(over15$ARRIVAL_DELAY~over15$Snow, xlab = 'Snow', ylab = 'Arrival Delay', main = 'Boxplot of Snow vs. Arrival Delay', ylim = c(15,200))
#hour
boxplot(over15$ARRIVAL_DELAY~as.factor(over15$hours), xlab = 'Hour of Flight', ylab = 'Arrival Delay', main = 'Hour of Flight vs. Arrival Delay', ylim = c(15,700))
boxplot(over15$ARRIVAL_DELAY~as.factor(over15$hours), xlab = 'Hour of Flight', ylab = 'Arrival Delay', main = 'Hour of Flight vs. Arrival Delay', ylim = c(15,150))
#EXPLORATORY ANALYSIS -- pos_delay (delay > 1 min)
#looking at temperature
boxplot(pos_delay$ARRIVAL_DELAY~as.factor(pos_delay$below_32), xlab = 'Temperature Below 32F', ylab = 'Departure Delay (minutes)', main = 'Boxplot Temperature being below 32F vs. Arrival Delay', ylim = c(-50,50))
#looking at flight events(rain, t-storm, snow, fog) #change y-lim to make axis bigger
boxplot(pos_delay$DEPARTURE_DELAY~as.factor(pos_delay$events), xlab = 'Type of Weather Event', ylab = 'Departure Delay (minutes)', main = 'Boxplot of Type of Weather Event vs. Departure Delay', ylim = c(-50,50))
boxplot(pos_delay$ARRIVAL_DELAY~as.factor(pos_delay$events), xlab = 'Type of Weather Event', ylab = 'Arrival Delay (minutes)', main = 'Boxplot of Type of Weather Event vs. Arrival Delay', ylim = c(0,90))
#Airline carrier
boxplot(pos_delay$DEPARTURE_DELAY~as.factor(pos_delay$AIRLINE), xlab = 'Carrier', ylab = 'Departure Delay (minutes)', main= 'Boxplot of Carriers vs. Departure Delay', ylim = c(-50,50))
boxplot(pos_delay$ARRIVAL_DELAY~as.factor(pos_delay$AIRLINE), xlab = 'Carrier', ylab = 'Arrival Delay (minutes)', main= 'Boxplot of Carriers vs. Arrival Delay', ylim = c(0,80))
#precipitation
plot(pos_delay$precip, pos_delay$ARRIVAL_DELAY, xlab = "Precipitation (inches)", ylab = 'Arrival Delay (minutes)', main = 'Amount of Percipitation vs. Arrival Delay')
#distance traveled
plot(pos_delay$DISTANCE, pos_delay$ARRIVAL_DELAY, xlab = 'Distance of Flight (miles)', ylab = 'Arrival Delay (minutes)', main = 'Flight Distance vs. Arrival Delay')
#day of week
boxplot(pos_delay$ARRIVAL_DELAY~pos_delay$DAY_OF_WEEK, xlab = 'Day of Week of Flight', ylab = 'Arrival Delay', main = 'Boxplot of Day of Week vs. Arrival Delay', ylim = c(0,100))
#Scheduled Time
plot(pos_delay$SCHEDULED_TIME, pos_delay$ARRIVAL_DELAY, xlab = 'Length of Flight (mins)', ylab = 'Arrival Delay', main = 'Length of Flight vs. Arrival Delay')
#temp average
plot(pos_delay$temp_avg, pos_delay$ARRIVAL_DELAY, xlab = 'Average Temperature (degrees F)', ylab = 'Arrival Delay', main = 'Average Temperature vs. Arrival Delay')
#visibility
plot(pos_delay$vis_avg,  pos_delay$ARRIVAL_DELAY, xlab = 'Average Visibility', ylab = 'Arrival Delay', main = 'Average Visibility vs. Arrival Delay')
#boxplot for Fog, Rain, Thunderstorm, Snow
boxplot(pos_delay$ARRIVAL_DELAY~pos_delay$Fog, xlab = 'Fog', ylab = 'Arrival Delay', main = 'Boxplot of Fog vs. Arrival Delay', ylim = c(0,150))
boxplot(pos_delay$ARRIVAL_DELAY~pos_delay$Rain, xlab = 'Rain', ylab = 'Arrival Delay', main = 'Boxplot of Rain vs. Arrival Delay', ylim = c(0,150))
boxplot(pos_delay$ARRIVAL_DELAY~pos_delay$Thunderstorm, xlab = 'Thunderstorm', ylab = 'Arrival Delay', main = 'Boxplot of Thunderstorm vs. Arrival Delay', ylim = c(0,150))
boxplot(pos_delay$ARRIVAL_DELAY~pos_delay$Snow, xlab = 'Snow', ylab = 'Arrival Delay', main = 'Boxplot of Snow vs. Arrival Delay', ylim = c(0,150))
#hour
boxplot(pos_delay$ARRIVAL_DELAY~as.factor(pos_delay$hours), xlab = 'Hour of Flight', ylab = 'Arrival Delay', main = 'Hour of Flight vs. Arrival Delay', ylim = c(0,120))

#Plotting precipitation vs average arrival delay for all flights on a given day
precipavg <- read.csv('precip_avg_delay.csv', header = TRUE)
dev.new()
par(mfrow = c(1,2))
plot(precipavg$precip, precipavg$avg_delay, xlab = 'Daily Precipitation', ylab = 'Average Daily Arrival Delay', main = "Precipitation vs. Average Arrival Delay")
abline(precipavg_model, col = 'red')
plot(over15$vis_avg,  over15$ARRIVAL_DELAY, xlab = 'Average Visibility', ylab = 'Arrival Delay', main = 'Average Visibility vs. Arrival Delay')

precipavg_model <- lm(avg_delay ~ precip, data = precipavg)
summary(precipavg_model)
abline(precipavg_model, col = 'red')

#Precip by airline--AVG. PLOTS -- lots of colors
precip_airline_delay <- read.csv('precip_avg_delay_by_flight.csv', header = TRUE)
df_melt <- tidyr::gather(precip_airline_delay, variable, value, AA:WN)
df_melt
ggplot(df_melt, aes(x = precip, y = value, colour = variable)) + geom_point() + ylab('Delay Time') + geom_smooth(method = 'lm', se = FALSE) + scale_fill_brewer(palette = 'Spectral')


## Cooks Distance on lm to determine outliers
#Exploratory LINEAR model for Cooks distance
colnames(pos_delay)
lmout <- lm(pos_delay$ARRIVAL_DELAY ~ as.factor(pos_delay$AIRLINE) + pos_delay$precip + as.factor(pos_delay$DAY_OF_WEEK) + pos_delay$DISTANCE + pos_delay$SCHEDULED_TIME + as.factor(pos_delay$hours) + pos_delay$temp_avg + as.factor(pos_delay$vis_avg) + pos_delay$Fog + pos_delay$Rain + pos_delay$Thunderstorm + pos_delay$Snow, data = pos_delay)

cook = cooks.distance(lmout)
plot(cook, type = 'h') #plot as histogram
resd = stdres(lmout)
str(resd)
ans = cbind(cook, resid)
ans
c = which(cook> .5)
c

#Outliers
pos_delay = pos_delay[-113517]

#### Scaling Data
normalize <- function(x){(x-min(x))/(max(x)-min(x))} # function to standardize
#saving the actual arrival delay, maximum delay, and minimum delay
arrival_delay <- pos_delay$ARRIVAL_DELAY 
max_delay <- max(pos_delay$ARRIVAL_DELAY)
min_delay <- min(pos_delay$ARRIVAL_DELAY)

pos_delay$DISTANCE <- normalize(pos_delay$DISTANCE) +1  # we also could use log of distance if it helps?
pos_delay$DEPARTURE_DELAY <- normalize(pos_delay$DEPARTURE_DELAY) + 1
pos_delay$SCHEDULED_TIME <- normalize(pos_delay$SCHEDULED_TIME) + 1
pos_delay$ELAPSED_TIME <- normalize(pos_delay$ELAPSED_TIME) + 1
pos_delay$AIR_TIME <- normalize(pos_delay$AIR_TIME) + 1
pos_delay$TAXI_OUT <- normalize(pos_delay$TAXI_OUT) + 1
pos_delay$TAXI_IN <- normalize(pos_delay$TAXI_IN) + 1
pos_delay$temp_avg <- normalize(pos_delay$temp_avg) + 1
pos_delay$dew_avg<- normalize(pos_delay$dew_avg) + 1
pos_delay$hum_avg <- normalize(pos_delay$hum_avg) + 1
pos_delay$vis_avg <- normalize(pos_delay$vis_avg) + 1
pos_delay$precip <- normalize(pos_delay$precip) + 1
pos_delay$ARRIVAL_DELAY <- normalize(pos_delay$ARRIVAL_DELAY) + 1


#SPLIT DATA


#Split data into train, valid, and test for non cross-validation analyses (60%-20%-20%)
{
  n_total <- nrow(pos_delay)
  n_train <- round(n_total*0.6)
  
  set.seed(8) #set seed for repeatibility
  
  train_indices <- sample(n_total,n_train)
  train <- pos_delay[train_indices,]
  #take remaining df after train is removed
  val_test <- pos_delay[-train_indices,]
  
  #split remaining dataset in half pseudorandomly using sample()
  vt_total <- nrow(val_test)
  n_valid <- round(vt_total*0.5) 
  
  val_indices <- sample(vt_total, n_valid)
  valid <- val_test[val_indices, ]
  test <- val_test[-val_indices, ]
}
#df names: train, valid, and test 

#Split for first LM model


#Split data into training & validation for cross validation, then test for final model selected
{
  n_total <- nrow(pos_delay)
  n_trainval <- round(n_total*0.8)
  
  set.seed(8) #set seed for repeatibility
  
  train_indices <- sample(n_total,n_trainval)
  trainval <- pos_delay[train_indices,]
  #take remaining df after train is removed
  test2 <- pos_delay[-train_indices,]
}
#df names: trainval, and test2
#sets are created as (80%, 20%) of the data

#Split and modifications for first LM based on full dataset
#Fixing percipitation, removing T
precip <- as.vector(flightsold$precip)
precip
for (i in seq(1, 326289, by = 1)){if (precip[i] =='T') 
{precip[i] = 0}}

flightsold$precip <- as.numeric(precip)
flightsold$precip

#split for old LM
n_totalold <- nrow(flightsold)
n_trainold <- round(n_totalold*0.6)

set.seed(8) #set seed for repeatibility

train_indicesold <- sample(n_totalold,n_trainold)
trainold <- flightsold[train_indicesold,]
#take remaining df after train is removed
val_testold <- flightsold[-train_indicesold,]

#split remaining dataset in half pseudorandomly using sample()
vt_totalold <- nrow(val_testold)
n_validold <- round(vt_totalold*0.5) 

val_indicesold <- sample(vt_totalold, n_validold)
validold <- val_testold[val_indicesold, ]
testold <- val_testold[-val_indicesold, ]
#df names: trainold, validold, and testold
#sets are created as (60%, 20%, 20%) of the data


#pulling out columns from the training data, these were the names already used in the model's code
sDistance <- as.vector(trainval$DISTANCE)
sDeparture_Delay <- as.vector(trainval$DEPARTURE_DELAY)
sScheduled_Time <- trainval$SCHEDULED_TIME
sElapsed_Time <- as.vector(trainval$ELAPSED_TIME)
sAir_Time <- as.vector(trainval$AIR_TIME)
sTaxi_Out <- as.vector(trainval$TAXI_OUT)
sTaxi_In <- as.vector(trainval$TAXI_IN)
sTemp <- as.vector(trainval$temp_avg)
sDew <- as.vector(trainval$dew_avg)
sHum <- as.vector(trainval$hum_avg)
sVis <- as.vector(trainval$vis_avg)
sPrecip <- as.vector(trainval$precip)
sArrival_Delay <- as.vector(trainval$ARRIVAL_DELAY)
sFog <- as.factor(trainval$Fog)
sRain <- as.factor(trainval$Rain)
sStorm <- as.factor(trainval$Thunderstorm)
sSnow <- as.factor(trainval$Snow)
sAirline <- as.factor(trainval$AIRLINE)
sDay <- as.factor(trainval$DAY_OF_WEEK)
sHours <- as.factor(trainval$hours)



##Following sections for non CV Models
#making validation data:
vDistance <- valid$DISTANCE
vDeparture_Delay <- valid$DEPARTURE_DELAY
vScheduled_Time <- valid$SCHEDULED_TIME
vElapsed_Time <- valid$ELAPSED_TIME
vAir_Time <- valid$AIR_TIME
vTaxi_Out <- valid$TAXI_OUT
vTaxi_In <- valid$TAXI_IN
vTemp <- valid$temp_avg
vDew <- valid$dew_avg
vHum <- valid$hum_avg
vVis <- valid$vis_avg
vPrecip <- valid$precip
vArrival_Delay <- valid$ARRIVAL_DELAY

#For Scale training data
sDistance <- train$DISTANCE
sDeparture_Delay <- train$DEPARTURE_DELAY
sScheduled_Time <- train$SCHEDULED_TIME
sElapsed_Time <- train$ELAPSED_TIME
sAir_Time <- train$AIR_TIME
sTaxi_Out <- train$TAXI_OUT
sTaxi_In <- train$TAXI_IN
sTemp <- train$temp_avg
sDew <- train$dew_avg
sHum <- train$hum_avg
sVis <- train$vis_avg
sPrecip <- train$precip
sArrival_Delay <- train$ARRIVAL_DELAY

#creating a validation data set with the parameters inputed already into the model  
validation <- data.frame(valid$AIRLINE, vPrecip, valid$DAY_OF_WEEK, vDistance, vScheduled_Time, valid$hours, vTemp, vVis, valid$Fog, valid$Rain, valid$Thunderstorm, valid$Snow)
colnames(validation) <- c('AIRLINE', 'sPrecip', 'DAY_OF_WEEK', 'sDistance' , 'sScheduled_Time', 'hours' , 'sTemp' , 'sVis' , 'Fog' , 'Rain' , 'Thunderstorm' , 'Snow')



###LINEAR

##LM #1 using all data
flights2 <- trainold
#BASIC MODEL
lm0 <- lm(ARRIVAL_DELAY ~ AIRLINE+ precip, data = flights2)
summary(lm0)
lm0$results
print(lm0)
lm0$finalModel
lm0$terms
#FULL MODEL
lm00 <- lm(ARRIVAL_DELAY ~ AIRLINE + precip + DAY_OF_WEEK + DISTANCE + SCHEDULED_TIME + temp_avg + vis_avg + events, data = flights2)
summary(lm00)
lm00$results
print(lm00)
len(lm00$coefficients)
lm00$terms
hist(lm00$residuals)
length(coef(lm00))
#STEPWISE REGRESSION -- all models sugest removing vis_avg
step(lm0, scop = list(lower = lm0, upper = lm00), direction = "forward")
step(lm00, scop = list(lower = lm0, upper = lm00), direction = "backward")
step(lm00, scop = list(lower = lm0, upper = lm00), direction = "both")

#This is model created from stepwise
model3 <- lm(ARRIVAL_DELAY ~ AIRLINE + precip + DAY_OF_WEEK + DISTANCE + SCHEDULED_TIME + temp_avg + events, data = flights2)
summary(model3)
plot(model3)
model3$terms
length(coef(model3))
#LASSO
#flights3 is dataframe with parameters we are looking at
flights3 <- data.frame(flights2$AIRLINE, flights2$precip, flights2$DAY_OF_WEEK, flights2$DISTANCE, flights2$SCHEDULED_TIME, flights2$temp_avg, flights2$vis_avg, flights2$events)
#to make lasso work, you can not have categorical variables.  model.matrix takes care of that (creates dummy variables)
predictors <- model.matrix(~flights2.AIRLINE + flights2.precip + flights2.DAY_OF_WEEK + flights2.DISTANCE + flights2.SCHEDULED_TIME + flights2.temp_avg + flights2.vis_avg + flights2.events, flights3)

#LASSO and Marlow's Cp.. looks like this shows keep all
y_var <- as.vector(train[,17]) #arrival delay column
object <- lars(x = predictors, y = y_var)
object
round(object$Cp,2)
plot.lars(object)
plot.lars(object, xvar = 'df', plottype = "Cp")

#using Lasso & penalty selected using 10-fold CV- this shows remove eventsRain
lasso.cv <- cv.glmnet(predictors, y_var, alpha = 1, nfolds =10)
lasso <- glmnet(predictors, y_var, alpha = 1, nlambda = 100)
coef(lasso, s = lasso.cv$lambda.min)
plot(lasso, xvar = 'lambda')
abline(v = log(lasso.cv$lambda.min), col = 'black', lty = 2)

#Elastic Net- this shows remove eventsRain
elasticnet.cv <- cv.glmnet(predictors, y_var, alpha = 0.5, nfolds =10)
elasticnet <- glmnet(predictors, y_var, alpha = 0.5, nlambda = 100)
coef(elasticnet, s = elasticnet.cv$lambda.min)
plot(elasticnet, xvar = 'lambda')
abline(v = log(elasticnet.cv$lambda.min), col = 'black', lty = 2)


##Linear Model #2 using scaled data, all relevant predictors, outlier(s) removed, and cross validation
#10 fold cross validation using 80% of data
set.seed(8)
train1 <- trainControl(method="cv", number=10, savePredictions = TRUE)
#names(getModelInfo()) to see model types in cv function
lm1 <- train(ARRIVAL_DELAY ~ as.factor(AIRLINE) + precip
             + as.factor(DAY_OF_WEEK) + DISTANCE 
             + SCHEDULED_TIME + as.factor(hours) + temp_avg 
             + vis_avg + as.factor(Fog) + as.factor(Rain)
             + as.factor(Thunderstorm) + as.factor(Snow), data = trainval,
             trControl=train1, method = "lm")
summary(lm1)
#Investigate assumptions
residualslm1 <- resid(lm1) #residuals
predictlm1 <- predict(lm1, trainval) #fitted values
hist(residualslm1, xlab = "Residuals LM 1", ylab = "Frequency", main = "Histogram of Linear Model 1 Residuals") #normality
qqPlot(residualslm1,lwd=1, main = 'Normality of residuals') #normality
plot(predictlm1,residualslm1,xlab = 'Fitted Values', ylab = 'Residuals of model', main = 'Residuals plot') #linearity
plot(residualslm1, xlab = 'Sequence',ylab = 'Residuals', main = 'Residuals Independence') #independence


##Linear Model #3 Using box cox transformation to account for Normality problems
lm2 <- train(ARRIVAL_DELAY ~ as.factor(AIRLINE) + precip
             + as.factor(DAY_OF_WEEK) + DISTANCE 
             + SCHEDULED_TIME + as.factor(hours) + temp_avg 
             + vis_avg + as.factor(Fog) + as.factor(Rain)
             + as.factor(Thunderstorm) + as.factor(Snow), data = trainval,
             trControl=train1, preProcess = "BoxCox", method = "lm")
summary(lm2)
lm2$results
print(lm2)
lm2$finalModel
lm2$terms

#Investigate assumptions
residualslm2 <- resid(lm2) #residuals
predictlm2 <- predict(lm2, trainval) #fitted values
hist(residualslm2, xlab = "Residuals LM 2", ylab = "Frequency", main = "Histogram of Linear Model 2 Residuals") #normality
qqPlot(residualslm1,lwd=1, main = 'Normality of residuals LM 2') #normality
plot(predictlm1,residualslm1,xlab = 'Fitted Values', ylab = 'Residuals of model', main = 'Residuals plot LM 2') #linearity
plot(residualslm1, xlab = 'Sequence',ylab = 'Residuals', main = 'Residuals Independence') #independence


#All models below are using training data, validation data separately (non CV)
#BASIC MODEL-- GAMMA
model <- glm(sArrival_Delay ~ AIRLINE + sPrecip, family = Gamma(link = 'inverse'), data = train) 

#FULL MODEL -- Using GAMMA
model1 <- glm(sArrival_Delay ~ AIRLINE + sPrecip + as.factor(DAY_OF_WEEK) + sDistance + sScheduled_Time + as.factor(hours) + sTemp + sVis + Fog + Rain + Thunderstorm + Snow, family = Gamma(link = 'inverse'), data = train)
summary(model1)
plot(model1)
res1 <- residuals(model1, type = 'deviance')
hist(res1, xlab = 'Deviance Residuals', main = 'Histogram of Scaled Gamma Model')

#Determining pseudo R2
1-(model1$deviance/model1$null.deviance)


#Prediction
#creating a valiation data set with the parameters inputed already into the model  

validation <- data.frame(valid$AIRLINE, vPrecip, valid$DAY_OF_WEEK, vDistance, vScheduled_Time, valid$hours, vTemp, vVis, valid$Fog, valid$Rain, valid$Thunderstorm, valid$Snow)
colnames(validation) <- c('AIRLINE', 'sPrecip', 'DAY_OF_WEEK', 'sDistance' , 'sScheduled_Time', 'hours' , 'sTemp' , 'sVis' , 'Fog' , 'Rain' , 'Thunderstorm' , 'Snow')

#Using predict function to determine the predicted times of arrival delay
prediction <- predict.glm(model1,validation, type = 'response')

{ #creating fucntion to unscale predictions
  original <- vector()
  valid_arrival <-vector()
  for (i in seq(1,length(prediction), by = 1)) {
    original[i] <- ((prediction[i]-1) * (max_delay-min_delay)) + min_delay
    valid_arrival[i] <- ((valid$ARRIVAL_DELAY[i]-1) * (max_delay-min_delay)) + min_delay
  }
}

#Analysis from predictions
valid_under15 <- which(valid_arrival<15) #12057 -- number of flights that actually arrived in under 15 mins
valid_under15
total_valid <- length(valid_arrival) # total flights

which.max(valid_arrival) # longest delay
valid_arrival[3648] # determine actual time of flight that corresponds to the max prediction value

pred_valid_under15 <- which(original <15) #2156 -- number of flights that actually arrived in under 15 mins
original[15115] #determine how long it took the max valid_arrival flight to arrive
max(original) #determine the max time of the prediction values


plot(valid_arrival, original, xlab = 'Actual Arrival Delay (minutes)', ylab = 'Predicted Arrival Delay (minutes)', main = 'Arrival Delay: Actual vs. Predicted')


#UNscaled data--model ran because histogram looked better
model2 <- glm(ARRIVAL_DELAY ~ AIRLINE + precip + as.factor(DAY_OF_WEEK) + DISTANCE + SCHEDULED_TIME + as.factor(hours) + temp_avg + vis_avg + Fog + Rain + Thunderstorm + Snow, family = Gamma(link = 'inverse'), data = train)
summary(model2)
res <- residuals(model2, type = 'deviance')
hist(res, xlab = 'Deviance Residuals', main = 'Histogram of Unscaled Gamma Model')
plot(model2)
#prediction values -- lot less classified correctly as ontime or late---plus much higher prediction values
prediction2 <- predict.glm(model2,valid, type = 'response')
p <- which(prediction2<15)  #984
p
max(p)

##Variable Selection

#STEPWISE REGRESSION -- all models
step(model, scop = list(lower = model, upper = model1), direction = "forward")
step(model1, scop = list(lower = model, upper = model1), direction = "backward")
step(model, scop = list(lower = model, upper = model1), direction = "both")


#Getting data ready to run LASSO, Elastic Net
training <- data.frame(train$AIRLINE, sPrecip, train$DAY_OF_WEEK, sDistance, sScheduled_Time, train$hours, sTemp, sVis, train$Fog, train$Rain, train$Thunderstorm, train$Snow)
colnames(training) <- c('AIRLINE', 'sPrecip', 'DAY_OF_WEEK', 'sDistance' , 'sScheduled_Time', 'hours' , 'sTemp' , 'sVis' , 'Fog' , 'Rain' , 'Thunderstorm' , 'Snow')
training_pred <- model.matrix(~AIRLINE + sPrecip + DAY_OF_WEEK + sDistance + sScheduled_Time + hours +sTemp + sVis + Fog + Rain + Thunderstorm + Snow, training)


#LASSO and Marlow's Cp
library(lars)
y_var <- as.vector(train[,48]) #arrival delay column
object <- lars(x = training_pred, y = y_var)
object
round(object$Cp,2)
plot.lars(object)
plot.lars(object, xvar = 'df', plottype = "Cp")


#using Lasso & penalty selected using 10-fold CV- this shows remove Rain
library(glmnet)
lasso.cv <- cv.glmnet(training_pred, y_var, alpha = 1, nfolds =10)
lasso <- glmnet(training_pred, y_var, alpha = 1, nlambda = 100)
coef(lasso, s = lasso.cv$lambda.min)
plot(lasso, xvar = 'lambda')
abline(v = log(lasso.cv$lambda.min), col = 'black', lty = 2)


#Elastic Net- this shows remove eventsRain
elasticnet.cv <- cv.glmnet(training_pred, y_var, alpha = 0.5, nfolds =10)
elasticnet <- glmnet(training_pred, y_var, alpha = 0.5, nlambda = 100)
coef(elasticnet, s = elasticnet.cv$lambda.min)
plot(elasticnet, xvar = 'lambda')
abline(v = log(elasticnet.cv$lambda.min), col = 'black', lty = 2)



### Logistic Model

#Split into trainlog, validlog, and testlog (60%-20%-20%)
{
  n_totallog <- nrow(logdata)
  n_trainlog <- round(n_totallog*0.6)
  
  set.seed(8) #set seed for repeatibility
  
  train_indiceslog <- sample(n_totallog,n_trainlog)
  trainlog <- logdata[train_indiceslog,]
  #take remaining df after train is removed
  val_testlog <- logdata[-train_indiceslog,]
  
  #split remaining dataset in half pseudorandomly using sample()
  vt_totallog <- nrow(val_testlog)
  n_validlog <- round(vt_totallog*0.5) 
  
  val_indiceslog <- sample(vt_totallog, n_validlog)
  validlog <- val_testlog[val_indiceslog, ]
  testlog <- val_testlog[-val_indiceslog, ]
}

logmodel3 = glm(LATE ~ AIRLINE + DESTINATION_AIRPORT + DAY_OF_WEEK + DISTANCE + precip + vis_avg + Fog + Rain + Snow + hours, family = "binomial", data = trainlog)
summary(logmodel3)
#rerun this 1 - (162734/179657)

#Get log odds of being late from log model (for validation dataset)
predictions = predict.glm(logmodel3, validlog, type = "response")

log_odds = predict.glm(logmodel3, trainlog, type = 'response')

#Explore Predictions
max(predictions)
which.max(predictions)
min(predictions)
length(which((predictions)>.5))
length(which((predictions)<=.5))
length(which((validlog$LATE) == 0))
length(which((validlog$LATE) == 1))

#make Predictions a vector
pred_vec = as.vector(predictions)
#bind vector to validation dataset
val_test_pred = cbind(validlog, pred_vec)

pred_late = val_test_pred$pred_vec
#change from log odds to 1 if odds are > 0.5, 0 otherwise
pred_late[pred_late >= .5 ]= 1
pred_late[pred_late < .5] = 0
#bind dataset
val_test_pred = cbind(val_test_pred, pred_late)
#confusion matrix
test_confusion.glm = confusionMatrix(data = val_test_pred$pred_late, reference = val_test_pred$LATE, positive = '1')
test_confusion.glm

#Reference
#https://github.com/ethen8181/machine-learning/blob/master/unbalanced/unbalanced_code/unbalanced_functions.R

#look at density of training set predicted log odds
ggplot( trainlog, aes( log_odds, color = as.factor(LATE) ) ) + 
  geom_density( size = 1 ) +
  scale_color_economist( name = "data", labels = c( "Not-Late", "Late" )) +
  theme_economist() +
  theme( axis.title.x = element_text(size =15), axis.title.y = element_text(size = 15))

AccuracyCutoffInfo <- function( train, test, predict, actual)
{
  # change the cutoff value's range as you please 
  cutoff <- seq( .2, .8, by = .05 )
  
  accuracy <- lapply( cutoff, function(c)
  {
    # use the confusionMatrix from the caret package
    cm_train <- confusionMatrix( as.numeric( train[[predict]] > c ), train[[actual]] )
    cm_test  <- confusionMatrix( as.numeric( test[[predict]]  > c ), test[[actual]]  )
    
    dt <- data.table( cutoff = c,
                      train  = cm_train$overall[["Accuracy"]],
                      test   = cm_test$overall[["Accuracy"]] )
    return(dt)
  }) %>% rbindlist()
  
  # visualize the accuracy of the train and test set for different cutoff value 
  # accuracy in percentage.
  accuracy_long <- gather( accuracy, "data", "accuracy", -1 )
  
  plot <- ggplot( accuracy_long, aes( cutoff, accuracy, group = data, color = data ) ) + 
    geom_line( size = 1 ) + geom_point( size = 3 ) +
    scale_y_continuous( label = percent ) +
    ggtitle( "Train/Test Accuracy for Different Cutoff" ) +
    theme( axis.title.x = element_text(size =15), axis.title.y = element_text(size = 15))
    
  
  return( list( data = accuracy, plot = plot ) )
}

ConfusionMatrixInfo <- function( data, predict, actual, cutoff )
{	
  # extract the column ;
  # relevel making 1 appears on the more commonly seen position in 
  # a two by two confusion matrix	
  predict <- data[[predict]]
  actual  <- relevel( as.factor( data[[actual]] ), "1" )
  
  result <- data.table( actual = actual, predict = predict )
  
  # caculating each pred falls into which category for the confusion matrix
  result[ , type := ifelse( predict >= cutoff & actual == 1, "TP",
                            ifelse( predict >= cutoff & actual == 0, "FP", 
                                    ifelse( predict <  cutoff & actual == 1, "FN", "TN" ) ) ) %>% as.factor() ]
  
  # jittering : can spread the points along the x axis 
  plot <- ggplot( result, aes( actual, predict, color = type ) ) + 
    geom_violin( fill = "white", color = NA ) +
    geom_jitter( shape = 1 ) + 
    geom_hline( yintercept = cutoff, color = "blue", alpha = 0.6 ) + 
    scale_y_continuous( limits = c( 0, 1 ) ) + 
    scale_color_discrete( breaks = c( "TP", "FN", "FP", "TN" ) ) + # ordering of the legend 
    guides( col = guide_legend( nrow = 2 ) ) + # adjust the legend to have two rows  
    ggtitle( sprintf( "Confusion Matrix with Cutoff at %.2f", cutoff ) )
  
  return( list( data = result, plot = plot ) )
}

trainlog$prediction = log_odds
validlog$prediction = predictions
accuracy_info <- AccuracyCutoffInfo( train = trainlog, test = validlog, 
                                     predict = "prediction", actual = "LATE" )
# define the theme for the next plot
accuracy_info$plot

cm_info <- ConfusionMatrixInfo( data = validlog, predict = "prediction", 
                                actual = "LATE", cutoff = .5)
cm_info$plot

#---------------------------------------------------------------------------------------------------------------------------

#Random Forest Model

#change data to factors for binary predictors
random_forest$LATE = factor(random_forest$LATE)
random_forest$DAY_OF_WEEK = factor(random_forest$DAY_OF_WEEK)
random_forest$below_32 = factor(random_forest$below_32)
random_forest$below_40 = factor(random_forest$below_40)
random_forest$hours = factor(random_forest$hours)
random_forest$Fog = factor(random_forest$Fog)
random_forest$Rain = factor(random_forest$Rain)
random_forest$Snow = factor(random_forest$Snow)
random_forest$Thunderstorm = factor(random_forest$Thunderstorm)
#take out information that is not available until after flight
random_forest$ARRIVAL_DELAY = NULL
random_forest$TAXI_IN = NULL
random_forest$TAXI_OUT = NULL

{
  n_totalforest <- nrow(random_forest)
  n_trainforest <- round(n_totalforest*0.6)
  
  set.seed(8) #set seed for repeatibility
  
  train_indicesforest <- sample(n_totalforest,n_trainforest)
  trainforest <- random_forest[train_indicesforest,]
  #take remaining df after train is removed
  val_testforest <- random_forest[-train_indicesforest,]
  
  #split remaining dataset in half pseudorandomly using sample()
  vt_totalforest <- nrow(val_testforest)
  n_validforest <- round(vt_totalforest*0.5) 
  
  val_indicesforest <- sample(vt_totalforest, n_validforest)
  validforest <- val_testforest[val_indicesforest, ]
  testforest <- val_testforest[-val_indicesforest, ]
}

#Train RF on training set and validation on validation set
flights.rf = randomForest(LATE ~ . ,ntree = 50, mtry = 3, data = trainforest, importance = TRUE, cutoff = c(.7,.3),
                          xtest = validforest[,-19], ytest = validforest$LATE) 

#confusion matrix for validation set
test_confusion.rf = confusionMatrix(data = flights.rf$test$predicted, reference = validforest$LATE, positive = '1')
test_confusion.rf

varImpPlot(flights.rf,type=1, main=NULL)


#----------------------------------------------------------------------------------------------------------------------

#Cancellation Analysis

#By Airline--------------------------------------------------------------------

airline_tot = summary(random_forest$AIRLINE)
airline_can = summary(can_flights$AIRLINE)
#Total Number
barchart(airline_can, xlab='cancelled flights',ylab = 'Airline')
rate_airline = airline_can/airline_tot *100
#Rate
barchart(rate_airline, xlab='cancelled flights (%)',ylab = 'Airline', xlim = c(0,10))

#By Day of Week----------------------------------------------------------------

random_forest$DAY_OF_WEEK = factor(random_forest$DAY_OF_WEEK)
can_flights$DAY_OF_WEEK = factor(can_flights$DAY_OF_WEEK)
levels(can_flights$DAY_OF_WEEK) = c('Mon.','Tues.','Wed.','Thurs,','Fri.','Sat.','Sun.')
levels(random_forest$DAY_OF_WEEK) = c('Mon.','Tues.','Wed.','Thurs,','Fri.','Sat.','Sun.')
day_can = summary(can_flights$DAY_OF_WEEK)
#Total Number
barchart(day_can, xlab='Number of Cancelled Flights',ylab='Day of Week')
day_tot = summary(random_forest$DAY_OF_WEEK)
rate_day = (day_can/day_tot) *100
#Rate
barchart(rate_day, xlab='Rate of Cancelled Flights (%)', ylab='Day of Week', xlim = c(0,1.2))

#By hour-----------------------------------------------------------------------

can_flights$hours = factor(can_flights$hours)
random_forest$hours = factor(random_forest$hours)
hour_can = summary(can_flights$hours)
#Total Number
barchart(hour_can, xlab='Number of Cancelled Flights', ylab = 'hour')
hour_tot = summary(random_forest$hours)
rate_hour = (hour_can/hour_tot[2:20])*100
#Rate
barchart(rate_hour, xlab='Rate of Cancelled Flights (%)', ylab='hour', xlim= c(0,3))

#Low Visibility-----------------------------------------------------------------

can_flights$vis_low = factor(can_flights$vis_low)
random_forest$vis_low = factor(random_forest$vis_low)
vis_can = summary(can_flights$vis_low)
vis_tot =summary(random_forest$vis_low)
rate_vis = (vis_can/vis_tot)*100
barchart(rate_vis, xlab='Rate of Cancelled Flights (%)', ylab='visability',xlim=c(0,2))
