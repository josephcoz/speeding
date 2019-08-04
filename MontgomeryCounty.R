#Written by: Joe Cosby
#Date: 1-23-2019

#read in data
#source('http://grimshawville.byu.edu/TrafficStops2018.R')

dirstr <- as.character(getwd())
filename <- "/MontgomerySpeeding.RData"

path <- paste0(dirstr, filename)

load(path)

head(speed.last)
tail(speed.last)
str(speed.last)

#response variable is speed.last$speed
#explanatory variables could be: SubAgency (people are more likely
#to speed in certain areas), Commercial Vehicle (probably less
#likely to speed), Alcohol (more likely to speed), Work.Zone 
#(less likely to speed), AutoYear (newer =  more likely), 
#Out.of.State (more likely).

#I think largest effect will be Out.of.State

print(mean(speed.last$speed))
print(sd(speed.last$speed))
print(length(speed.last$speed))
plot(speed.last$speed)

#Analysis
#create train and test datasets
set.seed(12)
n_speed_last = dim(speed.last)[1]
n_speed_last
train_rows = sample(n_speed_last, 8000)

speed_train <- speed.last[train_rows,]
speed_test <- speed.last[-train_rows,]

#ERROR FIXED: I previously forgot the comma after after -train_rows
library(randomForest)
out_speed <- randomForest(x = speed_train[, -18], y = speed_train$speed, xtest = speed_test[, -18],
ytest = speed_test$speed, replace = TRUE, keep.forest = TRUE, ntrees = 50, mtry = 5, nodesize = 25)
#this throws x and xtest have same number of columns..?
#ERROR FIXED: See after creation of speed_test above

#take sqrt of MSE to find RMSE
sqrt(48.07723)

#RMSE for test set
sqrt(60.87)

#make a new prediction at new.obs
new.obs
predict(out_speed, newdata = new.obs)

#to understand what the model is doing:
round(importance(out_speed), 0)

#create importance plot that's a little easier to understand
varImpPlot(out_speed)



