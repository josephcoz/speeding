#source('http://grimshawville.byu.edu/TrafficStops2018.R')

dirstr <- as.character(getwd())
filename <- "/MontgomerySpeeding.RData"

path <- paste0(dirstr, filename)

load(path)

#Analysis:

#create a dataset with half warnings and half tickets

all_bads = subset(ticket.last, Ticket == "TRUE")
n_bads <- dim(all_bads)[1]
n_bads

all_goods = subset(ticket.last, Ticket == "FALSE")
rows_goods = dim(all_goods)[1]
set.seed(42)
n_goods = dim(all_goods)[1]
rows_goods = sample(n_goods, n_bads)
sample_goods = all_goods[rows_goods,]

#combine all_bads and sample_goods

ticket_model = rbind(all_bads, sample_goods)
dim(ticket_model)

table(ticket_model$Ticket)

#Create train and test
n_ticket_model <- dim(ticket_model)[1] #number of rows you want
train_rows <- sample(n_ticket_model, 150000) #grabs a sample of row numbers
ticket_train <- ticket_model[train_rows,] #grabs the actual rows
ticket_test <- ticket_model[-train_rows,] #grabs all the rows except what it grabbed for train

#Confirm similar (Not perfect, but close enough)
table(ticket_train$Ticket)
table(ticket_test$Ticket)


#find out column of response variable
names(ticket_train) #column 17

library(randomForest)

out_ticket = randomForest(x = ticket_train[, -17], y = ticket_train$Ticket, xtest = ticket_test[, -17],
ytest = ticket_test$Ticket, replace = TRUE, keep.forest = TRUE, ntrees = 50, mtry = 5, nodesize = 25)

#remember: replace
#keep.forest stores the model so we can make a prediction VITAL
#ntrees = 50 is almost always good, don't worry too much about figuring out trees
#mtry equals p/3 (number of explanatory variables / 3)

#remember random forest tends to overfit data

#demonstrate prediction
new.obs
predict(out_ticket, newdata = new.obs)

# variable importance
#not clear *how* any of these things are important OR
#if anything below is statistically significant;
#data suggests it matters

round(importance(out_ticket), 0)

#picture!
varImpPlot(out_ticket)