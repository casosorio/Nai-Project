########## Simulating a dataset to start writting scripts


Send.Time<-c("9/14/20 7:00 AM", "9/19/21 12:00 AM
", "9/26/21 12:00 AM
")

Reference.Name<-c("FI Campaign 014
", "Eq Campaign 012
", "Newsletter 049
")

From.Firm<-c("True", "False")

email.Type<-c("Product","Other/Hybrid", "Event","Thought Leadership")

Product.Name<-c("Core Bond","Multiple", "Omit")

Subscription<- c("True","False")

Blast.Message<-c("True","False")

Distribution.Partner.Specific<-c("True","False")

Preferred.Partner<-c("True","False")

Producers.Only<-c("True","False")

Prospect.Only<-c("True","False")

Focus.Status<-c("True","False")

data <- data.frame(Send.Time=sample(Send.Time, 1000, replace=TRUE),
                   Reference.Name=sample(Reference.Name, 1000, replace=TRUE),
                   From.Firm=sample(From.Firm, 1000, replace=TRUE),
                   email.Type=sample(email.Type, 100, replace=TRUE),
                   Product.Name=sample(Product.Name, 1000, replace=TRUE),
                   Subscription=sample(Subscription, 1000, replace=TRUE),
                   Audience.Size=sample(5000:100000, 1000, replace=TRUE),
                   Blast.Message=sample(Blast.Message, 1000, replace=TRUE),
                   Distribution.Partner.Specific=sample(Distribution.Partner.Specific, 1000, replace=TRUE),
                   Preferred.Partner=sample(Preferred.Partner, 1000, replace=TRUE),
                   Producers.Only=sample(Producers.Only, 1000, replace=TRUE),
                   Prospect.Only=sample(Prospect.Only, 1000, replace=TRUE),
                   AUM=sample(25000:2500000, 1000, replace=TRUE),
                   Morningstar.Rating=sample(3:5, 1000, replace=TRUE),
                   Focus.Status=sample(Focus.Status, 1000, replace=TRUE),
                   US.AUM=sample(25000:25000000, 1000, replace=TRUE),
                   US.Intermediary.AUM=sample(2500000:2500000000, 1000, replace=TRUE),
                   Open.Rate=sample(1:100, 1000, replace=TRUE),
                   Clickthrought.Rate=sample(1:100, 1000, replace=TRUE),
                   Bounce.Rate=sample(1:100, 1000, replace=TRUE),
                   Unsubscribe.Rate=sample(1:1000, 1000, replace=TRUE)
                   

                   


)

write.csv(data, "simulation.csv")                  


################ Descriptive Statistics
library(descr)
par(mfrow=c(3,2))  

freq(data$From.Firm)
freq(data$Subscription)
freq(data$Blast.Message)
freq(data$Distribution.Partner.Specific)
freq(data$Preferred.Partner)
freq(data$Producers.Only)

freq(data$email.Type)
freq(data$Reference.Name)
freq(data$Product.Name)
frea(data$Focus.Status)

summary(data$Audience.Size)                  
summary(data$Open.Rate)
summary(data$Clickthrought.Rate)
summary(data$Bounce.Rate)
summary(data$Unsubscribe.Rate)
summary(data$AUM)
summary(data$Morningstar.Rating)
summary(data$US.AUM)
summary(data$US.Intermediary.AUM)




##################### Machine Learning
library(caret)
validation_index <- createDataPartition(data$Open.Rate, p=0.80, list=FALSE)

validation <- dataset[-validation_index,]

dataset <- dataset[validation_index,]

train.control <- trainControl(method = "cv", 
                              number = 5,verboseIter = TRUE)
metric="RMSE"

set.seed(7)
model.knn <- train(Open.Rate~., data=dataset, method="knn", metric=metric, trControl=control)

set.seed(9)
model.lm <- train(Open.Rate ~., data = dataset, method = "lm", metric=metric,
                  trControl = train.control)
summary(model.lm)

predictions <- predict(model.lm, validation)
confusionMatrix(predictions, validation$Open.Rate)