setwd("D://azureML//Expedia_hotel_recommendations")

traindata <- read.csv(file = "sampleExpediaInputData.csv", header = TRUE)
newtraindata <- subset(traindata, select = -c(date_time, orig_destination_distance, 
                                              srch_ci, srch_co, srch_destination_id, is_booking))
newtraindata$hotel_cluster <- as.factor(newtraindata$hotel_cluster)
newtraindata <- newtraindata[1:10000, ]
str(newtraindata)

library(caret)
intrain <- createDataPartition(y = newtraindata$hotel_cluster, p = 0.7, list = FALSE)
training <- newtraindata[intrain, ]
dim(training)

testing <- newtraindata[-intrain, ]
dim(testing)

rm(traindata, intrain, newtraindata)


library(nnet)
training$hotel_cluster <- relevel(training$hotel_cluster, ref = "0")

model <- nnet(hotel_cluster ~ posa_continent+is_mobile+is_package+channel+srch_adults_cnt+srch_children_cnt+srch_rm_cnt+srch_destination_type_id+cnt+hotel_continent, 
              data=training, family="multinomial", size = 4)
summary(model1)
# predict(model, newdata = testing, "probs")
pred_prob <- predict(model, newdata=testing, type="raw")

# OR
model1 <- multinom(formula = hotel_cluster ~ posa_continent+is_mobile+is_package+channel+srch_adults_cnt+srch_children_cnt+srch_rm_cnt+srch_destination_type_id+cnt+hotel_continent, 
              data=training, MaxNWts = 1300)

summary(model1)

pred_prob1 <- predict(model1, newdata=testing, type="probs")


sampleProb1 <- pred_prob1[1:5, ]
rowSums(sampleProb1)

