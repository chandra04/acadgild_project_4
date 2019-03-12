# Reading the CSV dataset

data <- read.csv("CancerData.csv", header = T, stringsAsFactors = F)

#Overall view of the data
str(data)
#summary of data
summary(data)
#Checking the first few rows
head(data)

# id and the last column are not required, so we will drop it from our data set
data <- data[,-c(1,33)]

#Converting the target variable "diagnosis into factor from string
data$diagnosis <- factor(data$diagnosis, levels = c("B", "M"),
                               labels = c("Benign", "Malignant"))
#Now check for missing data in the dataset column wise
sapply(data, function(x) sum(is.na(x)))   # This gives zero missing values in every row


# Visulisation of missing data
library(naniar)
vis_miss(data)  # gives the data present or absent in percentage


#-----Data Exploration----------------
#Looking at the number of patients with Malignant and Benign Tumors:
library(ggplot2)
ggplot(data, aes(x= diagnosis, fill=diagnosis)) +geom_bar()+ 
    ggtitle("Patient diagnosised with Benign or Malignant")+
    theme_bw()+theme(plot.title = element_text(hjust=0.5,size=20,face='bold')+
                         geom_text(aes(label=..count..),stat="count",position = position_stack()))

#This shows 65% patients had Benign cancer and rest have Malignent

# Now we will see how these mean features are correlated with diagnosis
set.seed(1)
df <- data.frame(data)
df[] <- lapply(df, as.integer) 

library(corrplot)
corrplot(cor(df, method = 'pearson'), method = 'number')


#1.radius_mean, perimeter_mean, area_mean, compactness_mean, concavity_mean, concave points_mean show high coorelation with the diagnosis.
#2.The other variables do not really show high impact over diagnoses.
library(ggbeeswarm)
ggplot(mapping=aes(diagnosis, perimeter_mean,color=diagnosis), data) +geom_beeswarm(dodge.width=.8,cex=2)




#---------Data Partition for Modelling-------------

library(caret)
set.seed(100)
index <- createDataPartition(data$diagnosis,p=0.8, list = F, times = 1)
train <- data[index,]
test <- data[-index,]



#-----------Building Logistic Regression Model -------------------


#From the correlation plot we got the radius_mean, texture_mean, perimeter_mean, area_mean, radius_se, perimeter_se, area_se
#radius_worst, texture_worst, perimeter_worst, area_worst, texture_worst

reg_model <- glm(diagnosis~ radius_mean+texture_mean+perimeter_mean+area_mean+radius_se+perimeter_se+area_se+radius_worst+
              texture_worst+perimeter_worst+area_worst , train, family = binomial)
summary(reg_model)
#Model performance evaluation

library(ROCR)

pred <-predict(reg_model, train, type='response')

ROCRPred <- prediction(pred, train$diagnosis)
ROCRPref <- performance(ROCRPred, "tpr", "fpr")

plot(ROCRPref, colorize=T, print.cutoffs.at=seq(0.1, by=0.1))

# Make prediction on the test data set
pred_lm <- predict(reg_model, test, type = "response")


tab <-table(Actualvalue=test$diagnosis, Predictedvalue=pred_lm>0.3)
tab
#accuracy of the model
sum(diag(tab))/sum(tab)

1-sum(diag(tab))/sum(tab)
# When our threshold is 0.3 we get better result i.e. true negative value goes down and the model accuracy also increses








#----------Building Random Forest Model---------
# before creating random forest model , first we find out the optimised "mtry" value
library(randomForest)
bestmtry <-tuneRF(train, train$diagnosis, stepFactor = 1.2, improve = 0.01, trace = T, plot = T) 

rf_model <- randomForest(diagnosis~., data = train)
rf_model
rf_model$importance
varImpPlot(rf_model)


#------Random forest model validation--------------
rf_pred <- predict(rf_model, newdata = test, type = "class")
rf_pred
library(caret)

# Now we will create confusion matrix which will give clear picture of predicted variable and actual variable
confusionMatrix(table(rf_pred, test$diagnosis))

# This give accuracy of the model is 100% 


