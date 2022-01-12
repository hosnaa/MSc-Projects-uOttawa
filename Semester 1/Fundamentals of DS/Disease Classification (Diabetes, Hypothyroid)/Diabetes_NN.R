
# Read the Data
diabet_df = read.csv('diabetes.csv')
diabet_df[diabet_df == '?'] <- NA
str(diabet_df)
md.pattern(diabet_df)
diabet_df[, -c(1, 7, 8, 9)] <- lapply(diabet_df[, -c(1, 7, 8, 9)], as.numeric)
str(diabet_df)

# check the distribution to know whether to impute be mean or median
# h<-hist(diabet_df$Glucose, breaks=10, col="red", xlab="Glucose",
#         main="Histogram with Normal Curve")
# xfit<-seq(min(diabet_df$Glucose),max(diabet_df$Glucose),length=40)
# yfit<-dnorm(xfit,mean=mean(diabet_df$Glucose),sd=sd(diabet_df$Glucose))
# yfit <- yfit*diff(h$mids[1:2])*length(diabet_df$Glucose)
# lines(xfit, yfit, col="blue", lwd=2)
hist(diabet_df$Glucose, main = "Glucose", xlab = "Density")
# Glucose seems to be symmetric somehow

hist(diabet_df$BMI, main = "BMI", xlab = "Density")
# BMI seems to have more outliers and more skewed

hist(diabet_df$BloodPressure, main = "BloodPressure", xlab = "Density")
# Pressure might be more of symmetric one

hist(diabet_df$SkinThickness, main = "SkinThickness", xlab = "Density")
# Skin is more likely to be skewed

hist(diabet_df$Insulin, main = "Insulin", xlab = "Density")
# Definitely Insulin is skewed
# impute(diabet_df$Glucose, object = NULL, method = "mean", flag = FALSE)

diabet_df[is.na(diabet_df[,2]), 2] <- mean(diabet_df[,2], na.rm = TRUE)
diabet_df[is.na(diabet_df[,c(3)]), c(3)] <- mean(diabet_df[,c(3)], na.rm = TRUE)

# For skewed dataset
for(i in 4:6){
  diabet_df[is.na(diabet_df[,i]), i] <- median(diabet_df[,i], na.rm = TRUE)
}
md.pattern(diabet_df)

hist_glucose <- density(diabet_df$Glucose) # returns the density data
plot(hist_glucose) # plots the results

hist_BMI <- density(diabet_df$BMI) # returns the density data
plot(hist_BMI) # plots the results

hist_BloodPressure <- density(diabet_df$BloodPressure) # returns the density data
plot(hist_BloodPressure) # plots the results

hist_skin <- density(diabet_df$SkinThickness) # returns the density data
plot(hist_skin) # plots the results

hist_insulin <- density(diabet_df$Insulin) # returns the density data
plot(hist_insulin) # plots the results

# Random sampling
samplesize = 0.75 * nrow(diabet_df)
set.seed(80) #to generate same random sample every time & maintain consistency
index = sample( seq_len (nrow(diabet_df)), size = samplesize )

# Create training and test set
datatrain = diabet_df[ index, ]
datatest = diabet_df[ -index, ]

# install.packages("neuralnet ")

# load library
library(neuralnet)

set.seed(2)
NN1 = neuralnet(Outcome ~., datatrain, hidden = 2 , linear.output = T)
plot(NN1)

predict_testNN1 = compute(NN1, datatest[,c(1:8)])
predict_testNN1 = predict_testNN1$net.result
predict_testNN1 <- ifelse(predict_testNN1 > 0.5, 1, 0)

plot(datatest$Outcome, predict_testNN1, col='blue', pch=16, ylab = "predicted value NN", xlab = "real value")

abline(0,1)

con1 <- confusionMatrix(as.factor(predict_testNN1[,1]), as.factor(datatest$Outcome))

draw_confusion_matrix <- function(cm) {
  
  layout(matrix(c(1,1,2)))
  par(mar=c(2,2,2,2))
  plot(c(100, 345), c(300, 450), type = "n", xlab="", ylab="", xaxt='n', yaxt='n')
  title('CONFUSION MATRIX', cex.main=2)
  
  # create the matrix 
  rect(150, 430, 240, 370, col='#3F97D0')
  text(195, 435, 'Class1', cex=1.2)
  rect(250, 430, 340, 370, col='#F7AD50')
  text(295, 435, 'Class2', cex=1.2)
  text(125, 370, 'Predicted', cex=1.3, srt=90, font=2)
  text(245, 450, 'Actual', cex=1.3, font=2)
  rect(150, 305, 240, 365, col='#F7AD50')
  rect(250, 305, 340, 365, col='#3F97D0')
  text(140, 400, 'Class1', cex=1.2, srt=90)
  text(140, 335, 'Class2', cex=1.2, srt=90)
  
  # add in the cm results 
  res <- as.numeric(cm$table)
  text(195, 400, res[1], cex=1.6, font=2, col='white')
  text(195, 335, res[2], cex=1.6, font=2, col='white')
  text(295, 400, res[3], cex=1.6, font=2, col='white')
  text(295, 335, res[4], cex=1.6, font=2, col='white')
  
  # add in the specifics 
  plot(c(100, 0), c(100, 0), type = "n", xlab="", ylab="", main = "DETAILS", xaxt='n', yaxt='n')
  text(10, 85, names(cm$byClass[1]), cex=1.2, font=2)
  text(10, 70, round(as.numeric(cm$byClass[1]), 3), cex=1.2)
  text(30, 85, names(cm$byClass[2]), cex=1.2, font=2)
  text(30, 70, round(as.numeric(cm$byClass[2]), 3), cex=1.2)
  text(50, 85, names(cm$byClass[5]), cex=1.2, font=2)
  text(50, 70, round(as.numeric(cm$byClass[5]), 3), cex=1.2)
  text(70, 85, names(cm$byClass[6]), cex=1.2, font=2)
  text(70, 70, round(as.numeric(cm$byClass[6]), 3), cex=1.2)
  text(90, 85, names(cm$byClass[7]), cex=1.2, font=2)
  text(90, 70, round(as.numeric(cm$byClass[7]), 3), cex=1.2)
  
  # add in the accuracy information 
  text(30, 35, names(cm$overall[1]), cex=1.5, font=2)
  text(30, 20, round(as.numeric(cm$overall[1]), 3), cex=1.4)
  text(70, 35, names(cm$overall[2]), cex=1.5, font=2)
  text(70, 20, round(as.numeric(cm$overall[2]), 3), cex=1.4)
}

draw_confusion_matrix(con1)

####### Scaling ########
## Scale data for neural network - important to prevent a variable from having a large impact on the prediction variable 
max = apply(diabet_df , 2 , max)
min = apply(diabet_df, 2 , min)
# max = max(diabet_df)
# min = min(diabet_df)

scaled_minmax = as.data.frame(scale(diabet_df, center = min, scale = max - min))

## Fit neural network 

# install library
#install.packages("neuralnet ")

# load library
library(neuralnet)

# creating training and test set
trainNN = scaled_minmax[index , ]
testNN = scaled_minmax[-index , ]

# fit neural network
# If you faced error in convergence: run the line of library(neural) and comment the set.seed line

set.seed(2)
# Pregnancies + Glucose + BloodPressure + DiabetesPedigreeFunction +
#   Age + BMI + SkinThickness + Insulin
NN = neuralnet(Outcome ~., trainNN, hidden = 2 , linear.output = T)

# plot neural network
plot(NN)

## Prediction using neural network

predict_testNN = compute(NN, testNN[,c(1:8)])
predict_testNN = predict_testNN$net.result
predict_testNN <- ifelse(predict_testNN > 0.5, 1, 0)

plot(datatest$Outcome, predict_testNN, col='blue', pch=16, ylab = "predicted value NN", xlab = "real value")

abline(0,1)

con <- confusionMatrix(as.factor(predict_testNN[,1]), as.factor(datatest$Outcome))

draw_confusion_matrix(con)

######## 2 layers; 5 nodes ##########
library(neuralnet)

# If you faced error in convergence: run the line of library(neural) and comment the set.seed line
set.seed(2)
# Pregnancies + Glucose + BloodPressure + DiabetesPedigreeFunction +
#   Age + BMI + SkinThickness + Insulin
NN2 = neuralnet(Outcome ~., trainNN, hidden = c(5,5) , linear.output = F)

# plot neural network
plot(NN2)

## Prediction using neural network

predict_testNN2 = compute(NN2, testNN[,c(1:8)])
predict_testNN2 = predict_testNN2$net.result
predict_testNN2 <- ifelse(predict_testNN2 > 0.5, 1, 0)

plot(datatest$Outcome, predict_testNN2, col='blue', pch=16, ylab = "predicted value NN", xlab = "real value")

abline(0,1)

con2 <- confusionMatrix(as.factor(predict_testNN2[,1]), as.factor(datatest$Outcome))

draw_confusion_matrix(con2)

############ change in activation--> tanh ###############
library(neuralnet)

# If you faced error in convergence: run the line of library(neural) and comment the set.seed line
set.seed(2)
NN3 = neuralnet(Outcome ~., trainNN, hidden = 2 , linear.output = F, 
                act.fct = tanh)

# plot neural network
plot(NN3)

## Prediction using neural network

predict_testNN3 = compute(NN3, testNN[,c(1:8)])
predict_testNN3 = predict_testNN3$net.result
predict_testNN3 <- ifelse(predict_testNN3 > 0.5, 1, 0)

plot(datatest$Outcome, predict_testNN3, col='blue', pch=16, ylab = "predicted value NN", xlab = "real value")

abline(0,1)

con3 <- confusionMatrix(as.factor(predict_testNN3[,1]), as.factor(datatest$Outcome))

draw_confusion_matrix(con3)

############ change in learning rate--> 0.01 ###############
library(neuralnet)

# If you faced error in convergence: run the line of library(neural) and comment the set.seed line
set.seed(2)
NN4 = neuralnet(Outcome ~., trainNN, hidden = 2 , linear.output = F, 
                act.fct = tanh, learningrate = 0.01)

# plot neural network
plot(NN4)

## Prediction using neural network

predict_testNN4 = compute(NN4, testNN[,c(1:8)])
predict_testNN4 = predict_testNN4$net.result
predict_testNN4 <- ifelse(predict_testNN4 > 0.5, 1, 0)

plot(datatest$Outcome, predict_testNN4, col='blue', pch=16, ylab = "predicted value NN", xlab = "real value")

abline(0,1)

con4 <- confusionMatrix(as.factor(predict_testNN4[,1]), as.factor(datatest$Outcome))

draw_confusion_matrix(con4)

############ change in epochs--> 10/5/3 --> 3 was best, no LR as it didn't improve ###############

library(neuralnet)
# If you faced error in convergence: run the line of library(neural) and comment the set.seed line
set.seed(2)
NN5 = neuralnet(Outcome ~., trainNN, hidden = 2 , linear.output = F, 
                act.fct = tanh,  rep = 3)

# plot neural network
plot(NN5)

## Prediction using neural network

predict_testNN5 = compute(NN5, testNN[,c(1:8)])
predict_testNN5 = predict_testNN5$net.result
predict_testNN5 <- ifelse(predict_testNN5 > 0.5, 1, 0)

plot(datatest$Outcome, predict_testNN5, col='blue', pch=16, ylab = "predicted value NN", xlab = "real value")

abline(0,1)

con5 <- confusionMatrix(as.factor(predict_testNN5[,1]), as.factor(datatest$Outcome))

draw_confusion_matrix(con5)
