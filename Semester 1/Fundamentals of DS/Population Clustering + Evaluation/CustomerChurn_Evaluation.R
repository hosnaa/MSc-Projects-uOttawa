########################### Read Data #############################################
churn_df <- read.csv("D:/DEBI_Outtawa/Semester 1/DS/Ass3/customer_churn.csv")

############################ Q (A) HoldOut and barplot########################################
# Hold out method: 67% train and 33% test
train.index <- sample(c(1:dim(churn_df)[1]), dim(churn_df)[1]*0.67)
train.df <- churn_df[train.index, ]
test.df <- churn_df[-train.index, ]

# https://www.statmethods.net/graphs/bar.html
counts <- c(nrow(train.df)/nrow(churn_df), nrow(test.df)/nrow(churn_df))
barplot(counts, main="Hold-Out data Split",
        xlab="Train and Test data", ylab = "Percentage")

############################ Q (B) Check True Churn ####################################

# Total records
total = nrow(train.df)
# True churn in Train
len = length(which(train.df$Churn == "Yes")) 
percent = len/total
counts_before <- c(percent, 1-percent)
barplot(counts_before, main="Before Resampling Churn Distribution",
        xlab="Churn or Not", ylab = "Percentage in Data", names.arg = c('Yes', 'No'))

###############################   Q (C) Resampling to 30% ###############################    
install.packages('ROSE')
library(ROSE)
train.df <- ovun.sample(Churn~., data= train.df, method = 'over', p=0.3)$data
table(train.df$Churn)
len2 = length(which(train.df$Churn == "Yes")) 
percent2 = len2/total
percent2

counts_churn <- c(percent2, 1-percent2)
barplot(counts_churn, main="Resampling Churn Distribution",
        xlab="Churn or Not", ylab = "Percentage in Data", names.arg = c('Yes', 'No'))

############################## Q (C) Previous --> undersampling True churn from 26% to 20% ######################
# number = (percent - 0.20)*total # --> 275 records need to be converted to NO
# num <- as.integer(number)+1
# 
# # Resampling the data
# # https://www.datamentor.io/r-programming/while-loop/
# i = 1
# j= 0
# while (TRUE) {
#   if (j == num){
#     break
#   }
#   else {
#     if (train.df$Churn[i] == "Yes"){
#       train.df$Churn[i] <- 'No'
#       j = j+1
#     }
#   }
#   i = i+1
# }
# 
# len2 = length(which(train.df$Churn == "Yes")) 
# percent2 = len2/total

########################################### Check Missing Data #################################
str(train.df)
# install.packages('mice')
library(mice)
md.pattern(train.df)
# Impute data
set.seed(101)
# Total charges had missing 
train.df<- train.df[-which(is.na(train.df$TotalCharges)),]
test.df<- test.df[-which(is.na(test.df$TotalCharges)),]
md.pattern(train.df)
md.pattern(test.df)

######################################### Q (D) Predictors to use and Decision Tree ######################
library(caret)
# Customer_ID isn't important, it doesn't tell any significant information
train.df <- train.df[, -c(1)]

# install.packages("rpart")
library(rpart)
model <- rpart( Churn~., data= train.df, method = 'class')
importance <- varImp(model, scale=FALSE)
print(importance)

# Drop the less important features
train.df <- train.df[,-c(1,2,3,4,6,11,16)]
test_feat <- test.df[,-c(1,2,3,4,5,7,12,17,21)]

################################ Confusion Matrix Plot ###################################
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
#####################################################################################################
ctrl <- trainControl(method = "repeatedcv",
                     number = 3, repeats = 3,
                     selectionFunction = "best",
                     savePredictions = TRUE,
                     classProbs = TRUE)

################################# TREEEE ###########################################
model2 <- train(Churn~., data = train.df,metric="ROC", method = "rpart",trControl = ctrl)
prp(model2$finalModel, box.palette = "Reds", tweak = 1.2)
test_out2 <- predict(model2, newdata = test_feat)
test_out_tree <- predict(model2, newdata = test_feat, type = 'prob')

pred2 <- unclass(test_out2)
true_class <- unclass(as.factor(test.df$Churn))
# 1--> No, 2--> Yes
con_tree <- confusionMatrix(as.factor(unclass(pred2)), as.factor(unclass(true_class)) )
draw_confusion_matrix(con_tree)


############################ Q (E) Random Forest Less (No Tune) ######################################
m_rf <- train(Churn ~ ., data = train.df, method = "rf",
              metric = "ROC", trControl = ctrl)
m_rf

pred5 <- predict(m_rf, newdata = test_feat, type='prob')
pred4 <- ifelse(pred5[,1]> 0.5,1,2)
con_rf <- confusionMatrix(as.factor(unclass(pred4)), as.factor(unclass(true_class)))
draw_confusion_matrix(con_rf)

######################################## Random Forest Tune ###################################
churn_Fact <- as.factor(train.df$Churn)
rf_tune <- randomForest(churn_fact ~ ., data = train.df, ntree= 100, nodesize = 5, maxnodes= 10)
rf_tune
test_feat$Churn <- test.df$Churn
test_out_tune <- predict(rf_tune, newdata = test_feat, type = 'prob')
pred_tune <- ifelse(test_out_tune[,1]> 0.5,1,2)
confusionMatrix(as.factor((pred_tune)), as.factor((true_class)) )

####################################### Q (F) Is the accumlation of all the three confusion metrices ######################
####################################################################################################
###################################### Q (G) ROC #######################################
# compare their ROC curves
# install.packages('pROC')
library(pROC)

roc_tree <- roc(test.df$Churn, test_out_tree[,1])
roc_rf_1 <- roc(test.df$Churn, pred5[,1])
roc_rf_tune <- roc(test.df$Churn,test_out_tune[,1])

plot(roc_tree, col = "red", legacy.axes = TRUE)
plot(roc_rf_1, col = "blue", add = TRUE)
plot(roc_rf_tune, col = "green", add = TRUE)





