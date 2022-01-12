
## Read csv file
# when converting the numeric variables to factors then to numeric, it gives an error
hyth_df <- read.csv('hypothyroid.csv')

## Return unique values in each category
# unique(hyth_df)
# 
# str(hyth_df)
## From this we found a lot of '?' denoting missing values
hyth_df[hyth_df == '?'] <- NA

# hyth_df$TBG[hyth_df$TBG == '?'] <- NA
# hyth_df$T3[hyth_df$T3 == '?'] <- NA
# hyth_df$TSH[hyth_df$TSH == '?'] <- NA
# hyth_df$T4U[hyth_df$T4U == '?'] <- NA
# hyth_df$FTI[hyth_df$FTI == '?'] <- NA
# unique(hyth_df)

# Check for nulls
# anyNA(hyth_df)

# Since TBG is only Nulls 
drops <- c("TBG")
hyth_df <- hyth_df[ , !(names(hyth_df) %in% drops)]

# summary(hyth_df)

# Check missing value in data
# is.na(hyth_df)
# sum(is.na(hyth_df))

# list rows of data that have missing values
# hyth_df[!complete.cases(hyth_df),]

install.packages("mice")
library(mice)

# check how many missing in each column
md.pattern(hyth_df)

# Age has only 1 missing value so it can be dropped or imputed by most frequent; drop for simplicity
hyth_df<- hyth_df[-which(is.na(hyth_df$age)),]
md.pattern(hyth_df)

# # For sex we have 150 value so we might impute it by most representative
# sex <- as.factor(c('sex'))
# hyth_df$sex <- unclass(sex)
# 
# must_convert<-sapply(hyth_df,is.factor)       # logical vector telling if a variable needs to be displayed as numeric
# hyth_df_num<-sapply(hyth_df[,must_convert],unclass)    # data.frame of all categorical variables now displayed as numeric
# 
# hyth_sex <- sapply(hyth_df$sex, unclass) 

# data.matrix(str(hyth_df$sex))

# To convert variable to factor or to numeric
library(dplyr) 
hyth_df <- hyth_df %>%
  mutate(
    age = as.numeric(age),
    TSH = as.numeric(TSH),
    T3 = as.numeric(T3),
    TT4 = as.numeric(TT4),
    T4U = as.numeric(T4U),
    FTI = as.numeric(FTI)
  )

# check the type of variables
str(hyth_df)

# columns that are numeric are the ones mentioned; then change all others to factors
hyth_df[, -c(1, 18, 20, 22, 24, 26)] <- lapply(hyth_df[, -c(1, 18, 20, 22, 24, 26)], as.factor)

glimpse(hyth_df)
str(hyth_df)

# # convert categorical to numeric --> must be factor
# hyth_df$sex <- unclass(hyth_df$sex)
# # convert all data at once
# hyth_df_new <- sapply(hyth_df, unclass)

library(mice)
init = mice(hyth_df, maxit=0) 
predM = init$predictorMatrix
meth = init$method

meth[c("Class")]=""
meth[c("sex")]="logreg" 
meth[c("TT4", "TSH","FTI","T4U","T3")]="norm" 

str(hyth_df)

set.seed(103)

imputed = mice(hyth_df, method=meth, predictorMatrix=predM, m=6, maxit = 10)
summary(imputed)
hyth_df <- complete(imputed)

md.pattern(hyth_df)

sum(is.na(hyth_df))

# 2) We will make correlation between variables and check which to drop maybe
# https://machinelearningmastery.com/feature-selection-with-the-caret-r-package/
# ensure the results are repeatable
set.seed(7)
install.packages('mlbench')
install.packages('caret')
install.packages('e1071')
# load the library
library(mlbench)
library(caret)
library(e1071)
set.seed(2)
# define the control using a random forest selection function
control <- rfeControl(functions=rfFuncs, method="cv", number=4)
# run the RFE algorithm
results <- rfe(hyth_df[,1:28], hyth_df[,29], sizes=c(1:28), rfeControl=control)
# summarize the results
print(results)
# list the chosen features
features <- predictors(results)
# plot the results
plot(results, type=c("g", "o"))

# From the plot we can see that there're 5 top features that can describe the data
top_features <- select(hyth_df, c('TSH', 'TSH_measured', 'on_thyroxine', 'FTI', 'TT4'))
# From this plot we can observe that we only need 6 features 
# the most important features are given from predictors(results)
# Take this subset
hyth_df_subset <- top_features
# hyth_df_subset = subset(hyth_df_subset, select = -c(thyroid_surgery) )
class <- hyth_df[,29]
hyth_df_subset$class <- class
str(hyth_df_subset)
# Split to train and test data
samplesize = 0.80 * nrow(hyth_df_subset)
set.seed(80) #to generate same random sample every time & maintain consistency
index = sample( seq_len (nrow(hyth_df_subset)), size = samplesize )
# Create training and test set
datatrain = hyth_df_subset[ index, ]
datatest = hyth_df_subset[ -index, ]

#specify the cross-validation method, should be split first into train and test then make cv on train ?
ctrl <- trainControl(method = "cv", number = 10)

# training the model
model <- train(class~., data = datatrain,
               method = "rpart",
               trControl = ctrl)
# install.packages('rpart')
# library(rpart)
# #plot the model
# rpart.plot(model)

print(model) #metrics give us an idea of how well the model performed on previously unseen data

library(dplyr)
library(tidyr)
install.packages('rpart.plot')
library(rpart.plot)
#view final model
model$finalModel
prp(model$finalModel, box.palette = "Reds", tweak = 1.2) #view the tree using prop() function

#view predictions for each fold
model$resample
############ Error might be here for SARA ############## 
test_feat <- datatest[,-c(10)]
# #Check accuracy
test_out <- predict(model, newdata = test_feat)
pred <- unclass(test_out)
confusionMatrix(as.factor(pred), as.factor(unclass(datatest$class)))  #check accuracy

## Gini
#fit a decision tree model and use k-fold CV to evaluate performance
model_gini <- train(class~., data = datatrain, method = "rpart", parms = list(split = "gini"), trControl = ctrl, tuneLength = 10)

#Step 5: Evaluate - view summary of k-fold CV               
print(model_gini) #metrics give us an idea of how well the model performed on previously unseen data

model_gini$finalModel
prp(model_gini$finalModel, box.palette = "Reds", tweak = 1.2) #view the tree using prop() function

model_gini$resample

test_out2 <- predict(model_gini, newdata = test_feat)
pred2 <- unclass(test_out2)
confusionMatrix(as.factor(pred2), as.factor(unclass(datatest$class)))  #check accuracy

##### prune ###########
prune_tree<-rpart(class~. , method="class", data=datatrain,
                   control=rpart.control(minsplit=2, cp=0.001))

print(prune_tree)
prune_tree$finalModel
prp(prune_tree, box.palette = "Reds", tweak = 1.2)

test_out3 <- predict(prune_tree, newdata = test_feat, type = 'class')
pred3 <- unclass(test_out3)
confusionMatrix( as.factor(unclass(datatest$class)), as.factor(pred3)) 

