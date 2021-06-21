
# loading library
cat('\014') # Clear The Console
rm(list=ls()) # Clear All User Objects From The Environment

library(e1071)
library(caret)
library(dplyr)
library(kernlab)
library(rpart)


data <- read.csv("C:/Users/Siddhant/Desktop/IST 707 - Data Analytics/Project/abcde.csv")

data <- subset(data, select = -c(IncidntNum))
data <- subset(data, select = -c(PdId))

str(data)

data1 <- subset(data, select = -c(Location,Descript))
data1 <- subset(data1, select = -c(Category,Time))
data1 <- subset(data1, select = -c(Date))
data1 <- subset(data1, select = -c(X,Y))
data1 <- subset(data1, select = -c(Address,Resolution))

str(data1)
####################################################

#ARM

data1 <- data1 %>%
  mutate(Month = as.factor(Month),
         Hour = as.factor(Hour),)


library(arules)


Rules <- apriori(data1,parameter = list(supp = 0.001, conf = 0.80, minlen = 5),
                 appearance = list(rhs = c("Crime.Category=Violent","Crime.Category=Non Violent")))
inspect(Rules)


##############################################################

#Decision Tree

# Splitting Dataset into Train and Test
set.seed(121)
trainList <- createDataPartition(data1$Crime.Category,p = 0.8,
                                 list = FALSE,
                                 times = 1)

trainSet <- data1[trainList,]
testSet <- data1[-trainList,]

# Method 1
tree_1 <- rpart(Crime.Category ~.,
                data = trainSet,
                control = rpart.control(cp = 0.006, minbucket = 8))

# Test set accuracy
tree.att <- predict(tree_1, testSet, type = 'class')
confusionMatrix(tree.att, testSet$Crime.Category)

# Train set accuracy
tree1.att <- predict(tree_1, trainSet, type = 'class')

confusionMatrix(tree1.att, trainSet$Crime.Category)


library(rpart.plot)
library(rattle)


prp(tree_1)
fancyRpartPlot(tree_1)


##############################################################################


# Splitting data into train and test
set.seed(125528)
indxTrain <- createDataPartition(y = data1$Crime.Category,p = 0.90,list = FALSE)

#data1 <- data

nb_training <- data1[indxTrain,]
nb_testing <- data1[-indxTrain,]

training_independent_vars = nb_training[,-1]
training_outcomes = nb_training$Crime.Category
str(data1)

#View(data1)


# Training the model
Controls <- trainControl(method='repeatedcv',number=15)

# Laplace Correction, Distribution type, Bandwidth adjustment

model <- train(training_independent_vars,
               training_outcomes,
               method = 'nb',
               trControl= Controls)

# Model Evaluation
Predict <- predict(model,newdata = nb_testing)

Predict

confusionMatrix(Predict, nb_testing$Crime.Category )


########################################################################

#KNN


# KNN Example - Iris Dataset using Train function from the caret package
set.seed(146)



# Creating a min-max normalization function
#norm_min_max <-function(x) { 
#  ( x - min(x) ) / ( max(x) - min(x) )
#}


# Creating Test and Train datasets
inTraining <- createDataPartition(data$Crime.Category, p = .90, list = FALSE)

in_train <- data[inTraining, ] 

in_test  <- data[-inTraining, ]


# Extracting the labels from our test and train datasets
in_train_category <- data[inTraining, 1]
in_test_category <- data[-inTraining, 1]


# Setting our model parameters and creating our model
ctrl <- trainControl(method="repeatedcv",
                     repeats = 4)


in_knn <- train(Crime.Category ~ ., 
                data = in_train, 
                method = "knn", 
                trControl = ctrl, 
                tuneLength = 10)

in_knn

plot(in_knn)



# Generating predictions on the test data        
knnPredict <- predict(in_knn,
                      newdata = in_test )


#Get the confusion matrix to see accuracy value and other parameter values
confusionMatrix(knnPredict, 
                in_test_category)



##########################################################

#ANN


set.seed(128)

inTraining <- createDataPartition(y = data$Crime.Category, p = 0.80, list = FALSE)

#training <- data[indxTrain]
#testing <- data[-indxTrain]


training_set <- data1[inTraining,] %>% select(-Crime.Category)
training_labels <- data1[inTraining,]$Crime.Category

test_labels <- data1[-inTraining,]$Crime.Category 
test_set_final  <- data1[-inTraining,] %>% select(-Crime.Category) 


#Training

Controls <- trainControl(method = "cv", number = 10)

model1 <- train(training_set,
                training_labels,
                method = "nnet",
                trControl = Controls,
                tuneGrid = expand.grid(size=c(1,100),decay=c(0.55,0.45)))

# Model Evaluation
Predict <- predict(model1,newdata = test_set_final)
Predict

confusionMatrix(Predict, test_labels )


###################################################################

#SVM

# Splitting the dataset into the Training set and Test set 
set.seed(121)

inTraining <- createDataPartition(data1$Crime.Category, p = .90, list = FALSE)

training_set <- data1[inTraining,] %>% select(-Crime.Category)
training_labels <- data1[inTraining,]$Crime.Category 

test_labels <- data1[-inTraining,]$Crime.Category # Extracting test labels
test_set_final  <- data1[-inTraining,] %>% select(-Crime.Category) 




# Fitting SVM to the Training set 
Controls <- trainControl(method='cv',number=10)
Grid_lin <- expand.grid(C = seq(0, 2, length = 11))


linear_SVM_ads <- train(training_set,
                        training_labels,
                        method = 'svmLinear',
                        trControl= Controls,
                        tuneGrid = Grid_lin)

#Grid_poly <- expand.grid(degree = 2:5, scale = 0.1, C = c(0.01,0.1,1,10,20))

#poly_SVM_ads <- train(training_set,
#                      training_labels,
#                      method = 'svmPoly',
#                      trControl= Controls,
#                      tuneGrid = Grid_poly)


#Grid_rad <- expand.grid(sigma = c(0.1,1,5), C = c(0.01,0.1,1,3,5,10,20))

#rad_SVM_ads <- train(training_set,
#                     training_labels,
#                     method = 'svmRadial',
#                     trControl= Controls,
#                     tuneGrid = Grid_rad)


# Predicting the Test set results 
ads_pred_lin <- predict(linear_SVM_ads,
                        newdata = test_set_final)

#ads_pred_poly <- predict(poly_SVM_ads,
#                         newdata = test_set_final)

#ads_pred_rad <- predict(rad_SVM_ads,
#                        newdata = test_set_final)


# Making the Confusion Matrices
conf_matrix_lin <- confusionMatrix(test_labels, ads_pred_lin) 
conf_matrix_lin

#conf_matrix_poly <- confusionMatrix(test_labels, ads_pred_poly)
#conf_matrix_poly 

#conf_matrix_rad <- confusionMatrix(test_labels, ads_pred_rad)
#conf_matrix_rad


###################################################################

#Random Forest


# Splitting data into train and test
set.seed(126)
indxTrain <- createDataPartition(y = data1$Crime.Category,p = 0.80,list = FALSE)

nb_training <- data1[indxTrain,]
nb_testing <- data1[-indxTrain,]

training_independent_vars = nb_training[,-1]
training_outcomes = nb_training$Crime.Category

#View(training_independent_vars)
#View(training_outcomes)


# Training the model
Controls <- trainControl(method='repeatedcv',number=10)


model <- train(training_independent_vars,
               training_outcomes,
               method = 'ranger',
               trControl= Controls)

# Model Evaluation
PredictRandom <- predict(model,newdata = nb_testing)

PredictRandom

nb_testing$Predicted <- PredictRandom
#View(nb_testing)

confusionMatrix(PredictRandom, nb_testing$Crime.Category )

str(data6)


