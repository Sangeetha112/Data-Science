#Import data

my_input <- readingSkills
View(my_input)

str(my_input)
summary(my_input)

#Splitting into train and test dataset

library("caret")

training <- createDataPartition(my_input$nativeSpeaker,p=0.7,list = F)

train <- my_input[training,]
test <- my_input[-training,]

dim(train)
dim(test)

#Data Preparation

#Checking Missing Values

sum(is.na(my_input))

#Decision tree model

#install.packages("rpart.plot")
library("rpart.plot")

tr <- trainControl(method = "repeatedcv",number = 10, repeats = 3)
dec_tree <- train(nativeSpeaker~.,data=train,method="rpart",
                  parms=list(split="gini"),
                  trControl=tr,
                 tuneLength=10)

dec_tree

#Plot decision tree

prp(dec_tree$finalModel,box.palette="Reds",tweak=1.2)

#Prediction

pred_dec <- predict(dec_tree,test)

#Confusion Matrix

confusionMatrix(pred_dec,test$nativeSpeaker)
