library(caTools)
library(rpart)
library(rpart.plot)

Data <- read.table("car-dataset.csv",header=TRUE,sep=',')

#data labels aren't balanced
table(Data$Label)

set.seed(777)
is_train <- sample.split(Data$Label,0.8)
train_data <- subset(Data, is_train == TRUE)
test_data <- subset(Data, is_train == FALSE)

x_test <- test_data[,-ncol(test_data)]
y_test <- test_data[,ncol(test_data)]

#kept Label Values Ratio
table(train_data$Label)
table(y_test)

dt_model <- rpart(Label ~ Feature1 + Feature2 + Feature3 + Feature4 + Feature5 
                  + Feature6, method='class', data=train_data, 
                  control=rpart.control(minsplit=1),
                  parms=list(split='information'))
rpart.plot(dt_model, type = 4, extra = 1)

prediction <- predict(dt_model,newdata=test_data,type="class")
classes_conf_matrix <- table(y_test,prediction)
sprintf("Accuracy : %f %s",
        100*sum(diag(classes_conf_matrix))/sum(classes_conf_matrix),'%')
