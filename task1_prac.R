
#Import csv files
task_data<-read.csv("F:/job/spark foundation/studentt1.csv",header = TRUE)
View(task_data)
attach(task_data)


#in linear model,it is a  linear combination  of intercept and slop and multiply with x value it will give the predict value of taht specific number
#2.4837+9.7758 * 9.25

             #Estimate Std.     
#(Intercept)   2.4837      
#hours         9.7758 

#Apply linear model


modelt1 <- lm(task_data$scores~.,data = task_data)
summary(modelt1) 



#split data into 30/70 using catools library

library(caTools)
split_model<- sample.split(task_data$scores,SplitRatio = 0.70,)
train_data <- subset(task_data,split_model== T)
test_data <- subset(task_data,split_model== F)

#train model

lmMod_t1 <- lm(scores ~ hours , data=train_data) 
plot(lmMod_t1)


newhour = data.frame(hours=9.25)
#predict(lmMod_t1,newhour)


distpred <-predict(lmMod_t1,data=test_data)
rmset1 <-sqrt(mean(distpred-test_data$scores)^2) 
rmset1

#4.3873+9.5659 *9.25
#[1] 92.87187

