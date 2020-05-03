library(tidyverse)
library(dplyr)
library(e1071)
library(caret)
library(reshape2)


CIS_435_project_data_1<-read_csv("/Users/johnhaag/Desktop/CIS_435_project_data-1.csv")
View(CIS_435_project_data_1)
attach(CIS_435_project_data_1)

plot(CIS_435_project_data_1$year, CIS_435_project_data_1$Individuals_Affected, main = "Regression for Year and Individuals Affected", xlab = 'Year', ylab = 'Individuals Affected')
abline(lm(year ~ Individuals_Affected, data = CIS_435_project_data_1), col = 'red')

 scatterPlot<-qplot(CIS_435_project_data_1$year, CIS_435_project_data_1$Individuals_Affected, data =CIS_435_project_data_1, color = Individuals_Affected, 
                    abline(lm(   CIS_435_project_data_1$year ~ CIS_435_project_data_1$Individuals_Affected, data = CIS_435_project_data_1  ), col = 'red'  ))
scatterPlot


summary(scatterPlot)












help("aes")



plot(CIS_435_project_data_1$year, CIS_435_project_data_1$Individuals_Affected, data =CIS_435_project_data_1,
     color = CIS_435_project_data_1$Individuals_Affected, abline(CIS_435_project_data_1$year ~ CIS_435_project_data_1$Individuals_Affected))

ggplot( data = CIS_435_project_data_1,mapping =  aes( x= CIS_435_project_data_1$year, y=CIS_435_project_data_1$Individuals_Affected)  +
         geom_point(color = 'blue') + geom_smooth(color = 'red') )
?ggplot
 model <-lm(CIS_435_project_data_1$Individuals_Affected ~  CIS_435_project_data_1$year, data = CIS_435_project_data_1)

 SupVector<-svm(CIS_435_project_data_1$Individuals_Affected ~ CIS_435_project_data_1$year, data = CIS_435_project_data_1)

 summary(SupVector)
 
 plot(SupVector, data = CIS_435_project_data_1)

  pred <-predict(SupVector,CIS_435_project_data_1)
  
   tab <-table(Predicted = pred, Actual = CIS_435_project_data_1$Individuals_Affected)
tab
#showing accuracy from confustion matrix didnt make training data and test data
1-sum(diag(tab))/sum(tab)

tuneModel <-tune(svm,CIS_435_project_data_1$Individuals_Affected ~ CIS_435_project_data_1$year, data = CIS_435_project_data_1,
     ranges = list(epsilon = seq(0,1,0.1) )  )

  plot(tuneModel)
summary(tuneModel)
  
  #best model choosing
myModel <- tuneModel$best.model
myModel
summary(myModel)



#multiple regression ----------------------------------------------------------


model1 <- lm( CIS_435_project_data_1$year ~CIS_435_project_data_1$Number+CIS_435_project_data_1$Individuals_Affected) 

plot(model1)
summary(model1)  

cor(CIS_435_project_data_1$Number, CIS_435_project_data_1$Individuals_Affected)  

confint(model1,conf.level = 0.95)  

model2 <-  lm(CIS_435_project_data_1$year ~ CIS_435_project_data_1$Individuals_Affected +CIS_435_project_data_1$State+
                CIS_435_project_data_1$Number)






CIS_435_project_data_1$State <- as.numeric(as.factor(CIS_435_project_data_1$State))
CIS_435_project_data_1$Type_of_Breach <- as.numeric(as.factor(CIS_435_project_data_1$Type_of_Breach))



  



#answer to question 1--------------------------------------------
 ggplot(data = CIS_435_project_data_1) + 
  geom_smooth(aes(x=CIS_435_project_data_1$year, y= CIS_435_project_data_1$Number),formula = y~x, colour= 'blue') +
  geom_jitter(aes(x=CIS_435_project_data_1$year, y= CIS_435_project_data_1$Number), colour= 'red') +
  xlab('Number of breaches') + ylab('Year') + ggtitle('Predicitive Modeling of Year vs Total Breaches')
#-----------------------------------------------------------

#this is the answer to number 4 ------------------------------------------
ggplot(data = CIS_435_project_data_1) +
  geom_jitter(aes(x=CIS_435_project_data_1$Type_of_Breach, CIS_435_project_data_1$Number), colour ='blue')+
  geom_smooth(aes(x=CIS_435_project_data_1$Type_of_Breach, CIS_435_project_data_1$Number),colour = 'red')+ 
  xlab('Type of breach') + ylab('Number of Breaches')+
  ggtitle('Type of breach in relation to Total')
  #------------------------------------------------------------------
                                  
model_Multiple_Regression <-lm(year ~ Individuals_Affected + Type_of_Breach + State)

summary(model_Multiple_Regression)

plot(model_Multiple_Regression)
#correlation between year and number of affected individuals 
cor(year, Individuals_Affected, method = "pearson")
#shows negative corralation [1] -0.01684484

confint(model_Multiple_Regression, conf.level =0.95)

                   




