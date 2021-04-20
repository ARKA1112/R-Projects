getwd()
setwd(choose.dir())
library(tidyverse)
install.packages('tidyverse')
library(tidyverse)
#Loading the healthcare xls file
library(readxl)
  Healthcare = read_xlsx('E:/SIMPLILEARn/Project/Healthcare cost analysis p7/1555054100_hospitalcosts.xlsx')

summary(Healthcare)
str(Healthcare)

#Since the female column is a categorical variable it will be convenient to convert it into 
#a factor
Healthcare$AGE = as.factor(Healthcare$AGE)
Healthcare$FEMALE = as.numeric(Healthcare$FEMALE)
# checking for na or missing values
apply(Healthcare, 2, sum)
sum(is.na(Healthcare$AGE))
sum(is.na(Healthcare$FEMALE))
sum(is.na(Healthcare$LOS))
sum(is.na(Healthcare$RACE))
sum(is.na(Healthcare$TOTCHG))
sum(is.na(Healthcare$APRDRG))
#Since the Race column has an NA field we will have to check it!

Healthcare = na.omit(Healthcare)
view(Healthcare)
str(Healthcare)

length(unique(Healthcare$APRDRG))

# There are 63 unique diagnosis related groups

#TASK 1
str(Healthcare)
library(ggplot2)
#total charges accumulated per age

totchgagewise = aggregate(TOTCHG~AGE,Healthcare,sum)
str(totchgagewise)
view(totchgagewise)
summary(Healthcare$AGE)
a1 = (summary(Healthcare$AGE))
a1  = as.data.frame(a1)
str(a1)
view(a1)
Healthcare$CUME = totchgagewise$TOTCHG
#creating a new data frame consisting of occurences of the ages and cumulative expenditures
totchgagewise = cbind(totchgagewise,summary(Healthcare$AGE))
view(totchgagewise)
?aggregate
#a stacked barplot will be good to ccheck patient age vs expenditure and their counts
names(Healthcare)
ggplot(totchgagewise)+
  geom_point(aes(x = AGE, y = summary(Healthcare$AGE),size  = TOTCHG),color ='red',shape = 22 ,fill = 'green')+
  theme(plot.background = element_rect(fill = 'white'),panel.background = element_rect(fill = 'gray'))+
  xlab('Age Group')+
  ylab('Counts')+
  ggtitle('Plot of Age group and counts and their corresponding expenditures')
#Question 2 
#Finding which diagnosis grp has the highest expenditure and admission


unique(Healthcare$APRDRG)
#For that lets factorise the APRDRG

Healthcare$APRDRG = as.factor(Healthcare$APRDRG)
str(Healthcare)

H_table = as.data.frame(table(Healthcare$APRDRG))
view(H_table)
colnames(H_table) = c('APRDRG', 'Counts')
#From this table one can clearly observe that
#APRDRG group number 640 has the highest number of patient admission


H_table_1 = aggregate(TOTCHG~APRDRG,Healthcare,sum)
view(H_table_1)

# deleting the third column as it is only a repetition of the first column
H_table = merge(H_table,H_table_1)
view(H_table)

#This H_table represents the diagnosis groups and their case counts and their total expenditure


  ggplot(H_table)+
    geom_point(aes(x = APRDRG, y = TOTCHG,size = Counts),col = 'red')+
    theme(axis.ticks.x = element_line(color = 'purple'),axis.ticks.y = element_line(color = 'purple'))+
    theme(axis.title.x = element_text(vjust = 0.6))+
    theme(axis.text.x = element_text(vjust = 0.4, angle = 45))
#Q3 whether the race of the patient is related to tot charges
  
str(Healthcare)
#Converting race to a factor

Healthcare$RACE = as.factor(Healthcare$RACE)
unique(Healthcare$RACE)
summary(Healthcare$RACE)

Race_health = aggregate(TOTCHG~RACE,Healthcare,sum)
view(Race_health)
Race_health1  = as.data.frame(table(Healthcare$RACE))
colnames(Race_health1) = c('RACE','COUNTS')
view(Race_health1)
Race_health = merge(Race_health,Race_health1)
view(Race_health)
Race_health$PPCOST = Race_health$TOTCHG/Race_health$COUNTS
View(Race_health)
#lets make and lmtest to confirm the influence of diffrent variables influencing totchg

??lmtest
install.packages('lmtest')
library(lmtest)
?lm
fit1  = lm(TOTCHG~.,Healthcare)
summary(fit1)
#from this summary one can observe that Race has no significantr role to play in terms of charge
# We will also use an alternative mothod to verify it
#making fit yusing a decision tree
library(rpart)
fit2 = rpart(TOTCHG~.,Healthcare)
  ?rpart
library(rpart.plot)
plot(fit2)
?plot
summary(fit2)

#from tthe lmtest we can clearly infer that the race has no significant influence
#in prices
#also from the decision tree we can clearly see RACE as almost no influence in the 
#price hence its safe to conclude that the healthcare is devoid of malpractice

#for the 4th question lets make a decision tree featuring the variables discussed
str(Healthcare)
Healthcare$FEMALE = as.factor(Healthcare$FEMALE)
fit3  =  rpart(TOTCHG~AGE+FEMALE,Healthcare)

fit3
summary(fit3)
plot(fit3,cex= 0.6)

#using lmtest
fit4 = lm(TOTCHG~AGE+FEMALE,Healthcare)
fit4
summary(fit4)
table(Healthcare$FEMALE)
#The decision treee concludes that the age has the highest influence 
#in total charges.
#The lmtest also specifies that Age group numbers 1,3,4,5,6,9,10,15,17 makes the most influence

#Q5 we will have to predict the LOS based on the specified criterions in the table


#Before we decide which method to use for prediction we will partition the data into
#training and test datasets
library(caret)
set.seed(10)
Healthcare$LOS = as.factor(Healthcare$LOS)
#shuffling the data
shuffle_index = sample(1:nrow(Healthcare))
Healthcare = Healthcare[shuffle_index,]
view(Healthcare)
trng1 = createDataPartition(Healthcare$LOS, p = 0.7,list = F)
view(trng1)
training_data = Healthcare[trng1,]
view(training_data)
test_data = Healthcare[-trng1,]
summary(training_data)
unique(Healthcare$LOS)
Healthcare$LOS = as.factor(Healthcare$LOS)
#Selecting the method of prediction

#1 method lm test

fit_1 = lm(LOS~AGE+FEMALE+RACE, training_data)
fit_1
summary(fit_1)

#2 method glm test

fit_2 = glm(LOS~AGE+FEMALE+RACE, training_data,family = binomial(link = 'logit'))
fit_2
summary(fit_2)
#KNN test
?knn
install.packages('class')
library(class)
library(caret)
library(knn)
fit_3 = knn(training_data,test_data,cl = training_data$LOS, k = 41)
fit3
plot(fit_3)
con_mat = confusionMatrix(test_data$LOS,fit_3)
str(fit3)
con_mat
#Decision tree classifier

fit_4 = rpart(LOS~AGE+FEMALE+RACE, training_data, method = 'class')
rpart.plot(fit_4, extra = 106, cex = 0.4)
str(fit4)
pred_2 = predict(fit_4,test_data,type = 'class')
pred_table2 = table(test_data$LOS,pred_2)
pred_table2
accuracy_test = sum(diag(pred_table2))/sum(pred_table2)
accuracy_test
library(heatmaply)
install.packages('heatmaply')
heatmaply(Healthcare)





























































































