library(devtools)
library(woe)
library(ggplot2)
library(caTools)
library(dplyr)
library(nnet)
library(e1071)


train_data = read.csv("C:\\machine_learning\\train.csv",na.strings = '')
test_data = read.csv("C:\\machine_learning\\test_users.csv",na.strings = '')
session=read.csv("C:\\machine_learning\\sessions.csv",na.strings = '')
age=read.csv("C:\\machine_learning\\age_gender_bkts.csv")
View(train_data)
str(train_data)
summary(train_data)
getwd()
# Ignoring date_first_booking column, since there is about 58% of NA values
#also since this entry is for the user who has made first booking, because of which test 
#data has entire date_first_booking column empty 
((sum(is.na(train_data$date_first_booking))*100) /nrow(train_data))
train_data=train_data[,!names(train_data) %in% 'date_first_booking']
test_data=test_data[,!names(test_data) %in% 'date_first_booking']

#missing value imputation for gender
#checking for distribution of gender with respect to country
ggplot(train_data,aes(country_destination))+geom_bar(aes(fill=gender),position = "dodge")

#with the help of the graph, we impute the NA value for gender as per 
#the proportional distribution of gender across the individual dstination country

#train
gender_imputation1=function(country)
{
  n=nrow(train_data[train_data$country_destination==country & train_data$gender=='MALE',])
  d=nrow(train_data[train_data$country_destination==country & train_data$gender %in% c('MALE','FEMALE','OTHER'),])
  p=n/d
  y=nrow(train_data[train_data$country_destination==country & train_data$gender=='-unknown-',])
  sample1=sample(c("MALE","FEMALE"),y,replace = T,prob = c(p,1-p))
  train_data[train_data$country_destination==country & train_data$gender=='-unknown-',]$gender=sample1
  return(train_data)
}

country_name=unique(as.character(train_data$country_destination))
for(j in 1:length(country_name))
{train_data=gender_imputation1(country_name[j])}
train_data$gender=as.factor(train_data$gender)
train_data$gender=factor(train_data$gender) #removing -unknown- gender level
summary(train_data$gender)

#test
p1=nrow(test_data[test_data$gender=='MALE',])/nrow(test_data[test_data$gender!='-unknown-',])
y1=nrow(test_data[test_data$gender=='-unknown-',])
sample2=sample(c("MALE","FEMALE"),y1,replace = T,prob = c(p1,1-p1))
test_data[test_data$gender=='-unknown-',]$gender=sample2
test_data$gender=factor(test_data$gender) #removing -unknown- gender level
summary(test_data)

##missing value imputation for first_affiliate_tracked
ggplot(train_data,aes(country_destination))+geom_bar(aes(fill=first_affiliate_tracked))
##replacing the NA value with untracked
train_data$first_affiliate_tracked[which(is.na(train_data$first_affiliate_tracked))]='untracked'
test_data$first_affiliate_tracked[which(is.na(test_data$first_affiliate_tracked))]='untracked'

##visualising and working on first browser feature
ggplot(train_data,aes(first_browser))+geom_bar(fill='orange')+coord_flip()
#There are more than 50 categories of browsers although the majority use only 5. 
#Therefore, we will club all the minor browsers into the 'Other' category.
train_data$first_browser=as.character(train_data$first_browser)
test_data$first_browser=as.character(test_data$first_browser)
browse=c('Chrome', 'Safari', 'Firefox', 'IE', 'Mobile_Safari','-unknown-')

#train
train_data$first_browser=ifelse(!train_data$first_browser %in% browse,'other',train_data$first_browser)
#test
test_data$first_browser=ifelse(!test_data$first_browser %in% browse,'other',test_data$first_browser)

train_data$first_browser=as.factor(train_data$first_browser)
test_data$first_browser=as.factor(test_data$first_browser)

##combining data in session csv
session=na.omit(session)  
summary(session)
session_new = session %>% group_by(user_id) %>% summarise(tot=sum(secs_elapsed))
View(session_new)

##with the help of train and session_new, create a new column for total sec, for the id
#that are prersent in session_new

#train
t=train_data$id %in% session_new$user_id
train_data['total_sec_elap']=train_data$signup_flow
for (i in 1:nrow(train_data))
{
  print(i)
  if (t[i]==T)
  {
    train_data$total_sec_elap[i]=session_new$tot[as.character(session_new$user_id)==as.character(train_data$id[i])]
  }
  else
  {
    train_data$total_sec_elap[i]=-1
  }
  
}
length(which(train_data['total_sec_elap']!=-1)) #just to validate if imputtaion is done correct
sum(t)

#test
t1=test_data$id %in% session_new$user_id
test_data['total_sec_elap']=test_data$signup_flow
for (i in 1:nrow(test_data))
{
  print(i)
  if (t1[i]==T)
  {
    test_data$total_sec_elap[i]=session_new$tot[as.character(session_new$user_id)==as.character(test_data$id[i])]
  }
  else
  {
    test_data$total_sec_elap[i]=-1
  }
  
}
length(which(test_data['total_sec_elap']!=-1)) #just to validate if imputtaion is done correct
sum(t1)

train_data1=NULL
train_data1=train_data
test_data1=test_data
##Time stamp column is not a correct format , hence extracting month , year and date, and time from it 
## time-stamp 

#train
## split timestamp_first_active in year, month and day
train_data1['first_active_month'] = substring(train_data1$timestamp_first_active, 5, 6)

summary(train_data1$first_active_month)
str(train_data1)

#remove previous timestamp_first_active variable
train_data1=train_data1[,-c(which(colnames(train_data) %in% c('timestamp_first_active')))]

train_data1$first_active_month =as.factor(train_data1$first_active_month)
ggplot(train_data1,aes(train_data1$first_active_month))+geom_bar(aes(fill=train_data1$country_destination))

f=levels(train_data1$first_active_month)
f[1:6]='spring'
f[7:12]='fall'
levels(train_data1$first_active_month)=f

#test

## split timestamp_first_active in year, month and day
test_data1['first_active_month'] = substring(test_data1$timestamp_first_active, 5, 6)

#remove previous timestamp_first_active variable
test_data1=test_data1[,-c(which(colnames(test_data1) %in% c('timestamp_first_active')))]

test_data1$first_active_month =as.factor(test_data1$first_active_month)

f1=levels(test_data1$first_active_month)
f1[1:6]='spring'
f1[7:12]='fall'
levels(test_data1$first_active_month)=f1



##missing value imputation for age
ggplot(train_data1,aes(age))+geom_bar(aes(fill=country_destination))+
  scale_x_continuous(limits = c(0,100),breaks = c(0,10,20,30,50,100,150,200))

df_age<-merge(aggregate(population_in_thousands ~ country_destination + gender, data=age, max), age, all.x=T)
df_age$Population_median<-(as.numeric(substring(df_age$age_bucket,1,2))+(as.numeric(substring(df_age$age_bucket,4,5))))/2
colnames(df_age)<-c("Country","Gender","Population","age","year","Population median")

levels(df_age$Gender)<-c("FEMALE","MALE")
summary(train_data1$age)


## Capping the age column. Less than 15 capped down to 15. More than 100 converted to NA
## and the values which had year we assumed they entered the birth year instead of the age
## so subtracted from the year where the data was collected, ie 2014.
train_data1$age<-ifelse(train_data1$age<15,15,train_data1$age)
train_data1$age<-ifelse(train_data1$age>2007,NA,train_data1$age)
train_data1$age<-ifelse(train_data1$age>1923,(2014-train_data1$age),train_data1$age)
train_data1$age<-ifelse(train_data1$age>100,NA,train_data1$age)

summary(test_data1$age)
table(train_data1$age)

test_data1$age<-ifelse(test_data1$age<15,15,test_data1$age)
test_data1$age<-ifelse(test_data1$age>1999,15,test_data1$age)
test_data1$age<-ifelse(test_data1$age>1919,(2014-test_data1$age),test_data1$age)
test_data1$age<-ifelse(test_data1$age>100,NA,test_data1$age)

table(test_data1$age)
## NA values for the age
## replacing all the NA values for id of each destination country with the mode of every 
## destination country gender wise, from the given age gender bkts.
i<-1
for(i in 1:nrow(train_data1))
{
  if(is.na(train_data1$age)[i] & !train_data1$country_destination[i] %in% c("NDF","other") &
     train_data1$gender[i] %in% c("MALE","FEMALE"))
  {
    train_data1$age[i]<-df_age[as.character(df_age$Country) == as.character(train_data1[i,"country_destination"]) & as.character(df_age$Gender) == as.character(train_data1[i,"gender"]),"Population median"]
    
  }
  
}

summary(train_data1$age)

## For the remaining NA values of the column age corresponding to NDF and other countries,
##imputing them with the mean age of the respective destination country section.

train_data1$age<-ifelse((train_data1$age=="NULL" | is.na(train_data1$age)) & 
                          train_data1$country_destination == "NDF",
                        round(mean(train_data1[train_data1$country_destination == "NDF","age"],na.rm = TRUE)),
                        train_data1$age)

train_data1$age<-ifelse((train_data1$age=="NULL" | is.na(train_data1$age)) & 
                          train_data1$country_destination == "other",
                        round(mean(train_data1[train_data1$country_destination == "other","age"],na.rm = TRUE)),
                        train_data1$age)

train_data1$age<-ifelse((train_data1$age=="NULL" | is.na(train_data1$age)) & 
                          train_data1$gender == "OTHER" & train_data1$country_destination == "US",
                        round(mean(train_data1[train_data1$gender=="OTHER" & train_data1$country_destination == "US","age"],na.rm = TRUE)),
                        train_data1$age)


test_data1$age<-ifelse((test_data1$age=="NULL" | is.na(test_data1$age)),round(mean(test_data1$age,na.rm = TRUE)),test_data1$age)

# cleaning levels
test_data1$signup_method<-ifelse(test_data1$signup_method == "weibo","basic",as.character(test_data1$signup_method))
test_data1$signup_flow<-ifelse(test_data1$signup_flow == 14,15,test_data1$signup_flow)
test_data1$signup_flow<-factor(test_data1$signup_flow)
test_data1$language<-ifelse(test_data1$language == "-unknown-","en",as.character(test_data1$language))
#

write.csv(train_data1,"C:\\Users\\Smitha\\Desktop\\aegis\\ppt\\machine_learning\\doc\\train_clean.csv")
write.csv(test_data1,"C:\\Users\\Smitha\\Desktop\\aegis\\ppt\\machine_learning\\doc\\test_clean.csv")

train_clean=read.csv("C:\\Users\\Smitha\\Desktop\\aegis\\ppt\\machine_learning\\doc\\train_clean.csv")
test_clean=read.csv("C:\\Users\\Smitha\\Desktop\\aegis\\ppt\\machine_learning\\doc\\test_clean.csv")
train_data11=train_data1[,-c(1,2)]
test_data1=test_data1[,-c(1,2)]

train_data11$signup_flow=as.factor(train_data11$signup_flow)
##model building 

nrow(train_data11)
0.7*213451
tr=train_data11[c(1:149415),]
te=train_data11[c(149416:213451),]

##using NAive Bayes
model_naive1=naiveBayes(tr$country_destination~.,data=tr,MaxNWts=1200)
##predict test
result_naive1_te=predict(model_naive1,te,type="class")
#performance
tbl1=table("Predicted"=result_naive1_te, "Actual"=te$country_destination)
tbl1
accuracy <- sum(diag(tbl1)) / sum(tbl1)
accuracy #0.532

#Prediction Kaggle score 0.5759
model_naiveBayes<-naiveBayes(country_destination~.,data = train_data11)
summary(train_data11)
result=predict(model_naiveBayes,test_data1)
table(result)


#logistic regression
model_logistic1=multinom(tr$country_destination~.,data=tr,MaxNWts=1200)
##predict test
result_logistic_te=predict(model_logistic1,te,type="class")
#performance
tbl=table("Predicted"=result_logistic_te, "Actual"=te$country_destination)
tbl 
accuracy <- sum(diag(tbl)) / sum(tbl)
accuracy #0.6061122


##prediction kaggle score 0.66357
model_logistic=multinom(train_data11$country_destination~.,data=train_data11,MaxNWts=1200)
result_logistic=predict(model_logistic,test_data1,type="class")
result_logistic=as.character(result_logistic)
t=as.character(test_data$id)
x=cbind(t,result_logistic)
colnames(x)=c("id","country")
write.csv(x,"C:\\Users\\Smitha\\Desktop\\aegis\\ppt\\machine_learning\\doc\\result1_log.csv",row.names = F)


##using linear discriminant analysis
model_lda1=lda(tr$country_destination~.,data=tr,MaxNWts=1200)
##predict test
result_lda_te=predict(model_lda1,te,type="class")
#performance
tbl1=table("Predicted"=result_lda_te, "Actual"=te$country_destination)
tbl1
accuracy <- sum(diag(tbl1)) / sum(tbl1)
accuracy #0.67

##prediction kaggle score 0.6767
model_logistic=lda(train_data11$country_destination~.,data=train_data11)
result_logistic=predict(model_logistic,test_data1,type="class")
result_logistic=as.character(result_logistic$class)
str(train_data11)
str(test_data1)
table(result_logistic)
t=as.character(test_data$id)
x=cbind(t,result_logistic)
colnames(x)=c("id","country")
write.csv(x,"D://Machine Learning//kaggle//result.csv",row.names = F)
