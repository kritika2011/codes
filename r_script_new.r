# create gender factor
#my_data$gender <- factor(my_data$gender,
                        # levels = c(0, 1, 2),
                         #labels = c("Male", "Female", "Other"))

# Impute missing age values with mean(age)
#donors$imputed_age <- ifelse(is.na(donors$age), 61.65, donors$age)

# Create missing value indicator for age
#donors$missing_age <- ifelse(is.na(donors$age), 1, 0)

setwd("C:\\Users\\Hp\\Desktop\\Queens_reading_material\\MMAI869_Machine_Learning\\Team_Project")

bgis1<-read.csv("Data_Cleaned_1018_final_colorfeature.csv",as.is=TRUE)
class(bgis1)

r<-bgis1[bgis1$Func_Burdened_Cost==67230.93,]

hist(log(bgis1$Func_Burdened_Cost))
quantile(log(bgis2$Func_Burdened_Cost), c(.01, .05, .10, .25, .50,  .75, .90,.95,0.99,.991,.992,.993,.994,.995,.996,.997,.998,.999,1),na.rm=TRUE)


x<-c("ID_Key",
     "Region_Name_2",
     "Rentable_SQFT",
     "Description_Document",
     "Resolution_Document",
     "Func_Burdened_Cost",
     "ServiceType_Cd",
     "ServiceType_Name",
     "Estimated_Time_Days",
     "off_by_days",
     "Vendor_Key",
     "ServiceProvider_Class",
     "ServiceProvider_Type",
     "WorkOrderSource_Cd",
     "WorkOrderType_Cd",
     "WorkOrderType_Desc1",
     "LeaseInd2",
     "Work_Duration_Days_Rounded",
     "off_by_days_Rounded",
     "newProperty_usage",
     "WorkOrder_Priority_new")


bgis2<- bgis1[,colnames(bgis1)%in%x]

hist(bgis2$Func_Burdened_Cost)


bgis2$cost_bin <- as.integer(cut(bgis2$Func_Burdened_Cost,
                              breaks=quantile(bgis2$Func_Burdened_Cost, seq(0,1, .33333), include.lowest=T)))
table(bgis2$cost_bin)

quantile(bgis2$Func_Burdened_Cost, c(.01, .05, .10, .25, .50,  .75, .90,.95,0.99,.991,.992,.993,.994,.995,.996,.997,.998,.999,1),na.rm=TRUE)

summary(bgis2$cost_bin)

write.csv(bgis2,"test.csv",row.names=FALSE)

bgis2$target <- ifelse(bgis2$Func_Burdened_Cost %in% seq(from=0,to=10,by=1),1,
                       ifelse(bgis2$Func_Burdened_Cost %in% seq(from=10,to=25,by=1),2,
                              ifelse(bgis2$Func_Burdened_Cost %in% seq(from=25,to=50,by=1),3,
                                     ifelse(bgis2$Func_Burdened_Cost %in% seq(from=50,to=75,by=1),4,
                                            ifelse(bgis2$Func_Burdened_Cost %in% seq(from=75,to=100,by=1),5,
                                                   ifelse(bgis2$Func_Burdened_Cost %in% seq(from=100,to=150,by=1),6,
                                                          ifelse(bgis2$Func_Burdened_Cost %in% seq(from=150,to=200,by=1),7,
                                                                 ifelse(bgis2$Func_Burdened_Cost %in% seq(from=200,to=250,by=1),8,
                                                                        ifelse(bgis2$Func_Burdened_Cost %in% seq(from=250,to=300,by=1),9,
                                                                               ifelse(bgis2$Func_Burdened_Cost %in% seq(from=300,to=350,by=1),10,
                                                                                      ifelse(bgis2$Func_Burdened_Cost %in% seq(from=350,to=400,by=1),11,
                                                                                             ifelse(bgis2$Func_Burdened_Cost %in% seq(from=400,to=450,by=1),12,
                                                                                                    ifelse(bgis2$Func_Burdened_Cost %in% seq(from=450,to=500,by=1),13,
                                                                                                           ifelse(bgis2$Func_Burdened_Cost %in% seq(from=500,to=600,by=1),14,
                                                                                                                  ifelse(bgis2$Func_Burdened_Cost %in% seq(from=600,to=700,by=1),15,
                                                                                                                         ifelse(bgis2$Func_Burdened_Cost %in% seq(from=700,to=800,by=1),16,
                                                                                                                                ifelse(bgis2$Func_Burdened_Cost %in% seq(from=800,to=900,by=1),17,
                                                                                                                                       ifelse(bgis2$Func_Burdened_Cost %in% seq(from=900,to=1000,by=1),18,
                                                                                                                                              ifelse(bgis2$Func_Burdened_Cost %in% seq(from=1000,to=1100,by=1),19,
                                                                                                                                                     ifelse(bgis2$Func_Burdened_Cost %in% seq(from=1100,to=1200,by=1),20,
                                                                                                                                                            ifelse(bgis2$Func_Burdened_Cost %in% seq(from=1200,to=1300,by=1),21,
                                                                                                                                                                   ifelse(bgis2$Func_Burdened_Cost %in% seq(from=1300,to=1400,by=1),22,
                                                                                                                                                                          ifelse(bgis2$Func_Burdened_Cost %in% seq(from=1400,to=1500,by=1),23,
                                                                                                                                                                                 ifelse(bgis2$Func_Burdened_Cost %in% seq(from=1500,to=1600,by=1),24,
                                                                                                                                                                                        ifelse(bgis2$Func_Burdened_Cost %in% seq(from=1600,to=1700,by=1),25,
                                                                                                                                                                                               ifelse(bgis2$Func_Burdened_Cost %in% seq(from=1700,to=1800,by=1),26,
                                                                                                                                                                                                      ifelse(bgis2$Func_Burdened_Cost %in% seq(from=1800,to=1900,by=1),27,
                                                                                                                                                                                                             ifelse(bgis2$Func_Burdened_Cost %in% seq(from=1900,to=2000,by=1),28,
                                                                                                                                                                                                                    ifelse(bgis2$Func_Burdened_Cost %in% seq(from=2000,to=2100,by=1),29,
                                                                                                                                                                                                                           ifelse(bgis2$Func_Burdened_Cost %in% seq(from=2100,to=2200,by=1),30,
                                                                                                                                                                                                                                  ifelse(bgis2$Func_Burdened_Cost %in% seq(from=2200,to=2300,by=1),31,
                                                                                                                                                                                                                                         ifelse(bgis2$Func_Burdened_Cost %in% seq(from=2300,to=2400,by=1),32,
                                                                                                                                                                                                                                                ifelse(bgis2$Func_Burdened_Cost %in% seq(from=2400,to=2500,by=1),33,
                                                                                                                                                                                                                                                       ifelse(bgis2$Func_Burdened_Cost %in% seq(from=2500,to=3000,by=1),34,
                                                                                                                                                                                                                                                              ifelse(bgis2$Func_Burdened_Cost %in% seq(from=3000,to=3500,by=1),35,
                                                                                                                                                                                                                                                                     ifelse(bgis2$Func_Burdened_Cost %in% seq(from=3500,to=4000,by=1),36,
                                                                                                                                                                                                                                                                            ifelse(bgis2$Func_Burdened_Cost %in% seq(from=4000,to=4500,by=1),37,
                                                                                                                                                                                                                                                                                   ifelse(bgis2$Func_Burdened_Cost %in% seq(from=4500,to=5000,by=1),38,
                                                                                                                                                                                                                                                                                          ifelse(bgis2$Func_Burdened_Cost %in% seq(from=5000,to=5500,by=1),39,
                                                                                                                                                                                                                                                                                                 ifelse(bgis2$Func_Burdened_Cost %in% seq(from=5500,to=6000,by=1),40,
                                                                                                                                                                                                                                                                                                        ifelse(bgis2$Func_Burdened_Cost %in% seq(from=6000,to=6500,by=1),41,
                                                                                                                                                                                                                                                                                                               ifelse(bgis2$Func_Burdened_Cost %in% seq(from=6500,to=7000,by=1),42,
                                                                                                                                                                                                                                                                                                                      ifelse(bgis2$Func_Burdened_Cost %in% seq(from=7000,to=7500,by=1),43,
                                                                                                                                                                                                                                                                                                                             ifelse(bgis2$Func_Burdened_Cost %in% seq(from=7500,to=8000,by=1),44,
                                                                                                                                                                                                                                                                                                                                    ifelse(bgis2$Func_Burdened_Cost %in% seq(from=8000,to=8500,by=1),45,
                                                                                                                                                                                                                                                                                                                                           ifelse(bgis2$Func_Burdened_Cost %in% seq(from=8500,to=9000,by=1),46,
                                                                                                                                                                                                                                                                                                                                                  ifelse(bgis2$Func_Burdened_Cost %in% seq(from=9000,to=9500,by=1),47,
                                                                                                                                                                                                                                                                                                                                                         ifelse(bgis2$Func_Burdened_Cost %in% seq(from=9500,to=10000,by=1),48,
                                                                                                                                                                                                                                                                                                                                                                ifelse(bgis2$Func_Burdened_Cost %in% seq(from=10000,to=11000,by=1),49,
                                                                                                                                                                                                                                                                                                                                                                       ifelse(bgis2$Func_Burdened_Cost %in% seq(from=11000,to=12000,by=1),50,
                                                                                                                                                                                                                                                                                                                                                                              ifelse(bgis2$Func_Burdened_Cost %in% seq(from=12000,to=13000,by=1),51,
                                                                                                                                                                                                                                                                                                                                                                                     ifelse(bgis2$Func_Burdened_Cost %in% seq(from=13000,to=14000,by=1),52,
                                                                                                                                                                                                                                                                                                                                                                                            ifelse(bgis2$Func_Burdened_Cost %in% seq(from=14000,to=15000,by=1),53,
                                                                                                                                                                                                                                                                                                                                                                                                   ifelse(bgis2$Func_Burdened_Cost %in% seq(from=15000,to=16000,by=1),54,
                                                                                                                                                                                                                                                                                                                                                                                                          ifelse(bgis2$Func_Burdened_Cost %in% seq(from=16000,to=17000,by=1),55,
                                                                                                                                                                                                                                                                                                                                                                                                                 ifelse(bgis2$Func_Burdened_Cost %in% seq(from=17000,to=18000,by=1),56,
                                                                                                                                                                                                                                                                                                                                                                                                                        ifelse(bgis2$Func_Burdened_Cost %in% seq(from=18000,to=19000,by=1),57,
                                                                                                                                                                                                                                                                                                                                                                                                                               ifelse(bgis2$Func_Burdened_Cost %in% seq(from=19000,to=20000,by=1),58,
                                                                                                                                                                                                                                                                                                                                                                                                                                      ifelse(bgis2$Func_Burdened_Cost %in% seq(from=20000,to=25000,by=1),59,
                                                                                                                                                                                                                                                                                                                                                                                                                                             ifelse(bgis2$Func_Burdened_Cost %in% seq(from=25000,to=30000,by=1),60,
                                                                                                                                                                                                                                                                                                                                                                                                                                                    ifelse(bgis2$Func_Burdened_Cost %in% seq(from=30000,to=35000,by=1),61,
                                                                                                                                                                                                                                                                                                                                                                                                                                                           ifelse(bgis2$Func_Burdened_Cost %in% seq(from=35000,to=40000,by=1),62,
                                                                                                                                                                                                                                                                                                                                                                                                                                                                  ifelse(bgis2$Func_Burdened_Cost %in% seq(from=40000,to=,by=1),63,
                                                                                                                                                                                                                                                                                                                                                                                                                                                                         
  
  
  
  
  
  
  
  
  
  
  

ifelse(bgis2$Func_Burdened_Cost %in% seq(from=0,to=10,by=1),1,
                ifelse(bgis2$Func_Burdened_Cost>10 & bgis2$Func_Burdened_Cost<=25,2,
                ifelse(bgis2$Func_Burdened_Cost>25 & bgis2$Func_Burdened_Cost<=50,3,
                ifelse(bgis2$Func_Burdened_Cost>50 & bgis2$Func_Burdened_Cost<=75,4,
                ifelse(bgis2$Func_Burdened_Cost>75 & bgis2$Func_Burdened_Cost<=100,5,
                ifelse(bgis2$Func_Burdened_Cost>100 & bgis2$Func_Burdened_Cost<=150,6,
                ifelse(bgis2$Func_Burdened_Cost>150 & bgis2$Func_Burdened_Cost<=200,7,
                ifelse(bgis2$Func_Burdened_Cost>200 & bgis2$Func_Burdened_Cost<=250,8,
                ifelse(bgis2$Func_Burdened_Cost>250 & bgis2$Func_Burdened_Cost<=300,9,
                ifelse(bgis2$Func_Burdened_Cost>300 & bgis2$Func_Burdened_Cost<=350,10,
                ifelse(bgis2$Func_Burdened_Cost>350 & bgis2$Func_Burdened_Cost<=400,11,
                ifelse(bgis2$Func_Burdened_Cost>400 & bgis2$Func_Burdened_Cost<=450,12,
                ifelse(bgis2$Func_Burdened_Cost>450 & bgis2$Func_Burdened_Cost<=500,13,
                ifelse(bgis2$Func_Burdened_Cost>500 & bgis2$Func_Burdened_Cost<=600,14,
                ifelse(bgis2$Func_Burdened_Cost>600 & bgis2$Func_Burdened_Cost<=700,15,
                ifelse(bgis2$Func_Burdened_Cost>700 & bgis2$Func_Burdened_Cost<=800,16,
                ifelse(bgis2$Func_Burdened_Cost>800 & bgis2$Func_Burdened_Cost<=900,17,
                ifelse(bgis2$Func_Burdened_Cost>900 & bgis2$Func_Burdened_Cost<=1000,18,
                ifelse(bgis2$Func_Burdened_Cost>1000 & bgis2$Func_Burdened_Cost<=1100,19,
                ifelse(bgis2$Func_Burdened_Cost>1100 & bgis2$Func_Burdened_Cost<=1200,20,
                ifelse(bgis2$Func_Burdened_Cost>1200 & bgis2$Func_Burdened_Cost<=1300,21,
                ifelse(bgis2$Func_Burdened_Cost>1300 & bgis2$Func_Burdened_Cost<=1400,22,
                ifelse(bgis2$Func_Burdened_Cost>1400 & bgis2$Func_Burdened_Cost<=1500,23,
                ifelse(bgis2$Func_Burdened_Cost>1500 & bgis2$Func_Burdened_Cost<=1600,24,
                ifelse(bgis2$Func_Burdened_Cost>1600 & bgis2$Func_Burdened_Cost<=1700,25,
                ifelse(bgis2$Func_Burdened_Cost>1700 & bgis2$Func_Burdened_Cost<=1800,26,
                ifelse(bgis2$Func_Burdened_Cost>1800 & bgis2$Func_Burdened_Cost<=1900,27,
                ifelse(bgis2$Func_Burdened_Cost>1900 & bgis2$Func_Burdened_Cost<=2000,28,
                ifelse(bgis2$Func_Burdened_Cost>2000 & bgis2$Func_Burdened_Cost<=2100,29,      
                ifelse(bgis2$Func_Burdened_Cost>2100 & bgis2$Func_Burdened_Cost<=2200,30,
                ifelse(bgis2$Func_Burdened_Cost>2200 & bgis2$Func_Burdened_Cost<=2300,31,
                ifelse(bgis2$Func_Burdened_Cost>2300 & bgis2$Func_Burdened_Cost<=2400,32,
                ifelse(bgis2$Func_Burdened_Cost>2400 & bgis2$Func_Burdened_Cost<=2500,33,
                ifelse(bgis2$Func_Burdened_Cost>2500 & bgis2$Func_Burdened_Cost<=3000,34,
                ifelse(bgis2$Func_Burdened_Cost>3000 & bgis2$Func_Burdened_Cost<=3500,35,
                ifelse(bgis2$Func_Burdened_Cost>3500 & bgis2$Func_Burdened_Cost<=4000,36,
                ifelse(bgis2$Func_Burdened_Cost>4000 & bgis2$Func_Burdened_Cost<=4500,37,
                ifelse(bgis2$Func_Burdened_Cost>4500 & bgis2$Func_Burdened_Cost<=5000,38,
                ifelse(bgis2$Func_Burdened_Cost>5000 & bgis2$Func_Burdened_Cost<=5500,39,
                ifelse(bgis2$Func_Burdened_Cost>5500 & bgis2$Func_Burdened_Cost<=6000,40,
                 ifelse(bgis2$Func_Burdened_Cost>6000 & bgis2$Func_Burdened_Cost<=6500,41,
                 ifelse(bgis2$Func_Burdened_Cost>6500 & bgis2$Func_Burdened_Cost<=7000,42,
                ifelse(bgis2$Func_Burdened_Cost>7000 & bgis2$Func_Burdened_Cost<=7500,43,
                ifelse(bgis2$Func_Burdened_Cost>7500 & bgis2$Func_Burdened_Cost<=8000,44,
                 ifelse(bgis2$Func_Burdened_Cost>8000 & bgis2$Func_Burdened_Cost<=8500,45,
                ifelse(bgis2$Func_Burdened_Cost>8500 & bgis2$Func_Burdened_Cost<=9000,46,
                 ifelse(bgis2$Func_Burdened_Cost>9000 & bgis2$Func_Burdened_Cost<=9500,47,
                 ifelse(bgis2$Func_Burdened_Cost>9500 & bgis2$Func_Burdened_Cost<=10000,48,
                ifelse(bgis2$Func_Burdened_Cost>10000 & bgis2$Func_Burdened_Cost<=11000,49,
                ifelse(bgis2$Func_Burdened_Cost>11000 & bgis2$Func_Burdened_Cost<=12000,50,
                 ifelse(bgis2$Func_Burdened_Cost>12000 & bgis2$Func_Burdened_Cost<=13000,51,
                 ifelse(bgis2$Func_Burdened_Cost>13000 & bgis2$Func_Burdened_Cost<=14000,52,
                 ifelse(bgis2$Func_Burdened_Cost>14000 & bgis2$Func_Burdened_Cost<=15000,53,
                ifelse(bgis2$Func_Burdened_Cost>15000 & bgis2$Func_Burdened_Cost<=16000,54,
                 ifelse(bgis2$Func_Burdened_Cost>17000 & bgis2$Func_Burdened_Cost<=18000,55,
                 ifelse(bgis2$Func_Burdened_Cost>19000 & bgis2$Func_Burdened_Cost<=20000,56,
                 ifelse(bgis2$Func_Burdened_Cost>20000 & bgis2$Func_Burdened_Cost<=25000,57,
                 ifelse(bgis2$Func_Burdened_Cost>25000 & bgis2$Func_Burdened_Cost<=30000,58,
                 ifelse(bgis2$Func_Burdened_Cost>30000 & bgis2$Func_Burdened_Cost<=35000,59,
                 ifelse(bgis2$Func_Burdened_Cost>35000 & bgis2$Func_Burdened_Cost<=40000,60,
                 ifelse(bgis2$Func_Burdened_Cost>40000 & bgis2$Func_Burdened_Cost<=68000,61,0)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))

table(bgis2$target)

bgis3<-bgis2[-which(bgis2$target==0),]
bgis3$target1<-as.factor(bgis3$target)

table(is.na(bgis3$target1))


set.seed(1)
n<- nrow(bgis3)
shuffled<- bgis3[sample(n), ]
train<-shuffled[1:round(0.7*n), ]
test<-shuffled[(round(0.7*n)+1):(round(0.8*n)), ]
validation<-shuffled[(round(0.8*n)+1):n, ]

train_1<-train[, colnames(train)%in% c(
  "Region_Name_2",
  "Rentable_SQFT",
  "ServiceType_Cd",
  "ServiceType_Name",
  "Estimated_Time_Days",
  "off_by_days",
  "Vendor_Key",
  "ServiceProvider_Class",
  "ServiceProvider_Type",
  "WorkOrderSource_Cd",
  "WorkOrderType_Cd",
  "WorkOrderType_Desc1",
  "LeaseInd2",
  "Work_Duration_Days_Rounded",
  "off_by_days_Rounded",
  "newProperty_usage",
  "WorkOrder_Priority_new",
  "target1")]



test_1<-test[, colnames(test)%in% c(
  "Region_Name_2",
  "Rentable_SQFT",
  "ServiceType_Cd",
  "ServiceType_Name",
  "Estimated_Time_Days",
  "off_by_days",
  "Vendor_Key",
  "ServiceProvider_Class",
  "ServiceProvider_Type",
  "WorkOrderSource_Cd",
  "WorkOrderType_Cd",
  "WorkOrderType_Desc1",
  "LeaseInd2",
  "Work_Duration_Days_Rounded",
  "off_by_days_Rounded",
  "newProperty_usage",
  "WorkOrder_Priority_new",
  "target1")]

validation_1<-validation[, colnames(validation)%in% c(
  "Region_Name_2",
  "Rentable_SQFT",
  "ServiceType_Cd",
  "ServiceType_Name",
  "Estimated_Time_Days",
  "off_by_days",
  "Vendor_Key",
  "ServiceProvider_Class",
  "ServiceProvider_Type",
  "WorkOrderSource_Cd",
  "WorkOrderType_Cd",
  "WorkOrderType_Desc1",
  "LeaseInd2",
  "Work_Duration_Days_Rounded",
  "off_by_days_Rounded",
  "newProperty_usage",
  "WorkOrder_Priority_new",
  "target1")]
install.packages("randomForest")
install.packages("gbm")
library(randomForest)
library(gbm)


train_1$Region_Name_2<-as.factor(train_1$Region_Name_2)
train_1$ServiceType_Cd<-as.factor(train_1$ServiceType_Cd)
train_1$ServiceType_Name<-as.factor(train_1$ServiceType_Name)
train_1$ServiceProvider_Class<-as.factor(train_1$ServiceProvider_Class)
train_1$ServiceProvider_Type<-as.factor(train_1$ServiceProvider_Type)
train_1$WorkOrderSource_Cd<-as.factor(train_1$WorkOrderSource_Cd)
train_1$WorkOrderType_Cd<-as.factor(train_1$WorkOrderType_Cd)
train_1$WorkOrderType_Desc1<-as.factor(train_1$WorkOrderType_Desc1)
train_1$newProperty_usage<-as.factor(train_1$newProperty_usage)
train_1$WorkOrder_Priority_new<-as.factor(train_1$WorkOrder_Priority_new)

fit<-randomForest(target1~.,data=train_1,ntree=100, mtry=10,replace=TRUE,importance=TRUE)
summary(fit)
y<-as.data.frame(fit$importance)
class(fit$importance)
set.seed(10)
fit_gbm<-gbm(formula=target1~.,data=train_1,bag.fraction=0.6, n.trees=100, shrinkage=0.01,interaction.depth=4,
             n.minobsinnode=1000,verbose=TRUE)
summary(fit_gbm)
save.image(file="ML.RData")


table(is.na(bgis2$Region_Name_2))
table(is.na(bgis2$Rentable_SQFT))
table(is.na(bgis2$ServiceType_Cd))
table(is.na(bgis2$ServiceType_Name))
table(is.na(bgis2$Estimated_Time_Days))
table(is.na(bgis2$off_by_days))
table(is.na(bgis2$Vendor_Key))
table(is.na(bgis2$ServiceProvider_Class))
table(is.na(bgis2$ServiceProvider_Type))
table(is.na(bgis2$WorkOrderSource_Cd))
table(is.na(bgis2$WorkOrderType_Cd))
table(is.na(bgis2$WorkOrderType_Desc1))
table(is.na(bgis2$LeaseInd2))
table(is.na(bgis2$Work_Duration_Days_Rounded))
table(is.na(bgis2$off_by_days_Rounded))
table(is.na(bgis2$newProperty_usage))
table(is.na(bgis2$cost_bin))


#sapply(train, function(x) sum(is.na(x))/length(x))*100
#table(is.na(test))

class(train_1$Region_Name_2)
class(train_1$Rentable_SQFT)
class(train_1$ServiceType_Cd)
class(train_1$ServiceType_Name)
class(train_1$Estimated_Time_Days)
class(train_1$off_by_days)
class(train_1$Vendor_Key)
class(train_1$ServiceProvider_Class)
class(train_1$ServiceProvider_Type)
class(train_1$WorkOrderSource_Cd)
class(train_1$WorkOrderType_Cd)
class(train_1$WorkOrderType_Desc1)
class(train_1$LeaseInd2)
class(train_1$Work_Duration_Days_Rounded)
class(train_1$off_by_days_Rounded)
class(train_1$newProperty_usage)


###multinomial logistic regression

install.packages("nnet")
library(nnet)
train_1$target1 <- relevel(train_1$target1, ref = 1)
fit_logit <- multinom(target1 ~ ., data = train_1)
summary(fit_logit)

z <- summary(fit_logit)$coefficients/summary(fit_logit)$standard.errors
z

exp(coef(fit_logit))

head(pp <- fitted(fit_logit))
