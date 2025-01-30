#install packages
install.packages("aqp")
install.packages("ggplot2")
install.packages("soilDB")
library("ggplot2")
library(aqp)
library(soilDB)

#data handling
sum(is.na(bank.additional.full))
sum(is.na(bank.additional))

#adjust the first row
head(bank.additional)
dim(bank.additional)
length(bank.additional)
df2<-bank.additional[-1,]
head(df2)
colnames(df2)<- c(
  "age", 
  "job",
  "marital",
  "education",
  "default",
  "housing",
  "loan",
  "contact",
  "month",
  "day_of_week",
  "duration",
  "campaign",
  "pdays",
  "previous",
  "poutcome",
  "emp.var.rate",
  "cons.price.idx",
  "cons.conf.idx",
  "euribor3m",
  "nr.employed",
  "y"
)

head(df2)
str(df2)
df2[df2$y == 0,]$y<-"no"

#fac datas
df2$job<-as.factor(df2$job)
df2$marital<-as.factor(df2$marital)
df2$education<-as.factor(df2$education)
df2$default<-as.factor(df2$default)
df2$housing<-as.factor(df2$housing)
df2$loan<-as.factor(df2$loan)
df2$contact<-as.factor(df2$contact)
df2$month<-as.factor(df2$month)
df2$day_of_week<-as.factor(df2$day_of_week)
df2$poutcome<-as.factor(df2$poutcome)
df2$y<-as.factor(df2$y)

#num datas
df2$age<-as.numeric(as.factor(df2$age))
df2$duration<-as.numeric(as.factor(df2$duration))
df2$campaign<-as.numeric(as.factor(df2$campaign))
df2$pdays<-as.numeric(as.factor(df2$pdays))
df2$previous<-as.numeric(as.factor(df2$previous))
df2$emp.var.rate<-as.numeric(as.factor(df2$emp.var.rate))
df2$cons.price.idx<-as.numeric(as.factor(df2$cons.price.idx))
df2$cons.conf.idx<-as.numeric(as.factor(df2$cons.conf.idx))
df2$euribor3m<-as.numeric(as.factor(df2$euribor3m))
df2$nr.employed<-as.numeric(as.factor(df2$nr.employed))

str(df2)

#EDA barchart
c_age<-table(df2$age)
c_age
barplot(c_age, xlab="age_range", ylab="frequency", main ="age" )

c_job<-table(df2$job)
c_job
barplot(c_job, xlab="job_range", ylab="frequency", main ="job" )

c_marital<-table(df2$marital)
c_marital
barplot(c_marital, xlab="marital_range", ylab="frequency", main ="marital" )

c_job<-table(df2$job)
c_job
barplot(c_job, xlab="job_range", ylab="frequency", main ="job" )

c_education<-table(df2$education)
c_education
barplot(c_education, xlab="education_range", ylab="frequency", main ="education" )

c_default<-table(df2$default)
c_default
barplot(c_default, xlab="default_range", ylab="frequency", main ="default" )

c_housing<-table(df2$housing)
c_housing
barplot(c_housing, xlab="housing_range", ylab="frequency", main ="housing" )

c_loan<-table(df2$loan)
c_loan
barplot(c_loan, xlab="loan_range", ylab="frequency", main ="loan" )

c_contact<-table(df2$contact)
c_contact
barplot(c_contact, xlab="contact_range", ylab="frequency", main ="contact" )

c_month<-table(df2$month)
c_month
barplot(c_month, xlab="month_range", ylab="frequency", main ="month" )

c_day_of_week<-table(df2$day_of_week)
c_day_of_week
barplot(c_day_of_week, xlab="day_of_week_range", ylab="frequency", main ="day_of_week" )

c_poutcome<-table(df2$poutcome)
c_poutcome
barplot(c_poutcome, xlab="poutcome_range", ylab="frequency", main ="poutcome" )

c_y<-table(df2$y)
c_y
barplot(c_y, xlab="y_range", ylab="frequency", main ="y" )

#EDA boxchart
boxplot(df2$age, main="Age_Boxplot", ylab="age")
boxplot.stats(df2$age)

boxplot(df2$duration, main="duration_Boxplot", ylab="duration")
boxplot.stats(df2$duration)

boxplot(df2$campaign, main="campaign_Boxplot", ylab="campaign")
boxplot.stats(df2$campaign)

boxplot(df2$emp.var.rate, main="emp.var.rate_Boxplot", ylab="emp.var.rate")
boxplot.stats(df2$emp.var.rate)

boxplot(df2$cons.price.idx, main="cons.price.idx_Boxplot", ylab="cons.price.idx")
boxplot.stats(df2$cons.price.idx)

boxplot(df2$cons.conf.idx, main="cons.conf.idx_Boxplot", ylab="cons.conf.idx")
boxplot.stats(df2$cons.conf.idx)

boxplot(df2$euribor3m, main="euribor3m_Boxplot", ylab="euribor3m")
boxplot.stats(df2$euribor3m)

boxplot(df2$nr.employed, main="nr.employed_Boxplot", ylab="nr.employed")
boxplot.stats(df2$nr.employed)

#GAM 
library(gam)
library(mgcv)

df3=data.frame(df2[,-21], y=as.integer(df2$y)-1)

gam_age<-gam(df3$y~ s(df3$age, bs="cr"),family="binomial",data=df3)
plot(gam_age, se =TRUE,col="darkblue")
summary(gam_age)

gam_duration<-gam(df3$y~ s(df3$duration, bs="cr"),family="binomial",data=df3)
plot(gam_duration, se =TRUE,col="darkblue")

gam_campaign<-gam(df3$y~ s(df3$campaign, bs="cr"),family="binomial",data=df3)
plot(gam_campaign, se =TRUE,col="darkblue")

gam_pdays<-gam(df3$y~ s(df3$pdays, bs="cr"),family="binomial",data=df3)
plot(gam_pdays, se =TRUE,col="darkblue")

#gam_previous<-gam(df3$y~ s(df3$previous, bs="re"),family="binomial",data=df3)
#plot(gam_previous, se =TRUE,col="darkblue")

gam_emp.var.rate<-gam(df3$y~ s(df3$emp.var.rate, bs="cr"),family="binomial",data=df3)
plot(gam_emp.var.rate, se =TRUE,col="darkblue")

gam_cons.price.idx<-gam(df3$y~ s(df3$cons.price.idx, bs="cr"),family="binomial",data=df3)
plot(gam_cons.price.idx, se =TRUE,col="darkblue")

gam_cons.conf.idx<-gam(df3$y~ s(df3$cons.conf.idx, bs="cr"),family="binomial",data=df3)
plot(gam_cons.conf.idx, se =TRUE,col="darkblue")

gam_euribor3m<-gam(df3$y~ s(df3$euribor3m, bs="cr"),family="binomial",data=df3)
plot(gam_euribor3m, se =TRUE,col="darkblue")

gam_nr.employed<-gam(df3$y~ s(df3$nr.employed, bs="cr"),family="binomial",data=df3)
plot(gam_nr.employed, se =TRUE,col="darkblue")

#gam_y<-gam(df3$y~ s(df3$y, bs="re"),family="binomial",data=df3)
#plot(gam_y, se =TRUE,col="darkblue")


#gam fac
gam_job<-gam(df3$y~ s(df3$job, bs="re"),family="binomial",data=df3)
plot(gam_job, se =TRUE,col="darkblue")

gam_marital <-gam(df3$y~ s(df3$marital , bs="re"),family="binomial",data=df3)
plot(gam_marital , se =TRUE,col="darkblue")

gam_education <-gam(df3$y~ s(df3$education , bs="re"),family="binomial",data=df3)
plot(gam_education , se =TRUE,col="darkblue")

gam_default <-gam(df3$y~ s(df3$default , bs="re"),family="binomial",data=df3)
plot(gam_default , se =TRUE,col="darkblue")

gam_housing<-gam(df3$y~ s(df3$housing, bs="re"),family="binomial",data=df3)
plot(gam_housing, se =TRUE,col="darkblue")

gam_loan<-gam(df3$y~ s(df3$loan, bs="re"),family="binomial",data=df3)
plot(gam_loan, se =TRUE,col="darkblue")

gam_contact<-gam(df3$y~ s(df3$contact, bs="re"),family="binomial",data=df3)
plot(gam_contact, se =TRUE,col="darkblue")

gam_month<-gam(df3$y~ s(df3$month, bs="re"),family="binomial",data=df3)
plot(gam_month, se =TRUE,col="darkblue")

gam_day_of_week<-gam(df3$y~ s(df3$day_of_week, bs="re"),family="binomial",data=df3)
plot(gam_day_of_week, se =TRUE,col="darkblue")

gam_poutcome<-gam(df3$y~ s(df3$poutcome, bs="re"),family="binomial",data=df3)
plot(gam_poutcome, se =TRUE,col="darkblue")


#gam_all for predict
install.packages("gplots")
library(ROCR)

data_index<- sample(nrow(df3),nrow(df3)*0.70)
Bank_train<- df3[data_index,]
Bank_test<- df3[-data_index,]

#gam_all<-gam(df3$y~ s(df3$age, bs="cr")+s(df3$duration, bs="cr")+s(df3$campaign, bs="cr")+s(df3$pdays, bs="cr")+s(df3$emp.var.rate, bs="cr")+s(df3$cons.price.idx, bs="cr")+s(df3$cons.conf.idx, bs="cr")+s(df3$euribor3m, bs="cr")+s(df3$nr.employed, bs="cr"),family="binomial",data=Bank_train)
#plot(gam_all, se =TRUE,col="darkblue", pages=1)

#gam_all<-gam(Bank_train$y~ s(Bank_train$y, bs="re")+s(Bank_train$previous, bs="re")+s(Bank_train$marital, bs="re")+s(Bank_train$education, bs="re")+s(Bank_train$default, bs="re")+s(Bank_train$housing, bs="re")+s(Bank_train$loan, bs="re")+s(Bank_train$contact, bs="re")+s(Bank_train$month, bs="re")+s(Bank_train$day_of_week, bs="re")+s(Bank_train$poutcome, bs="re")+s(Bank_train$job, bs="re")+s(Bank_train$age, bs="cr")+s(Bank_train$duration, bs="cr")+s(Bank_train$campaign, bs="cr")+s(Bank_train$pdays, bs="cr")+s(Bank_train$emp.var.rate, bs="cr")+s(Bank_train$cons.price.idx, bs="cr")+s(Bank_train$cons.conf.idx, bs="cr")+s(Bank_train$euribor3m, bs="cr")+s(Bank_train$nr.employed, bs="cr"),family="binomial",data=Bank_train)
#plot(gam_all, se =TRUE,col="darkblue", pages=2)

gam_all<-gam(Bank_train$y~ s(Bank_train$marital, bs="re")+s(Bank_train$education, bs="re")+s(Bank_train$default, bs="re")+s(Bank_train$housing, bs="re")+s(Bank_train$loan, bs="re")+s(Bank_train$contact, bs="re")+s(Bank_train$month, bs="re")+s(Bank_train$day_of_week, bs="re")+s(Bank_train$poutcome, bs="re")+s(Bank_train$job, bs="re")+s(Bank_train$age, bs="cr")+s(Bank_train$duration, bs="cr")+s(Bank_train$campaign, bs="cr")+s(Bank_train$pdays, bs="cr")+s(Bank_train$emp.var.rate, bs="cr")+s(Bank_train$cons.price.idx, bs="cr")+s(Bank_train$cons.conf.idx, bs="cr")+s(Bank_train$euribor3m, bs="cr")+s(Bank_train$nr.employed, bs="cr"),family="binomial",data=Bank_train)
plot(gam_all, se =TRUE,col="darkblue", pages=2)

#deleat re
gam_all<-gam(Bank_train$y~ s(Bank_train$age, bs="cr")+s(Bank_train$duration, bs="cr")+s(Bank_train$campaign, bs="cr")+s(Bank_train$pdays, bs="cr")+s(Bank_train$emp.var.rate, bs="cr")+s(Bank_train$cons.price.idx, bs="cr")+s(Bank_train$cons.conf.idx, bs="cr")+s(Bank_train$euribor3m, bs="cr")+s(Bank_train$nr.employed, bs="cr"),family="binomial",data=Bank_train)
plot(gam_all, se =TRUE,col="darkblue", pages=1)

#ROC and AUC
prob_gam<-predict(gam_all, type="response")
pred_gam<-prediction(c(prob_gam), Bank_train$y)
perf_gam<-performance(pred_gam, "tpr", "fpr")
plot(perf_gam, colorize=TRUE)

unlist(slot(performance(pred_gam, "auc"),"y.values"))

#Tree
install.packages("tree")
install.packages("cowplot")
install.packages("randomForest")
library(tree)
library(cowplot)

set.seed(42)
data_index<- sample(nrow(df3),nrow(df3)*0.70)
Bank_train<- df3[data_index,]
Bank_test<- df3[-data_index,]

tree.df3=tree(y~., Bank_train)
summary(tree.df3)
plot(tree.df3)
text(tree.df3, pretty = 0)

yhat=predict(tree.df3,Bank_test)
mean((yhat -Bank_test$y)^2)

cv.df3 =cv.tree(tree.df3)
plot(cv.df3)

prune.df3 =prune.tree(tree.df3 ,best =6)
plot(prune.df3)

pred.cart=predict(prune.df3,Bank_test)
mean((pred.cart -Bank_test$y)^2)
text(prune.df3,pretty =0)

#randomForest
library(randomForest)

set.seed(261)
bag.df3 =randomForest(y~.,data=Bank_train,mtry=20, ntree=15, importance =TRUE)
bag.df3

pred.bag = predict (bag.df3,newdata =Bank_test)
plot(pred.bag , Bank_test$y)
abline (0,1)

mean(( pred.bag -Bank_test$y)^2)

rf.df3=randomForest(y~., data=Bank_train, mtry=4, ntree=100, importance=TRUE)
pred.rf = predict (rf.df3,newdata =Bank_test)
mean(( pred.rf -Bank_test$y)^2)

importance(rf.df3)
varImpPlot(rf.df3)

#tree ROC and AUC
install.packages("pROC")
library(pROC)
yhat=predict(tree.df3,Bank_test)
pred.cart=predict(prune.df3,Bank_test)
pred.bag = predict (bag.df3,newdata =Bank_test)


tree.roc<-roc(Bank_test$y, pred.cart)
print(tree.roc)
plot(tree.roc,main="ROC Curve",col=2,lwd=2,xlab = "False Positive Rate", ylab = "True positive Rate")

auc(Bank_test$y,pred.cart)

#boosted regression trees
install.packages("gbm")
library (gbm)

set.seed (517)
boost.df3 =gbm(y~.,data=Bank_train, distribution="bernoulli",
                  n.trees =1000, interaction.depth =2)
summary(boost.df3)

pred.boost=predict(boost.df3,newdata =Bank_test, n.trees =1000)
mean((pred.boost-Bank_test$y)^2)

tree.roc<-roc(Bank_test$y, pred.boost)
print(tree.roc)
plot(tree.roc,main="ROC Curve",col=2,lwd=2,xlab = "False Positive Rate", ylab = "True positive Rate")

auc(Bank_test$y,pred.boost)

