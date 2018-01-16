library(lubridate)
library(dplyr)
library(ggplot2)
library(ROCR)
library(pscl)
library(InformationValue)#auc

y<-read.csv('booktrain.csv',header = T, stringsAsFactors = F)
x<-read.csv('orders.csv',header = T, stringsAsFactors = F)
View(x)
View(y)
summary(x)

#EDA TO timeseries
x$orddate<-as.Date(x$orddate,
                   "%d%b%Y")
class(x$orddate)
x$year<-year(x$orddate)
x$month<-month(x$orddate)
x$week<-week(x$orddate)
x$weekday<-weekdays(x$orddate)
x$season<-x$month
x$season[x$season %in% c(12,1,2)]<-'Winter'
x$season[x$season %in% c(3,4,5)]<-'Spring'
x$season[x$season %in% c(6,7,8)]<-'Summer'
x$season[x$season %in% c(9,10,11)]<-'Fall'

#x$id<-as.factor(x$id)
#x$category<-as.factor(x$category)
#x$month<-as.integer(x$month)


#Y_EDA
y$targ<-exp(y$logtarg)
y$targ[y$targ==1]<-0

#dimension
dim(x)
length(unique(x$id))
summary(x)

dim(y)
length(unique(y$id))
summary(y)


#train
df<-merge(x,y,by.x="id",by.y="id")
View(df)

dim(df)
length(unique(df$id))
summary(df)

train<-df

#predict
c<-x$id %in% df$id
pred<-x[!c,]
View(pred)
dim(pred)
length(unique(pred$id))
summary(pred)

#targ
ggplot(train[train$targ!=0,], aes(x=targ))+geom_histogram(binwidth = 0.2,aes(fill=..count..))+
  coord_cartesian(xlim = c(0, 150))
dim(train[!(train$targ==0),])
dim(train[(train$targ==0),])

#year
train$year<-as.factor(train$year)
ggplot(train, aes(x=year))+geom_bar()

#season
ggplot(train, aes(x=season))+geom_bar()+scale_x_discrete(limits=c("Spring","Summer","Fall","Winter"))

#week
ggplot(train, aes(x=weekday))+geom_bar()+scale_x_discrete(limits=c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))

#month
train$month<-as.factor(train$month)
ggplot(train, aes(x=month))+geom_bar()

#price
plot(train$price)

#qty
plot(train$qty)

#category
train$category<-as.factor(train$category)
ggplot(train,aes(x=category))+geom_bar(aes(fill=..count..))

#ordernum
plot(train$ordnum)
ggplot(train,aes(x=ordnum))+geom_histogram(binwidth = 50)

#price vs category
train$category<-as.factor(train$category)
class(train$price)
ggplot(train, aes(x=category,y=price))+geom_boxplot()+coord_cartesian(ylim = c(0, 100))

#year, season, month, weekday
ggplot(train, aes(x=year,y=price))+geom_boxplot()+coord_cartesian(ylim = c(0, 25))
ggplot(train, aes(x=season,y=price))+geom_boxplot()+coord_cartesian(ylim = c(0, 15))
ggplot(train, aes(x=month,y=price))+geom_boxplot()+coord_cartesian(ylim = c(0, 15))
ggplot(train, aes(x=weekday,y=price))+geom_boxplot()+coord_cartesian(ylim = c(0, 15))

#price vs targ
ggplot(train,aes(price, targ))+geom_point()+coord_cartesian(xlim = c(0, 300))


#imputation on targ


train$orderspend<-train$price*train$qty
sum<-aggregate(x=train$orderspend,by=list(train$id),FUN=sum)
#purchasedata<-merge(train,sum,by.train="id",by.sum="Group.1",all.train=TRUE)

#classification model
train$if_order[train$targ==0]<-0
train$if_order[train$targ!=0]<-1
summary(train$if_order)


#logistic regression

md1<-glm(if_order~factor(category)+qty+price+factor(year)+factor(month)+factor(week)+factor(weekday)+factor(season),data = train, family = binomial)
summary(md1)
#step(md1)

md2<-glm(formula = if_order ~ factor(category) + price + factor(year) + 
      factor(month) + factor(week) + factor(weekday), family = binomial, 
    data = train)

summary(md2)
coef(md2)
summary(md2)$coef
summary(md2)$coef[,4]

glm.probs=predict(md2,type = "response")
glm.probs[1:10]
contrasts(as.factor(train$if_order))

glm.pred=rep(0,dim(train)[1])
glm.pred[glm.probs>0.2]<-1
sum(glm.pred)

table(glm.pred,train$if_order)
(26+144943)/ dim(train)[1]
mean(glm.pred==train$if_order)

pr <- prediction(glm.probs, train$if_order)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

#plotROC(train$if_order, glm.pred)


#prediction
glm.probs2<-predict(md2,pred, type="response" )
glm.pred2=rep(0,dim(pred)[1])
glm.pred2[glm.probs2>0.2]<-1
sum(glm.pred2)

pred$if_order<-glm.pred2
pred$totalspend<-pred$price*pred$qty
orderlist=as.integer(unique(pred$id[pred$if_order==1]))
pred$if_order[pred$id %in% orderlist]=1
sum(pred$if_order)

pred$offerspend<-pred$totalspend*pred$if_order
sum2<-aggregate(x=pred$offerspend,by=list(pred$id),FUN=sum)
sum2<-as.data.frame(sum2)
colnames(sum2)[1] <- "id"
#test:pred[pred$id==123153,]

output<-data.frame(pred$id,pred$offerspend)
colnames(output)[1] <- "id"
View(output)

output$id<-as.integer(output$id)
sum2$id<-as.integer(sum2$id)

#output2<-merge(output,sum2,by.output=id,by.sum2=Group.1,all.train=TRUE)
#left outer
final_output<-left_join(output, sum2,by='id')
View(final_output)
final_output[final_output$id==123153,]

final2<-final_output[,c(1,3)]
colnames(final2)[2]<-'yhat'

final3<-unique(final2)
final3$yhat[final3$yhat!=0]<-log(final3$yhat[final3$yhat!=0])
final3$yhat<-log(final3$yhat)
View(final3)

write.csv(final3,file="output.csv")


#linear regression
sum(train$if_order)
train_unqiue<-train[train$if_order==1,]


lm1<-lm(targ~factor(category)+qty+price+factor(year)+factor(month)+factor(week)+factor(weekday)+factor(season),data = train_unqiue)
summary(lm1)
step(lm1)

lm2<-lm(formula = targ ~ factor(category) + price + factor(year) + 
     factor(month) + factor(week) + factor(weekday), data = train_unqiue)
summary(lm2)

lm.pred<-predict(lm2,data=train_unqiue)


