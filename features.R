setwd("~/Desktop/MSIA/MSIA 421/HW 1")
library(lubridate)
library(dplyr)
booktrain <- read.csv("booktrain.csv", header= T)
orders <- read.csv("orders.csv", header = T)
booktrain$purchase_flag <- ifelse(booktrain$logtarg == 0,0,1)
order <- orders
order$orddate<-as.Date(orders$orddate,"%d%b%Y")
order$year<-year(order$orddate)
order$month<-month(order$orddate)
order$season[order$month %in% c(12,1,2)]<-'Winter'
order$season[order$month %in% c(3,4,5)]<-'Spring'
order$season[order$month %in% c(6,7,8)]<-'Summer'
order$season[order$month %in% c(9,10,11)]<-'Fall'
sea_count <- aggregate(order[c("qty")], by=list(season=order$season, id=orders$id), FUN=sum, na.rm=TRUE)
cat_count <- aggregate(order[c("qty")], by=list(category=order$category, id=order$id), FUN=sum, na.rm=TRUE)
yr_count <- aggregate(order[c("qty")], by=list(year=order$year, id=orders$id), FUN=sum, na.rm=TRUE)

df <- data.frame(unique(order$id))
colnames(df) <- "id"
df$ordtyr <- sapply(df$id,tyr_fun)
df$ordlyr <- sapply(df$id,lyr_fun)
df$ord2yr <- sapply(df$id,ago2yr_fun)
df$ord3yr <- sapply(df$id,ago3yr_fun)
df$ordhist <- sapply(df$id,his_fun)
df$sprord <- sapply(df$id,spr_fun)
df$sumord <- sapply(df$id,sum_fun)
df$fallord <- sapply(df$id,fall_fun)
df$winord <- sapply(df$id,wint_fun)



tyr_fun <- function(id){
  temp<-yr_count[yr_count$id==id,]
  count<-nrow(filter(temp,temp$year==2014))
  return(count)
}

lyr_fun <- function(id){
  temp<-yr_count[yr_count$id==id,]
  count<-nrow(filter(temp,temp$year==2013))
  return(count)
}

ago2yr_fun <- function(id){
  temp<-yr_count[yr_count$id==id,]
  count<-nrow(filter(temp,temp$year==2012))
  return(count)
}

ago3yr_fun <- function(id){
  temp<-yr_count[yr_count$id==id,]
  count<-nrow(filter(temp,temp$year==2011))
  return(count)
}

his_fun <- function(id){
  temp<-yr_count[yr_count$id==id,]
  count<-nrow(filter(temp,temp$year < 2011))
  return(count)
}

spr_fun <- function(id){
  temp<-sea_count[sea_count$id==id & sea_count$category=="Spring",]
  count<-temp$qty
  if(length(count)==0){
    count<-0
  }
  return(count)
}

sum_fun <- function(id){
  temp<-sea_count[sea_count$id==id & sea_count$category=="Summer",]
  count<-temp$qty
  if(length(count)==0){
    count<-0
  }
  return(count)
}
fall_fun <- function(id){
  temp<-sea_count[sea_count$id==id & sea_count$category=="Fall",]
  count<-temp$qty
  if(length(count)==0){
    count<-0
  }
  return(count)
}
wint_fun <- function(id){
  temp<-sea_count[sea_count$id==id & sea_count$category=="Winter",]
  count<-temp$qty
  if(length(count)==0){
    count<-0
  }
  return(count)
}

for (i in 1:length(unique(order$category))){
  df[,10+i]= 0
}

rank <- sort(unique(order$category))

for (i in 1:length(cat_count$category)) {
  df[df$id==cat_count$id[i],10+which(rank==cat_count$category[i])] = cat_count$qty[i]
}




yr_count
sea_count
cat_count


df <- read.csv("df.csv",header=T)
df <- df[,-1]
train <- df[df$id %in% booktrain$id,]
train <- merge(train,booktrain,by="id")
test <- df[!df$id %in% booktrain$id,]

### logistic modeling ###
log_fit<-glm(purchase_flag~.,family = binomial,data=train[,-41])
fit1_pred <- predict(log_fit,newdata = test[,-41],type="response")

### regression modeling ###
reg_fit1 <- lm(logtarg~.,data=train[train$purchase_flag==1,][,-42])
summary(reg_fit1)
fit2_pred <- predict(reg_fit1,newdata=test[,-42])

step_fit <- step(reg_fit1,direction = "both")
summary(step_fit)
fit2_pred2 <- predict(step_fit,newdata = test[,-42])

prediction <- fit1_pred*fit2_pred

result <- data.frame(test[,-2:-42])
colnames(result) <- "id"
result$yhat<-prediction

write.csv(result,file="pred.csv",row.names = FALSE)
write.csv(df,file="df.csv",row.names = FALSE)



