library(lubridate)
library(lme4)
library(dplyr)
book_y <- read.csv('booktrain.csv')
book_x <- read.csv('orders.csv')

book_x$date_new <- as.Date(book_x$orddate,"%d%b%Y")
book_x$month <- month(book_x$date_new)
book_x$year <- year(book_x$date_new)

book_x$season<-book_x$month
book_x$season[book_x$season %in% c(12,1,2)]<-'Winter'
book_x$season[book_x$season %in% c(3,4,5)]<-'Spring'
book_x$season[book_x$season %in% c(6,7,8)]<-'Summer'
book_x$season[book_x$season %in% c(9,10,11)]<-'Fall'

cat_dum <- dummy(factor(book_x$category))
cat_dum$`1` <- ifelse()

book_x<-cbind(book_x,cat_dum)
book_x$p3<-book_x$`3`*book_x$qty*book_x$price
book_x$p5<-book_x$`5`*book_x$qty*book_x$price
book_x$p6<-book_x$`6`*book_x$qty*book_x$price
book_x$p7<-book_x$`7`*book_x$qty*book_x$price
book_x$p8<-book_x$`8`*book_x$qty*book_x$price
book_x$p9<-book_x$`9`*book_x$qty*book_x$price
book_x$p10<-book_x$`10`*book_x$qty*book_x$price
book_x$p12<-book_x$`12`*book_x$qty*book_x$price
book_x$p14<-book_x$`14`*book_x$qty*book_x$price
book_x$p17<-book_x$`17`*book_x$qty*book_x$price
book_x$p19<-book_x$`19`*book_x$qty*book_x$price
book_x$p20<-book_x$`20`*book_x$qty*book_x$price
book_x$p21<-book_x$`21`*book_x$qty*book_x$price
book_x$p22<-book_x$`22`*book_x$qty*book_x$price
book_x$p23<-book_x$`23`*book_x$qty*book_x$price
book_x$p26<-book_x$`26`*book_x$qty*book_x$price
book_x$p27<-book_x$`27`*book_x$qty*book_x$price
book_x$p30<-book_x$`30`*book_x$qty*book_x$price
book_x$p31<-book_x$`31`*book_x$qty*book_x$price
book_x$p35<-book_x$`35`*book_x$qty*book_x$price
book_x$p36<-book_x$`36`*book_x$qty*book_x$price
book_x$p37<-book_x$`37`*book_x$qty*book_x$price
book_x$p38<-book_x$`38`*book_x$qty*book_x$price
book_x$p39<-book_x$`39`*book_x$qty*book_x$price
book_x$p40<-book_x$`40`*book_x$qty*book_x$price
book_x$p41<-book_x$`41`*book_x$qty*book_x$price
book_x$p44<-book_x$`44`*book_x$qty*book_x$price
book_x$p50<-book_x$`50`*book_x$qty*book_x$price
book_x$p99<-book_x$`99`*book_x$qty*book_x$price


book_agg <- book_x %>% group_by(id) %>% summarise(order_num=length(unique(factor(ordnum))),
                                                  avg_price=mean(price),sum_price=sum(price),
                                                  recency=min(as.Date('2014-08-01')-date_new),
                                                  duration=max(as.Date('2014-08-01')-date_new))
cat_agg <- aggregate(x=book_x[,11:68],by=list(book_x$id),FUN = sum)
book_agg2 <- cbind(book_agg,cat_agg[,-1])
#book_agg2$`1`<-book_agg2$order_num -apply(book_agg2[,7:35],1, sum)

book_train <- book_agg2[book_agg2$id %in% book_y$id,]
length(unique(book_train$id))
book_train <- merge(book_train,book_y,by='id')
book_train$purchase <- ifelse(book_train$logtarg!=0,1,0)

book_test <- book_agg2[!book_agg2$id %in% book_y$id,]

# logistic 
log1 <- glm(purchase~.,data = book_train[,-c(1,65)],family = binomial)
summary(log1)
log1_pred <- predict(log1,newdata = book_test,type = 'response')


# regression
lm1 <- lm(logtarg~.,data = book_train[book_train$logtarg!=0,-66])
summary(lm1)
lm1_pred <- predict(lm1,newdata = book_test)

#result <- data.frame(id=book_test$id,yhat=lm1_pred*log1_pred)
#write.csv(result,'/Users/Willie/Desktop/MSiA/Winter 2018/MSiA 421 Data Mining/Book_purchase/result01.csv',row.names = FALSE)


# logistic 2 
#step(log1)
log2 <- glm(formula = purchase ~ order_num + recency + `6` + `8` + `10` + 
              `12` + `19` + `20` + `23` + `30` + `38`, family = binomial, 
            data = book_train[, -c(1, 34)])
summary(log2)
log2_pred <- predict(log2,newdata = book_test,type = 'response')

log3<-glm(formula = purchase ~ order_num + sum_price + recency + duration + `6` + `20` + `21` + `23` + `30` + `35` + `37` + `38` + p8 + 
             p10 + p12 + p21 + p23 + p35 + p37, family = binomial, data = book_train[,-c(1, 65)])
summary(log3)
log3_pred <- predict(log3,newdata = book_test,type = 'response')

# regression 2
step(lm1)
lm2 <- lm(formula = logtarg ~ order_num + recency + `6` + `7` + `9` + 
            `14` + `19` + `20` + `26` + `31` + `35` + `36` + `40` + `50` + 
            `99`, data = book_train[book_train$logtarg != 0, -35])
summary(lm2)
lm2_pred <- predict(lm2,newdata = book_test)

lm3<-lm(formula = logtarg ~ order_num + avg_price + sum_price + 
     duration + `5` + `10` + `19` + `20` + `21` + `23` + `26` + 
     `99` + p3 + p5 + p6 + p8 + p9 + p10 + p20 + p21 + p27 + p40 + 
     p41, data = book_train[book_train$logtarg != 0, -66])

summary(lm3)
lm3_pred <- predict(lm3,newdata = book_test)



result3 <- data.frame(id=book_test$id,yhat=lm3_pred*log3_pred)
write.csv(result3,'result04.csv',row.names = FALSE)

summary(result3$X1)






#ridge

#logistic
library(glmnet)

x=model.matrix(purchase ~.,book_train[, c("order_num","recency","purchase","sum_price",6,20,21,23,30,35,37,38,"p8","p10","p12",
                                          "p21","p23","p35","p37")])[,-1]
y=book_train[, c("order_num","recency","purchase","sum_price",6,20,21,23,30,35,37,38,"p8","p10","p12",
                 "p21","p23","p35","p37")]$purchase

book_test$recency<-as.integer(book_test$recency)

test=as.matrix(book_test[, c("order_num","recency","sum_price",6,20,21,23,30,35,37,38,"p8","p10","p12",
                             "p21","p23","p35","p37")])


grid=10^seq(10,-2,length=100)
ridge.mod=glmnet(x,y,alpha=0,lambda=grid, family = "binomial")
dim(coef(ridge.mod))
ridge.mod$lambda
coef(ridge.mod)[,50]
sqrt(sum(coef(ridge.mod)[-1,50]^2))

ridge.mod$lambda[60]
coef(ridge.mod)[,60]
sqrt(sum(coef(ridge.mod)[-1,60]^2))

predict(ridge.mod,s=50,type="coefficients")


set.seed (1)
cv.out1=cv.glmnet(x,y,alpha=1,family = "binomial")
plot(cv.out1)
bestlam1=cv.out1$lambda.min
bestlam1[1]

out=glmnet(x,y,alpha=1,family = "binomial" )
predict(out,type="coefficients",s=bestlam1,family = "binomial")
final_logis<-predict(out,s=bestlam1,newx=test,type="response")






#linear
x2=model.matrix(logtarg ~.,book_train[, c("order_num","recency","logtarg","avg_price","sum_price","duration",5,10,19,20,21,23,26,99,"p3","p5",
                                          "p6","p8","p9","p10","p20","p21","p27","p40","p41")])[,-1]
y2=book_train[, c("order_num","recency","logtarg","avg_price","sum_price","duration",5,10,19,20,21,23,26,99,"p3","p5",
                  "p6","p8","p9","p10","p20","p21","p27","p40","p41")]$logtarg

grid=10^seq(10,-2,length=100)
ridge.mod2=glmnet(x2,y2,alpha=1,lambda=grid,family= "gaussian")
dim(coef(ridge.mod2))
ridge.mod2$lambda
coef(ridge.mod2)[,50]
sqrt(sum(coef(ridge.mod2)[-1,50]^2))

ridge.mod2$lambda[60]
coef(ridge.mod2)[,60]
sqrt(sum(coef(ridge.mod2)[-1,60]^2))

predict(ridge.mod2,s=50,type="coefficients")


set.seed (1)
cv.out2=cv.glmnet(x2,y2,alpha=1,family= "gaussian")
plot(cv.out2)
bestlam2=cv.out2$lambda.min
bestlam2[1]

out2=glmnet(x2,y2,alpha=1,family= "gaussian")
predict(out2,type="coefficients",s=bestlam2)
final_linear<-predict(out2,s=bestlam2,newx=test)

result3 <- data.frame(id=book_test$id,yhat=final_logis*final_linear)
write.csv(result3,'result05.csv',row.names = FALSE)






