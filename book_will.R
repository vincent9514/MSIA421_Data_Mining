library(lubridate)
library(lme4)
book_y <- read.csv('/Users/Willie/Desktop/MSiA/Winter 2018/MSiA 421 Data Mining/Book_purchase/booktrain.csv')
book_x <- read_csv('/Users/Willie/Desktop/MSiA/Winter 2018/MSiA 421 Data Mining/Book_purchase/orders.csv')

book_x$date_new <- as.Date(book_x$orddate,"%d%b%Y")
book_x$month <- month(book_x$date_new)
book_x$year <- year(book_x$date_new)

book_x$season<-book_x$month
book_x$season[book_x$season %in% c(12,1,2)]<-'Winter'
book_x$season[book_x$season %in% c(3,4,5)]<-'Spring'
book_x$season[book_x$season %in% c(6,7,8)]<-'Summer'
book_x$season[book_x$season %in% c(9,10,11)]<-'Fall'

cat_dum <- dummy(factor(book_x$category))
book_x<-cbind(book_x,cat_dum)

book_agg <- book_x %>% group_by(id) %>% summarise(order_num=length(unique(factor(ordnum))),avg_price=mean(price),recency=min(as.Date('2014-08-01')-date_new))
cat_agg <- aggregate(x=book_x[,11:39],by=list(book_x$id),FUN = sum)
book_agg2 <- cbind(book_agg,cat_agg[,-1])

book_train <- book_agg2[book_agg2$id %in% book_y$id,]
length(unique(book_train$id))
book_train <- merge(book_train,book_y,by='id')
book_train$purchase <- ifelse(book_train$logtarg!=0,1,0)

book_test <- book_agg2[!book_agg2$id %in% book_y$id,]

# logistic 
log1 <- glm(purchase~.,data = book_train[,-c(1,34)],family = binomial)
summary(log1)
log1_pred <- predict(log1,newdata = book_test,type = 'response')


# regression
lm1 <- lm(logtarg~.,data = book_train[book_train$logtarg!=0,-35])
summary(lm1)
lm1_pred <- predict(lm1,newdata = book_test)

result <- data.frame(id=book_test$id,yhat=lm1_pred*log1_pred)
write.csv(result,'/Users/Willie/Desktop/MSiA/Winter 2018/MSiA 421 Data Mining/Book_purchase/result01.csv',row.names = FALSE)

