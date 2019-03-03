library(MASS)
library(rpart)
install.packages("tree")
library(tree)

####회귀나무 + 교차검증
max1 = apply(order_dt[[1]][,-c(1,6)],2,max)
min1 = apply(order_dt[[1]][,-c(1,6)],2,min)

order_dt_scale = scale(order_dt[[1]][,-c(1,6)],center=min1,scale=max1-min1)
order_dt_scale=cbind(as.data.frame(order_dt_scale),order_dt[[1]]$Holiday)

set.seed(1234)
tmp<-sample(1:nrow(order_dt_scale),nrow(order_dt_scale)*0.7)
train2<-order_dt_scale[tmp,]
test2<-order_dt_scale[-tmp,]

fit.tree = tree(sum_ct~avg_sales+Close+avg_view_ct+avg_hr_v+female+male+
                   mob+desk+tab,data=train2)
cv.fit<-cv.tree(fit.tree)
plot(cv.fit$size,cv.fit$dev,type = "b", main = "cv result")

prune.fit<-prune.tree(fit.tree,best=5)
plot(prune.fit)
text(prune.fit,col='blue',cex=0.7)

yhat.tree = predict(prune.fit,newdata=test2,type = "vector")
plot(test2$sum_ct,yhat.tree,xlab='Observed',
     ylab='Predicted',main='Regression Tree',xlim = c(0,1),ylim = c(0,1))
abline(0,1,lty=2)
rmse.tree<-sqrt(mean((test2$sum_ct - yhat.tree)^2))

rmse.tree<-data.frame(clac1)
for(i in 1: length(order_dt)){
  max1 = apply(order_dt[[i]][,-c(1,6)],2,max)
  min1 = apply(order_dt[[i]][,-c(1,6)],2,min)
  
  order_dt_scale = scale(order_dt[[i]][,-c(1,6)],center=min1,scale=max1-min1)
  order_dt_scale=cbind(as.data.frame(order_dt_scale),order_dt[[i]]$Holiday)
  
  set.seed(1234)
  tmp<-sample(1:nrow(order_dt_scale),nrow(order_dt_scale)*0.7)
  train1<-order_dt_scale[tmp,]
  test1<-order_dt_scale[-tmp,]
  
  fit.tree = tree(sum_ct~avg_sales+Close+avg_view_ct+avg_hr_v+female+male+
                    mob+desk+tab,data=train1)
  cv.fit<-cv.tree(fit.tree)
  
  prune.fit<-prune.tree(fit.tree,best=5)
  yhat.tree = predict(prune.fit,newdata=test1,type='vector')
  rmse.tree[i,2]<-sqrt(mean((test1$sum_ct - yhat.tree)^2))
}
rmse.tree

plot(test1$sum_ct,yhat.tree,xlab='Observed',
     ylab='Predicted',main='Regression Tree')
abline(0,1,lty=2)
####배깅
install.packages("randomForest")
library(randomForest)

set.seed(1234)
bag.order = randomForest(sum_ct~avg_sales+Close+avg_view_ct+avg_hr_v+female+male+
                         mob+desk+tab,data=train2, mtry=13, importance=TRUE)#배깅
bag.order = randomForest(sum_ct~avg_sales+Close+avg_view_ct+avg_hr_v+female+male+
                           mob+desk+tab,data=train2, mtry=4, ntree=25)#랜덤숲
#랜덤숲 mtry : 회귀나무 p/3, 분류나무 sqrt(p)
yhat.bag = predict(bag.order,newdata=test2)
plot(test2$sum_ct,yhat.bag,xlab='Observed',
     ylab='Predicted',main='Bagging',xlim = c(0,1),ylim = c(0,1))
abline(0,1,lty=2)
rmse.bag<-sqrt(mean((test2$sum_ct - yhat.bag)^2))

importance(bag.order)#importance=TRUE, 해당 변수가 빠졌을 때의  순수도 변화
varImpPlot(bag.order)
plot(bag.order)

rmse.bag2<-data.frame(clac1)
for (i in 1:length(order_dt)){
  max1 = apply(order_dt[[i]][,-c(1,6)],2,max)
  min1 = apply(order_dt[[i]][,-c(1,6)],2,min)
  
  order_dt_scale = scale(order_dt[[i]][,-c(1,6)],center=min1,scale=max1-min1)
  order_dt_scale=cbind(as.data.frame(order_dt_scale),order_dt[[i]]$Holiday)
  
  set.seed(1234)
  tmp<-sample(1:nrow(order_dt_scale),nrow(order_dt_scale)*0.7)
  train2<-order_dt_scale[tmp,]
  test2<-order_dt_scale[-tmp,]
  
  # bag.order = randomForest(sum_ct~avg_sales+Close+avg_view_ct+avg_hr_v+female+male+
  #                            mob+desk+tab,data=train2, mtry=13, importance=TRUE)
  bag.order = randomForest(sum_ct~avg_sales+Close+avg_view_ct+avg_hr_v+female+male+
                             mob+desk+tab,data=train2, mtry=4, ntree=25)
  yhat.bag = predict(bag.order,newdata=test2)
  rmse.bag2[i,2]<-sqrt(mean((test2$sum_ct - yhat.bag)^2))
}
rmse.bag2

plot(test2$sum_ct,yhat.bag,xlab='Observed',
     ylab='Predicted',main='Bagging')
abline(0,1,lty=2)
####부스팅
install.packages("gbm")
library(gbm)
boost.order = gbm(sum_ct~avg_sales+Close+avg_view_ct+avg_hr_v+female+male+
                mob+desk+tab,data=train2, n.trees = 5000, #shrinkage = 0.002,
                distribution = "gaussian", interaction.depth = 4)
#회귀나무 : gaussian, 분류나무 : bernoulli, n.trees : 반복, shrinkage : 람다
summary(boost.order)#rel.inf 상대적 영향
plot(boost.order,i="female")#주변효과(마지널 이펙트)???
plot(boost.order,i="mob")

yhat.boost = predict(boost.order,newdata=test2,n.trees = 5000)
plot(test2$sum_ct,yhat.boost,xlab='Observed',
     ylab='Predicted',main='Boosting',xlim = c(0,1),ylim = c(0,1))
abline(0,1,lty=2)
rmse.boost<-sqrt(mean((test2$sum_ct - yhat.boost)^2))

rmse.boost<-data.frame(clac1)
for (i in 1:length(order_dt)){
  max1 = apply(order_dt[[i]][,-c(1,6)],2,max)
  min1 = apply(order_dt[[i]][,-c(1,6)],2,min)
  
  order_dt_scale = scale(order_dt[[i]][,-c(1,6)],center=min1,scale=max1-min1)
  order_dt_scale=cbind(as.data.frame(order_dt_scale),order_dt[[i]]$Holiday)
  
  set.seed(1234)
  tmp<-sample(1:nrow(order_dt_scale),nrow(order_dt_scale)*0.7)
  train3<-order_dt_scale[tmp,]
  test3<-order_dt_scale[-tmp,]
  
  boost.order = gbm(sum_ct~avg_sales+Close+avg_view_ct+avg_hr_v+female+male+
                    mob+desk+tab,data=train3, n.trees = 5000, #shrinkage = 0.002,
                    distribution = "gaussian", interaction.depth = 4)
  yhat.boost = predict(boost.order,newdata=test2,n.trees = 5000)
  rmse.boost[i,2]<-sqrt(mean((test2$sum_ct - yhat.boost)^2))
}
rmse.boost

plot(test3$sum_ct,yhat.boost,xlab='Observed',
     ylab='Predicted',main='Boosting',xlim = c(0,1),ylim = c(0,1))
abline(0,1,lty=2)

####비교
boxplot(rmse.tree$V2, rmse.bag$V2, rmse.bag2$V2, 
        rmse.reg$V2, rmse.boost$V2, names = c("tree","bag","forest","reg","boost"))
cor(data.frame(rmse.tree$V2, rmse.bag$V2, rmse.bag2$V2, rmse.reg$V2, rmse.boost$V2))