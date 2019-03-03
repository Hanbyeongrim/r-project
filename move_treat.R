##불러오기
output_total_final<-read.csv("output_total_final3.csv",header = T,na.strings=c("NA", ""))
summary(output_total_final)
str(output_total_final)
write.csv(output_total_final,"C:/Users/A/Documents/빅청 영화프로젝트0/output_total_final3.csv",row.names=F)
apply(output_total_final,2,function(x) sum(is.na(x)))#결측치 갯수 세기
out<-output_total_final[,-c(22:26)]
apply(out,2,function(x) sum(is.na(x)))
##불필요한 변수 제거(시리얼, 제목, 감독 배우 이름)
output<-output_total_final[,-c(1,2,5,7:10)]
str(output)
summary(output)
apply(output,2,function(x) sum(is.na(x)))
##액터에 번호 붙이기 위한 통합작업 엑셀에서 한는게 좋을 듯(vlookup, 값만 붙이기)
actor1<-na.omit(as.data.frame(output$actor_1_name))
names(actor1)<-c("actor")
actor2<-na.omit(as.data.frame(output$actor_2_name))
names(actor2)<-c("actor")
actor3<-na.omit(as.data.frame(output$actor_3_name))
names(actor3)<-c("actor")
actor<-unique(rbind(actor1,actor2,actor3))
actor1<-output_total_final[,c(8,11,14)]
names(actor1)<-c("actor","starmeter","like")
actor2<-output_total_final[,c(9,12,15)]
names(actor2)<-c("actor","starmeter","like")
actor3<-output_total_final[,c(10,13,16)]
names(actor3)<-c("actor","starmeter","like")
actor_all<-rbind(actor1,actor2,actor3)
write.csv(actor_all,"C:/Users/A/Documents/빅청 영화프로젝트0/actor_all.csv",row.names=F)
##장르 db
genres<-unique(as.data.frame(output$genres))
write.csv(genres,"C:/Users/A/Documents/빅청 영화프로젝트0/genres.csv",row.names=F)
##감독 db
director<-unique(as.data.frame(output$director_name))
names(output_total_final)
director_all<-output_total_final[,c(7,17)]
summary(director_all)
write.csv(director_all,"C:/Users/A/Documents/빅청 영화프로젝트0/director_number.csv",row.names=F)
##factor를 numeric으로 바꾸기(이유는 모르겠는데 형변환 한번돌리면 오류, 두번돌리면 안남)
output2<-output
output2$year<-as.numeric(as.character(output2$year))
output2$duration<-as.numeric(as.character(output2$duration,na.rm=T))
output2$actor_1_starmeter<-as.numeric(output2$actor_1_starmeter,stringsAsFactors = FALSE)
output2$actor_2_starmeter<-as.numeric(output2$actor_2_starmeter,stringsAsFactors = FALSE)
output2$actor_3_starmeter<-as.numeric(output2$actor_3_starmeter,stringsAsFactors = FALSE)
output2$actor_1_likes<-as.numeric(as.character(output2$actor_1_likes))
output2$actor_2_likes<-as.numeric(as.character(output2$actor_2_likes))
output2$actor_3_likes<-as.numeric(as.character(output2$actor_3_likes))
output2$director_likes<-as.numeric(as.character(output2$director_likes))
output2$ratings<-as.numeric(as.character(output2$ratings))
output2$ratingCounts<-as.numeric(as.character(output2$ratingCounts))
output2$movie_likes<-as.numeric(as.character(output2$movie_likes))
output2$writer_likes<-as.numeric(as.character(output2$writer_likes))
output2$director_num<-as.numeric(as.character(output2$director_num))
output2$director_num<-as.numeric(as.character(output2$director_num))

summary(output2)
sum(output$year!=output2$year,na.rm=T)
str(output2)

##년도 자르기
output3<-subset(output2,output2$year<=2017 & output2$year>1950)

##레이팅 여부로 분리
output_y<-subset(output3,output3$ratings!="NA")
output_n<-subset(output3,is.na(output3$ratings))

##결측치 대체(등록이 안되서인지, 인기가 없어서인지 일일히 구분 할 수 없음)
apply(output_y,2,function(x) sum(is.na(x)))
output_y$duration[is.na(output_y$duration)]<-median(output_y$duration, na.rm = T)
output_y$budget[is.na(output_y$budget)]<-median(output_y$budget, na.rm = T)
output_y$actor_1_starmeter[is.na(output_y$actor_1_starmeter)]<-median(output_y$actor_1_starmeter, na.rm = T)
output_y$actor_2_starmeter[is.na(output_y$actor_2_starmeter)]<-median(output_y$actor_2_starmeter, na.rm = T)
output_y$actor_3_starmeter[is.na(output_y$actor_3_starmeter)]<-median(output_y$actor_3_starmeter, na.rm = T)
output_y$actor_1_likes[is.na(output_y$actor_1_likes)]<-median(output_y$actor_1_likes, na.rm = T)
output_y$actor_2_likes[is.na(output_y$actor_2_likes)]<-median(output_y$actor_2_likes, na.rm = T)
output_y$actor_3_likes[is.na(output_y$actor_3_likes)]<-median(output_y$actor_3_likes, na.rm = T)
output_y$director_likes[is.na(output_y$director_likes)]<-median(output_y$director_likes, na.rm = T)
output_y$ratingCounts[is.na(output_y$ratingCounts)]<-median(output_y$ratingCounts, na.rm = T)
output_y$movie_likes[is.na(output_y$movie_likes)]<-median(output_y$movie_likes, na.rm = T)
output_y$writer_likes[is.na(output_y$writer_likes)]<-median(output_y$writer_likes, na.rm = T)
output_y$act1_num[is.na(output_y$act1_num)]<-0
output_y$act2_num[is.na(output_y$act2_num)]<-0
output_y$act3_num[is.na(output_y$act3_num)]<-0
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}#최빈값 함수는 없어서 만들어야 함
output_y$director_num[is.na(output_y$director_num)]<-getmode(output_y$director_num)
output_y$genre_num[is.na(output_y$genre_num)]<-getmode(output_y$genre_num)

output_y$act1_num<-as.factor(output_y$act1_num)
output_y$act2_num<-as.factor(output_y$act2_num)
output_y$act3_num<-as.factor(output_y$act3_num)
output_y$genre_num<-as.factor(output_y$genre_num)
output_y$director_num<-as.factor(output_y$director_num)
summary(output_y)
str(output_y)

write.csv(output_y,"C:/Users/A/Documents/빅청 영화프로젝트0/output_y.csv",row.names=F)

##train, test 분리
output_y2<-output_y[,-c(15:18)]
names(output_y2)
set.seed(1234)#set.seed 난수 시작점. 샘플링 할때마다 같이 돌려야 함
a<-sample(1:nrow(output_y),nrow(output_y)*0.7)
output_train<-output_y2[a,]
output_test<-output_y2[-a,]

#선형회귀(변수 선택 3가지 모두 같은 변수가 선택됨)
fit.null<-lm(ratings~1,data = output_train)
fit.reg<-lm(ratings~.,data = output_train)
summary(fit.reg)
fit.fwd=step(fit.null, scope=list(lower=fit.null, upper=fit.reg), direction="forward")
summary(fit.fwd) 
fit.bwd=step(fit.reg, scope=list(lower=fit.null, upper=fit.reg), direction="backward")
summary(fit.bwd)
fit.step.reg = step(fit.reg,direction = 'both', trace = FALSE)
summary(fit.step.reg)
fit.step.reg$anova #지워진 변수
yhat.reg = predict(fit.step.reg,newdata=output_test,type='response')
rmse.reg<-sqrt(mean((output_test$ratings-yhat.reg)^2))
rmse.reg

##선형회귀에서 선택된 변수만 추출
names(output_train2)
output_train2<-output_train[,c(1,2,6,7,11,12,15)]
output_test2<-output_test[,c(1,2,6,7,11,12,15)]

##회귀나무
library(rpart)
my.control = rpart.control(xval=10, cp=0, minsplit=nrow(output_train2)*0.05)
fit.tree = rpart(ratings~year+duration+actor_3_starmeter+
                   actor_1_likes+ratingCounts+genre_num,
                 data=output_train2, method='anova',control=my.control)
fit.tree
which.min(fit.tree$cp[,4])#에러 최소값 찾
ii = which.min(fit.tree$cp[,4])

fit.prune.tree = prune(fit.tree,cp=fit.tree$cp[ii,1])
plotcp(fit.tree)

plot(fit.prune.tree,uniform=T,margin=0.1)
text(fit.prune.tree,col='blue',cex=0.7)
yhat.tree = predict(fit.prune.tree,newdata=output_test2,type='vector')
rmse.tree<-sqrt(mean((output_test2$ratings - yhat.tree)^2))
rmse.tree

##신경망
output_y3 <- output_y2[,c(1,2,6,7,11,12,15)]
for(i in 1:ncol(output_y3))if(!is.numeric(output_y3[,i]))
  output_y3[,i]=as.numeric(output_y2[,i])
str(output_y3)
max1 = apply(output_y3,2,max)
min1 = apply(output_y3,2,min)

output_y3_scale = scale(output_y3,center=min1,scale=max1-min1)
output_y3_scale=as.data.frame(output_y3_scale)

set.seed(1234)
i=sample(1:nrow(output_y3_scale),round(nrow(output_y3_scale)*0.7))
output_y3.train = output_y3_scale[i,]
output_y3.test = output_y3_scale[-i,]

vname = names(output_y3.train)
form = as.formula(paste('ratings~',paste(vname[!vname %in% "ratings"],collapse='+')))
form
library(neuralnet)
fit.nn = neuralnet(form,data=output_y3.train,hidden=c(3,2),linear.output=T)
plot(fit.nn)

pred=compute(fit.nn,output_y3.test[,-5])
yhat.nn = pred$net.result*(max(output_y3
                               $ratings)-min(output_y3$ratings))+min(output_y3$ratings)
output_y3.test$ratings=output_y3.test$
  ratings*(max(output_y3$ratings)-min(output_y3$ratings))+min(output_y3$ratings)
rmse.nn<-sqrt(mean((output_y3.test$ratings-yhat.nn)^2))
rmse.nn

##실제값 예측값 산점도
plot(output_test$ratings,yhat.reg,xlab='Observed',ylab='Predicted',main='Regression',xlim = c(0,10),ylim = c(0,10))
abline(0,1,lty=2)

plot(output_test2$ratings,yhat.tree,xlab='Observed',ylab='Predicted',main='Regression Tree',xlim = c(0,10),ylim = c(0,10))
abline(0,1,lty=2)

plot(output_y3.test$ratings,yhat.nn,xlab='Observed',ylab='Predicted',main='Neural Network',xlim = c(0,10),ylim = c(0,10))
abline(0,1,lty=2)

##잔차 박스 도표
boxplot(output_test$ratings-yhat.reg, output_test2$ratings-yhat.tree,
        output_y3.test$ratings-yhat.nn, names = c("reg","tree","nn"))
summary(output_test$ratings-yhat.reg)
summary(output_test2$ratings-yhat.tree)
summary(output_y3.test$ratings-yhat.nn)
c(rmse.reg, rmse.tree, rmse.nn)
