#####데이터 로딩 및 결측치 확인
#파싱에러 해결책 : col_names=T, col_types= cols(PD_BUY_CT=col_number()
rm(list = ls())
install.packages("readr")
install.packages("lubridate")
library(readr)
library(lubridate)

product <- read_csv(file = "Pruduct.csv",
                    locale = locale(date_names = "ko", encoding = "UTF-8"),
                    col_names=T, col_types= cols(PD_BUY_CT=col_number()))
apply(product,2,function(x) sum(is.na(x)))

search1 <- read_csv(file = "Search1.csv",
                    locale = locale(date_names = "ko", encoding = "UTF-8"))
apply(search1,2,function(x) sum(is.na(x)))

search2 <- read_csv(file = "Search2.csv",
                    locale = locale(date_names = "ko", encoding = "UTF-8"),
                    col_names=T, col_types= cols(SEARCH_CNT=col_number()))
apply(search2,2,function(x) sum(is.na(x)))

custom <- read_csv(file = "Custom.csv",
                   locale = locale(date_names = "ko", encoding = "UTF-8"))
apply(custom,2,function(x) sum(is.na(x)))

session <- read_csv(file = "Session.csv",
                    locale = locale(date_names = "ko", encoding = "UTF-8"))
apply(session,2,function(x) sum(is.na(x)))#뷰카운트, 세션 시간 결측치 존재

master <- read_delim(file = "Master.csv", delim =",", escape_double = F,
                     locale = locale(date_names = "ko", encoding = "UTF-8"))
apply(master,2,function(x) sum(is.na(x)))
master[538894,]<-c(725630,"'18년 신상출시' 블라썸 실버 버니-M(31cm) /정상가 55,000원",
                   "완구","여아완구","봉제인형")

#외부데이터
kospi<-read.csv("KOSPI.csv", header = T)
kospi$Date<-ymd(kospi$Date)
weather<-read.table("weather.txt", sep = ",")
names(weather)<-c("region","date","avg_tem","max_tem","min_tem","cloud","rain")
cpi<-read.csv("CPI.csv",header = T)
holiday<-read.csv("holiday.csv",header=T)
holiday$Date<-ymd(holiday$Date)
kos<-read.csv("KOSPI2.csv",header = T)

#session 디바이스를 문자에서 숫자로 바꾸기
device<-c("desktop","mobile","tablet")
for (i in 1:3){
  final$DVC_CTG_NM<-gsub(device[i],i,final$DVC_CTG_NM)
}

#session 결측치를 디바이스별 최빈값으로 바꾸기
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(na.omit(v), uniqv)))]
}
for (i in 1:3){
  session$TOT_PAG_VIEW_CT[is.na(session$TOT_PAG_VIEW_CT)&
                            session$DVC_CTG_NM==i]<-
    getmode(session[session$DVC_CTG_NM==i,]$TOT_PAG_VIEW_CT)
  session$TOT_SESS_HR_V[is.na(session$TOT_SESS_HR_V)&session$DVC_CTG_NM==i]<-
    getmode(session[session$DVC_CTG_NM==i,]$TOT_SESS_HR_V)
}
#session city 결측치 채우기
zone<-table(session[session$CITY_NM=="(not set)",8])
for (i in 1:length(zone)){
  sb<-subset(session, session$ZON_NM==names(zone)[i]&session$CITY_NM!="(not set)")
  session[session$ZON_NM==names(zone)[i]&session$CITY_NM=="(not set)",]$CITY_NM<-
    getmode(sb$CITY_NM)
}

#####데이터 조합
library(sqldf)
pd_cus<-sqldf("select * from product left join custom using (CLNT_ID)")
write.csv(pd_cus,"pd_cus.csv",row.names = F)
pd_cus<-read.csv("pd_cus.csv", header = T)
pd_cus_ses<-sqldf("select * from pd_cus left join session using (CLNT_ID, SESS_ID)")
pd_cus_ses2<-pd_cus_ses[,c(4,7:10,11:16)]
rm(custom, pd_cus, pd_cus_ses, product, session)
master<-master[,-2]
write.csv(pd_cus_ses2,"pd_cus_ses2.csv",row.names = F)
pd_cus_ses_mas<-sqldf("select * from pd_cus_ses2 left join master using (PD_C)")
write.csv(pd_cus_ses_mas,"pd_cus_ses_mas.csv",row.names = F)
pd_cus_ses_mas2<-pd_cus_ses_mas[,-1]
pd_cus_ses_mas2$SESS_DT<-ymd(pd_cus_ses_mas2$SESS_DT)
rm(master, pd_cus_ses2, pd_cus_ses_mas)
kos_close<-kospi[,c(1,5)]
pd_cus_ses_mas_kos<-sqldf("select * from pd_cus_ses_mas2 left join 
                          kos_close on pd_cus_ses_mas2.SESS_DT = kos_close.Date")
pd_cus_ses_mas_kos2<-pd_cus_ses_mas_kos[,-14]
rm(kos_close, kospi, pd_cus_ses_mas2, pd_cus_ses_mas_kos)
final$mm<-month(final$SESS_DT)
fin_cpi<-sqldf("select * from final left join cpi on final.mm=cpi.Month")
write.csv(fin_cpi[,-c(15,16)], "final.csv", row.names = F)
fin_holi<-sqldf("select * from fin_cpi left join 
                holiday on final.SESS_DT=holiday.Date")
write.csv(fin_holi[,-16],"final.csv", row.names = F)
final<-read.csv("final.csv", header = T)

names(final)[15]<-"cp_index"
final$Holiday<-as.factor(final$Holiday)
final$SESS_DT<-ymd(final$SESS_DT)

for (i in 1:3){
  final$TOT_PAG_VIEW_CT[is.na(final$TOT_PAG_VIEW_CT)&final$DVC_CTG_NM==i]<-
    getmode(final[final$DVC_CTG_NM==i,]$TOT_PAG_VIEW_CT)
  final$TOT_SESS_HR_V[is.na(final$TOT_SESS_HR_V)&final$DVC_CTG_NM==i]<-
    getmode(final[final$DVC_CTG_NM==i,]$TOT_SESS_HR_V)
}

#대분류별 예측을 위한 데이터 세분화
library(sqldf)
final2<-sqldf("select * from final order by SESS_DT")

order_dt<-list()
clac1<-unique(final2$CLAC1_NM)
for (k in 1:length(clac1)){
  sub<-subset(final2, final2$CLAC1_NM==clac1[k])
  
  dt<-sqldf("select SESS_DT, sum(PD_BUY_CT) as sum_ct, 
            avg(PD_BUY_AM*PD_BUY_CT) as avg_sales, Close, cp_index, Holiday,
            avg(TOT_PAG_VIEW_CT) as avg_view_ct, avg(TOT_SESS_HR_V) as avg_hr_v,
            sum(case when CLNT_GENDER='F' then 1 end) as female,
            sum(case when CLNT_GENDER='M' then 1 end) as male,
            sum(case when DVC_CTG_NM=2 then 1 end) as mob,
            sum(case when DVC_CTG_NM=1 then 1 end) as desk,
            sum(case when DVC_CTG_NM=3 then 1 end) as tab
            from sub
            group by SESS_DT
            order by SESS_DT")
  dt$Close<-kos$Close[c(1:nrow(dt))]
  dt$Holiday<-as.numeric(dt$Holiday)
  dt[is.na(dt)]<-0
  order_dt[[k]]<-dt
}
names(order_dt)<-clac1

#####일 구매량 예측 회귀
rmse.reg<-data.frame(clac1)
r_square.reg<-data.frame(clac1)
for (i in 1:length(order_dt)){
  max1 = apply(order_dt[[i]][,-c(1,6)],2,max)
  min1 = apply(order_dt[[i]][,-c(1,6)],2,min)
  
  order_dt_scale = scale(order_dt[[i]][,-c(1,6)],center=min1,scale=max1-min1)
  order_dt_scale=cbind(as.data.frame(order_dt_scale),order_dt[[i]]$Holiday)
  
  set.seed(1234)
  tmp<-sample(1:nrow(order_dt_scale),nrow(order_dt_scale)*0.7)
  train1<-order_dt_scale[tmp,]
  test1<-order_dt_scale[-tmp,]
  
  fit.reg<-lm(sum_ct~.,data = train1)
  yhat.reg = predict(fit.reg,newdata=test1,type='response')
  rmse.reg[i,2]<-sqrt(mean((test1$sum_ct-yhat.reg)^2))
  r_square.reg[i,2]<-summary(fit.reg)$r.squared
}
rmse.reg
r_square.reg
names(r_square.reg)<-c("clac1","r.square")
head(r_square.reg[order(r_square.reg$r.square,decreasing = T),])

plot(test1$sum_ct,yhat.reg,xlab='Observed',
     ylab='Predicted',main='Regression',xlim = c(0,1),ylim = c(0,1))
abline(0,1,lty=2)

####회귀나무
library(rpart)

rmse.tree<-data.frame(clac1)
for (i in 1:length(order_dt)){
  max1 = apply(order_dt[[i]][,-c(1,6)],2,max)
  min1 = apply(order_dt[[i]][,-c(1,6)],2,min)
  
  order_dt_scale = scale(order_dt[[i]][,-c(1,6)],center=min1,scale=max1-min1)
  order_dt_scale=cbind(as.data.frame(order_dt_scale),order_dt[[i]]$Holiday)
  
  set.seed(1234)
  tmp<-sample(1:nrow(order_dt_scale),nrow(order_dt_scale)*0.7)
  train2<-order_dt_scale[tmp,]
  test2<-order_dt_scale[-tmp,]
  
  my.control = rpart.control(xval=10, cp=0, minsplit=nrow(train2)*0.05)
  fit.tree = rpart(sum_ct~avg_sales+Close+avg_view_ct+avg_hr_v+female+male+
                     mob+desk+tab,data=train2, method='anova',control=my.control)
  which.min(fit.tree$cp[,4])#에러 최소값 찾기
  ii = which.min(head(fit.tree$cp[,4],10))
  fit.prune.tree = prune(fit.tree,cp=fit.tree$cp[ii,1])
  yhat.tree = predict(fit.prune.tree,newdata=test2,type='vector')
  rmse.tree[i,2]<-sqrt(mean((test2$sum_ct - yhat.tree)^2))
}
rmse.tree

plotcp(fit.tree)
plot(fit.prune.tree,uniform=T,margin=0.1)
text(fit.prune.tree,col='blue',cex=0.7)

plot(test2$sum_ct,yhat.tree,xlab='Observed',
     ylab='Predicted',main='Regression Tree',xlim = c(0,1),ylim = c(0,1))
abline(0,1,lty=2)

##신경망
library(nnet)
test.err<-function(h.size)#에러값 계산하는 함수 제작
{set.seed(1234)
  ir <- nnet(sum_ct ~ ., data=train3, size = h.size,maxit=44,
             decay = 6e-5, trace=F)
  y<- test3$sum_ct
  p<- predict(ir, test3, type = "raw")
  err<-mean(y != p)
  c(h.size, err)
}

rmse.nn<-data.frame(clac1)
for (i in 1:length(order_dt)){
  set.seed(1234)
  max1 = apply(order_dt[[i]][,-c(1,6)],2,max)
  min1 = apply(order_dt[[i]][,-c(1,6)],2,min)
  
  order_dt_scale = scale(order_dt[[i]][,-c(1,6)],center=min1,scale=max1-min1)
  order_dt_scale=cbind(as.data.frame(order_dt_scale),order_dt[[i]]$Holiday)
  
  set.seed(1234)
  tmp<-sample(1:nrow(order_dt_scale),nrow(order_dt_scale)*0.7)
  train3<-order_dt_scale[tmp,]
  test3<-order_dt_scale[-tmp,]
  
  out<-t(sapply(2:19, FUN=test.err))
  node<-out[min(which(out[,2]==min(out[,2]))),2]
  fit.nn<-nnet(sum_ct ~ ., data=train3, size=node, decay=6e-5, maxit=44,trace=F)
  
  yhat.nn = predict(fit.nn,newdata=test3,type='raw')
  rmse.nn[i,2]<-sqrt(mean((test3$sum_ct - yhat.nn)^2))
}
rmse.nn

library(NeuralNetTools)
plotnet(fit.nn)

plot(test3$sum_ct,yhat.nn,xlab='Observed',
     ylab='Predicted',main='Neural Network',xlim = c(0,1),ylim = c(0,1))
abline(0,1,lty=2)

####rmse 최소인 모델(회귀)
boxplot(rmse.reg$V2,rmse.tree$V2,rmse.nn$V2, names = c("reg","tree","nnet"))

row_num<-as.numeric(row.names(head(
  r_square.reg[order(r_square.reg$r.square,decreasing = T),])))
r_square.reg[row_num,]

for (i in row_num){
  m1<-lm(sum_ct~., data = order_dt[[i]][,-1])
  df<-data.frame(round(summary(m1)$coefficients[,4],10))
  df_sub<-subset(df,df[,1]<=0.15)
  print(row.names(df_sub))
}#p값 0.15에 따른 유의한 변수 도출

library(MASS)
for (i in row_num){
  m2<-lm(sum_ct~., data = order_dt[[i]][,-1])
  m_aic<-stepAIC(m2,trace=F)
  print(row.names(summary(m_aic)$coefficients))
}#AIC에 따라 변수 선정

#대분류별 회귀 summary 결과 저장
sumup<-list()
for(i in 1:length(clac1)){
  sub<-subset(final2,final2$CLAC1_NM==clac1[i])
  fin_dt<-sqldf("select SESS_DT, sum(PD_BUY_CT) as sum_ct, 
                avg(PD_BUY_AM*PD_BUY_CT) as avg_sales, Close, cp_index, Holiday,
                avg(TOT_PAG_VIEW_CT) as avg_view_ct, avg(TOT_SESS_HR_V) as avg_hr_v,
                sum(case when CLNT_GENDER='F' then 1 end) as female,
                sum(case when CLNT_GENDER='M' then 1 end) as male,
                sum(case when DVC_CTG_NM=2 then 1 end) as mob,
                sum(case when DVC_CTG_NM=1 then 1 end) as desk,
                sum(case when DVC_CTG_NM=3 then 1 end) as tab
                from sub
                group by SESS_DT
                order by SESS_DT")
  fin_dt[is.na(fin_dt)]<-0
  fin_dt$Close<-kos$Close[c(1:nrow(fin_dt[,-1]))]
  fin_dt$Holiday<-as.numeric(fin_dt$Holiday)
  
  fit.reg<-lm(sum_ct~.,data = fin_dt)
  sumup[[i]]<-summary(fit.reg)
}
names(sumup)<-clac1

#예측 잘 되는 대분류 추출(p값 0.15 이하 6개 이상이고 코스피 0.15이하)
good<-c()
for(i in 1:length(sumup)){
  if (sum(sumup[[i]]$coefficients[,4]<=0.15)>=6 
      & sumup[[i]]$coefficients[4,4]<=0.15){
    good[i]<-paste0(i,"-",names(sumup[i]))
  }
}
unique(good)

#예측 잘 안되는 대분류 추출(p값 0.15 이하 3개 이하이고 코스피 0.7이상)
bad<-c()
for(i in 1:length(sumup)){
  if (sum(sumup[[i]]$coefficients[,4]<=0.15)<=3 
      & sumup[[i]]$coefficients[4,4]>=0.7){
    bad[i]<-paste0(i,"-",names(sumup[i]))
  }
}
unique(bad)

#회귀 적합 확인
fit.reg<-lm(sum_ct~.,data = order_dt[[31]])
f_v<-data.frame(fit.reg$fitted.values)
pp<-cbind(order_dt[[5]][,c(1,2)],f_v)
names(pp)<-c("SESS_DT","sum_ct","fitted_value")

meltdf <- melt(pp,id="SESS_DT")
ggplot(meltdf,aes(x=SESS_DT,y=value,colour=variable,group=variable)) + geom_line()

####트렌드 예측
future<-subset(kos[,c(1,5)],month(kos$Date)>=10)
future$Date<-ymd(future$Date)
future$mm<-month(future$Date)
future2<-sqldf("select * from future left join cpi on future.mm = cpi.Month")
future2<-future2[,c(2,5)]
write.csv(future2,"future2.csv",row.names = F)
future2<-read.csv("future2.csv", header = T)
names(future2)<-c("Close","cp_index","Holiday")

f1<-lm(sum_ct~Close+cp_index+Holiday,order_dt[[37]][-1])
yhat = predict(f1,newdata=future2,type='response')
pred_future<-cbind(future$Date,data.frame(yhat))
names(pred_future)[1]<-c("Date")

ggplot(data = pred_future, aes(x = Date, y = yhat, group = 1)) + 
  geom_line(color = "#FC4E07", size = 1)
