################# 데이터 로드 및 DLL / API 분리 #################
setwd("C:/Users/pccom/Desktop/데이터 마이닝")
api<-read.csv("API Frequence.csv",header=T)
api1<-api[,3294:7538] #API 소스만 분석하기 위해서 변수 분리

################# 카테고리별 분류 #################
ct1<-api1[api1$Category=="Audio_Player",]
ct2<-api1[api1$Category=="Browser",]
ct3<-api1[api1$Category=="CD_Writer",]
ct4<-api1[api1$Category=="FTP",]
ct5<-api1[api1$Category=="Image_Viewer",]
ct6<-api1[api1$Category=="Messenger",]
ct7<-api1[api1$Category=="Text_Editor",]
ct8<-api1[api1$Category=="Video_Player",]
ct9<-api1[api1$Category=="Zip",]


################# train 데이터 0.7 test 데이터 0.3 #################
set.seed(1234)
a1<-sample(1:nrow(ct1),nrow(ct1)*0.7)
ct1train<-ct1[a1,]
a11<-ct1[-a1,]
a2<-sample(1:nrow(ct2),nrow(ct2)*0.7)
ct2train<-ct2[a2,]
a22<-ct2[-a2,]
a3<-sample(1:nrow(ct3),nrow(ct3)*0.7)
ct3train<-ct3[a3,]
a33<-ct3[-a3,]
a4<-sample(1:nrow(ct4),nrow(ct4)*0.7)
ct4train<-ct4[a4,]
a44<-ct4[-a4,]
a5<-sample(1:nrow(ct5),nrow(ct5)*0.7)
ct5train<-ct5[a5,]
a55<-ct5[-a5,]
a6<-sample(1:nrow(ct6),nrow(ct6)*0.7)
ct6train<-ct6[a6,]
a66<-ct6[-a6,]
a7<-sample(1:nrow(ct7),nrow(ct7)*0.7)
ct7train<-ct7[a7,]
a77<-ct7[-a7,]
a8<-sample(1:nrow(ct8),nrow(ct8)*0.7)
ct8train<-ct8[a8,]
a88<-ct8[-a8,]
a9<-sample(1:nrow(ct9),nrow(ct9)*0.7)
ct9train<-ct9[a9,]
a99<-ct9[-a9,]
ct_train<-rbind(ct1train,ct2train,ct3train,ct4train,ct5train,ct6train,ct7train,ct8train,ct9train)
ct_test<-rbind(a11,a22,a33,a44,a55,a66,a77,a88,a99)


################# 디시전트리(변수선택용) #################
set.seed(1234)
install.packages("tree")
library(tree)
fit5<-tree(Category~.,ct_train,minsize=2,mindev=0)
train_pred2 = predict(fit5, ct_test, type="class") 
k<-table(ct_test$Category,train_pred2)
sum(k[upper.tri(k)|lower.tri(k)])/sum(k) ##오분류율 계산기

cvgr=cv.tree(fit5, K=10,method="misclass") 
windows()
plot(cvgr)
# cv.tree를 이용해 Training set을 이용하여 Validation set 역할을 하게 만듦
# k= 10등분 함을 의미한다
# 최적 노드 수가 100개로 나타남-> best를 100으로 두고 가지치기를 시행 함
cvgr

######################
k<-prune.tree(fit5,newdata=ct_test, best=100)
aa<-k$frame$var[k$frame$var!="<leaf>"]
out_pred=predict(k, ct_test, type="class")
k1<-table(ct_test$Category, out_pred)

sum(k1[upper.tri(k1)|lower.tri(k1)])/sum(k1) ##오분류율 계산기

api2<-api1[c(aa,4245)]  ### 뽑힌 변수들과 카테고리를 묶어서 api2 생성
apply(api2[-100],2,sum) ## 뽑힌 변수들이 0이 없다 


################# PCA #################
prin2=princomp(api2[-100],cor=T)  #사용하는 단위가 다를 수 있으므로 표준화 cor=t 사용
summary(prin2)  #summary 결과 comp.40까지 사용 (분산 90%)
prin2$loadings
summary(prin2) #분산의 결과
screeplot(prin2,npcs=100,type="lines")

##################################################

api3<-cbind(prin2$scores[,1:40],api2[100])   # pca 를 통해 40개와 카테고리로 api3 생성

################# PCA 한 값으로 다시 0.7,  0.3 나누기 #################
ct1<-api3[api1$Category=="Audio_Player",]
ct2<-api3[api1$Category=="Browser",]
ct3<-api3[api1$Category=="CD_Writer",]
ct4<-api3[api1$Category=="FTP",]
ct5<-api3[api1$Category=="Image_Viewer",]
ct6<-api3[api1$Category=="Messenger",]
ct7<-api3[api1$Category=="Text_Editor",]
ct8<-api3[api1$Category=="Video_Player",]
ct9<-api3[api1$Category=="Zip",]

set.seed(1234)
a1<-sample(1:nrow(ct1),nrow(ct1)*0.7)
ct1train<-ct1[a1,]
a11<-ct1[-a1,]
a2<-sample(1:nrow(ct2),nrow(ct2)*0.7)
ct2train<-ct2[a2,]
a22<-ct2[-a2,]
a3<-sample(1:nrow(ct3),nrow(ct3)*0.7)
ct3train<-ct3[a3,]
a33<-ct3[-a3,]
a4<-sample(1:nrow(ct4),nrow(ct4)*0.7)
ct4train<-ct4[a4,]
a44<-ct4[-a4,]
a5<-sample(1:nrow(ct5),nrow(ct5)*0.7)
ct5train<-ct5[a5,]
a55<-ct5[-a5,]
a6<-sample(1:nrow(ct6),nrow(ct6)*0.7)
ct6train<-ct6[a6,]
a66<-ct6[-a6,]
a7<-sample(1:nrow(ct7),nrow(ct7)*0.7)
ct7train<-ct7[a7,]
a77<-ct7[-a7,]
a8<-sample(1:nrow(ct8),nrow(ct8)*0.7)
ct8train<-ct8[a8,]
a88<-ct8[-a8,]
a9<-sample(1:nrow(ct9),nrow(ct9)*0.7)
ct9train<-ct9[a9,]
a99<-ct9[-a9,]
ct_train1<-rbind(ct1train,ct2train,ct3train,ct4train,ct5train,ct6train,ct7train,ct8train,ct9train)
ct_test1<-rbind(a11,a22,a33,a44,a55,a66,a77,a88,a99)


################# 신경망 #################
apply(ct_train1,2,function(x) sum(is.na(x))) #결측값이 없다는 걸 알 수 있다.

#신경망 히든노드 개수에따른 오분류율표
library(nnet)
test.err<-function(h.size)
{set.seed(1234)
  ir <- nnet(Category ~ ., data=ct_train1, size = h.size,maxit=44,
             decay = 6e-5, trace=F)
  y<-ct_test1$Category
  p<- predict(ir, ct_test1, type = "class")
  err<-mean(y != p)
  c(h.size, err)
}
out<-t(sapply(2:19, FUN=test.err));out
plot(out, type="b", xlab="The number of Hidden units",
     ylab="Test Error")
#히든노드가 18개일 때 오분류율이 가장 작음을 확인 -> nnet의 size를 18로 설정할 근거
#이미 pca에서 정규화를 했으므로 신경망을 위한 정규화는 스킵
set.seed(1234)
out=nnet(Category ~ ., data=ct_train1, size=18,decay=6e-5, maxit=44,trace=F)
summary(out)
y<-ct_test1$Category
p<-as.factor(predict(out,ct_test1,type="class"))
k4<-table(y,p)
sum(k4[upper.tri(k4)|lower.tri(k4)])/sum(k4) #오분류율 계산기

install.packages("NeuralNetTools")
library(NeuralNetTools)
windows()
plotnet(out) #신경망그림
