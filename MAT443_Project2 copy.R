wine <- read.csv("~/Downloads/winemag-data-130k-v2.csv")

#What taster#
table(Wine$taster_name)
Wine.Roger=Wine[Wine$taster_name == "Roger Voss",]
table(Wine.Roger$country) #US 2, France 18603, italy 97, argentina 3800#

wine.K=wine[wine$taster_name == "Matt Kettmann",] #all US#
str(Wine.K)
table(Wine.Kettman$country)
Wine.Boone=Wine[Wine$taster_name == "Virginie Boone",]
table(Wine.Boone$country) #all US#
Wine.Gregutt=Wine[Wine$taster_name == "Paul Gregutt",]
table(Wine.Gregutt$country) #mainly US#
Wine.S=Wine[Wine$taster_name=="Michael Schachner",]
table(Wine.Schachner$country) #3797 A, 20 F, 76 I 6575 S, 71 U#




#No more designation, wine name#
wine.K= wine.K[,c(-2,-3,-4,-7,-10,-11,-13)]
wine.K$region_1=factor(wine.K$region_1)
wine.K$region_2=factor(wine.K$region_2)
wine.K$variety=factor(wine.K$variety)

#Omit NA's#
str(wine.K)
library(dplyr)
empty_as_na <- function(x){
  if("factor" %in% class(x)) x <- as.character(x) 
  ifelse(as.character(x)!="", x, NA)
}
wine1=wine.K %>% mutate_each(funs(empty_as_na))
Wine.K=na.omit(wine1)

Wine.K$variety<- as.factor(Wine.K$variety)
Wine.K$region_1<- as.factor(Wine.K$region_1)
Wine.K$region_2<- as.factor(Wine.K$region_2)

Wine.K$designation<- as.factor(Wine.K$designation)
Wine.K$province<- as.factor(Wine.K$province)
Wine.K$title<- as.factor(Wine.K$title)
Wine.K$winery<- as.factor(Wine.K$winery)

#Combine Wine Types#
library(plyr)
Wine.K$variety=revalue(Wine.K$variety, c("Rhône-style Red Blend"="Red Blend", "Bordeaux-style Red Blend
"="Red Blend", "Rhône-style White Blend"="White Blend","Syrah-Petite Sirah"="Syrah",
                        "G-S-M"="Red Blend","Syrah-Petit Verdot"="Syrah","Grenache-Syrah
"="Grenache","Syrah-Grenache"="Syrah","Syrah-Mourvèdre"="Syrah","Syrah-Cabernet Sauvignon"="Syrah","Cabernet Sauvignon-Syrah
"="Cabernet Sauvignon","Mourvèdre-Syrah"="Mourvèdre","Sangiovese-Syrah"="Sangiovese", "Syrah-Merlot"="Syrah","Malbec-Syrah
"="Malbec","Syrah-Cabernet"="Syrah","Tannat-Syrah"="Tannat","Cabernet Franc-Merlot"="Cabernet Franc","Cabernet Sauvignon-Merlot
"="Cabernet Sauvignon","Merlot-Cabernet Sauvignon"="Merlot","Cabernet Sauvignon-Sangiovese"="Cabernet Sauvignon","Sangiovese-Cabernet Sauvignon
"="Sangiovese","Cabernet Sauvignon-Cabernet Franc"="Cabernet Sauvignon","Viognier-Chardonnay"="Viognier","Sauvignon Blanc-Semillon
"="Sauvignon Blanc","Semillon-Sauvignon Blanc"="Semillon", "Grenache Blanc"="Grenache", "Grenache Blend
"="Grenache","Grenache Noir"="Grenache","Viognier-Grenache Blanc"="Viognier","Tempranillo Blend
"="Tempranillo","Roussanne-Viognier"="Roussanne"))
str(Wine.K$variety)

#Make Dummy Variables#
Wine2.0=fastDummies::dummy_cols(Wine.K)
Wine2.1=Wine2.0[,c(-1,-4,-5,-6)]
names(Wine2.1)=make.names(names(Wine2.1))

#split the data#
winex=Wine2.1[-1]
y=Wine2.1$points
library(caret)
set.seed(8)
train=createDataPartition(y, p = 3/4, list = FALSE)
trainx=winex[train,]
testx=winex[-train,]
trainClass=y[train]
testClass=y[-train]   

Train=Wine2.1[train,]
Test=Wine2.1[-train,]
y.test=Test$points
str(Wine2.1)

#pca#
pca=prcomp(winex)
summary(pca)

#linear regression#
mr=lm(points~., data=Train)
summary(mr)
mr.pred=predict(mr, newdata=Test)
MSE.mr=mean((y.test-mr.pred)^2)
MSE.mr
#5.046114#

#linear regression with just points#
mr.price=lm(points~price, data=Train)
summary(mr.price)
mr.pred.price=predict(mr.price, newdata=Test)
MSE.mr.price=mean((y.test-mr.pred.price)^2)
MSE.mr.price
#6.030198#

#svm#
library(e1071)
svm.fit=svm(points~.,data=Train,kernel ="linear", scale=FALSE)
summary(svm.fit)
svm.pred=predict(svm.fit,newdata=Test)
MSE.svmlin=mean((y.test-svm.pred)^2)
MSE.svmlin
#4.405336#

tune.out=tune(svm, points~.,data=Train, kernel ="linear", ranges =list(cost=c(0.001 , 0.01, 0.1, 1,5,10,100)))
summary(tune.out)
bestmod =tune.out$best.model
ypred=predict(bestmod,spam[test,])

#radial#
svmrad.fit<-svm(points~.,data=Train,kernel ="radial", scale=FALSE)
svmrad.pred=predict(svmrad.fit,newdata=Test)
MSE.svmrad=mean((y.test-svmrad.pred)^2)
MSE.svmrad
#4.23709#

set.seed(8)
tune.out=tune(svm, points~.,data=Train, kernel ="radial", scale=FALSE,
              ranges =list(cost=c(0.001 , 0.1, 1,10,100), gamma=c(.1,.5,1,2,10)))
summary(tune.out)
bestmod =tune.out$best.model
ypred=predict(bestmod,Test)
MSE.ypred=mean((y.test-ypred)^2)
MSE.ypred
#4.128963#

#polynomial#
svm.poly<-svm(points~.,data=Train,kernel ="polynomial", scale=FALSE, maxit=50000000)
svmpoly.pred=predict(svm.poly,newdata=Test)
MSE.svmpoly=mean((y.test-svmpoly.pred)^2)
MSE.svmpoly
#7.780739#

#sigmoid#
svmsig.fit=svm(points~.,data=Train,kernel ="sigmoid", scale=FALSE)
svmsig.pred=predict(svmsig.fit,newdata=Test)
MSE.svmsig=mean((y.test-svmsig.pred)^2)
MSE.svmsig
#1086.602#

Train=Train[,-54]
#boosting#
library(gbm)
boost.fit=gbm(points~.-variety_Muskat,data=Train,distribution="gaussian", n.trees =5000, interaction.depth =4)
boost.pred=predict(boost.fit, newdata=Test, n.trees =5000)
MSE.boost=mean((y.test-boost.pred)^2)
MSE.boost
#4.150284#

#tune#
boost.fit1=gbm(points~.-variety_Muskat,data=Train,distribution="gaussian", n.trees =5000, interaction.depth =6,shrinkage =0.01,verbose =F)
pred.boost1=predict(boost.fit1, newdata=Test, n.trees =5000)
MSE.boost1=mean((y.test-pred.boost1)^2)
MSE.boost1
#3.965529#

#random forest with caret#
library(randomForest)
set.seed(8)
rf.fit=train(trainx, trainClass,method = "rf", tuneLength = 5, scaled = TRUE)
rf.pred=predict(rf.fit, newdata = testx)
MSE.rf=mean((y.test-rf.pred)^2)
MSE.rf
summary(rf.fit)
plot(rf.fit)
#3.879257#

#random forest, no caret#
library(randomForest)
set.seed(8)
rf.fit1=randomForest(points ~.,data=Train,mtry=40, importance=TRUE)
importance(rf.fit1)
varImpPlot(rf.fit1)
rf.pred1=predict(rf.fit1, newdata=Test)
MSE.rf1=mean((y.test-rf.pred1)^2)
MSE.rf1
#3.874154874#
#pcr#
library (pls)
set.seed(8)
pcr.fit=pcr(points~., data=Train , validation ="CV")
summary(pcr.fit)
validationplot(pcr.fit ,val.type="R2")
pcr.pred=predict(pcr.fit ,Test,ncomp =66)
MSE.pcr=mean((y.test-pcr.pred)^2)
MSE.pcr
#4.987548#

#nnet, no caret#
library(nnet)
nnet.fit=nnet(points~.,data=Train,size=5,lineout=TRUE,skip=TRUE)
summary(nnet.fit)
nnet.pred<-predict(nnet.fit,newdata=Test)
MSE.nnet=mean((y.test-nnet.pred)^2)
MSE.nnet
#7932.003#

#nnet with caret#
set.seed(8)
nn.fit=train(trainx, trainClass,method = "nnet", tuneLength = 5, scaled = TRUE)
nn.pred=predict(nn.fit, newdata = testx)
MSE.nn=mean((y.test-nn.pred)^2)
MSE.nn

