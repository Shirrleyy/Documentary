rm(list=ls())

#install packages we need
install.packages("car")
install.packages("caret")
install.packages("rpart.plot")
install.packages("Metrics")
install.packages("randomforest")
library(randomForest)
library(car)
library(ggplot2)
library(lattice)
library(caret)
library(rpart)
library(rpart.plot)
library(Metrics)
library(rmse)


#Section 1:Read data and data pre-Process – preliminary.

#reading env and fish data
data(doubs,package="ade4")
env<-doubs$env
fish<-doubs$fish
fish

#summarizing fish abundance data by sites
tfs<-rowSums(fish)
env_fish<-cbind(env,tfs)
env_fish



#用featurePlot函数中的scatter绘制线性关系图
featurePlot(x=env_fish[, -12],
            y=env_fish[, 12],
            plot = "scatter",
            type=c("p","smooth"),
            layout=c(3,4))

#使用scatterplotMatrix绘制散点图
scatterplotMatrix(env_fish,
                  smooth=list(Ity.smooth=2,spread=F),
                  upper.panel=NULL,
                  main="Total_fish and env")

#去除total fish为0的行
env_fish<-subset(env_fish,tfs!=0)

#循环函数筛选离群值
for(i in 1:12){
  a<-which(env_fish[ ,i] %in% boxplot.stats(env_fish[ ,i])$out)
  print(a)
}
filter_env_fish<-env_fish[-c(1,2,3,6,14,22:25,28,29), ]

#删除零方差
dim(filter_env_fish)
nzv<-nearZeroVar(filter_env_fish)#integer empty

#removing highly correlated features (with an absolute correlation of 0.75 or higher) 
descrCor<-cor(filter_env_fish)
highlyCorDescr<-findCorrelation(descrCor,cutoff = .75)
filter_env_fish<-filter_env_fish[,-highlyCorDescr]
dim(filter_env_fish)


#Section II: Building a regression model. 

#splitting data into training and test sets
set.seed(1020)
assignment<-sample(1:2,
                   size=nrow(env_fish),
                   prob = c(0.75,0.25),
                   replace = TRUE)

training<-env_fish[assignment==1,]
test<-env_fish[assignment==2,]

#training tree,building a model using grid search
env_tfs_model<-rpart(formula=tfs~.,
                     data=training,
                     control = rpart.control(minsplit = 2),
                     method="anova")
print(env_tfs_model)

# visualizing boosting tree
rpart.plot(env_tfs_model)

#预测bt模型在测试集中的表现
pred<-predict(object=env_tfs_model,
              newdata = test)
pred

#评估bt模型优劣
rmse(actual=test$tfs,
     predicted=pred)


#Build another model with random forest
fitControl<-trainControl(
  method = "repeatedcv",
  number = 30,
  repeats=30
)

set.seed(111)
rf_random<-train(tfs~.,
                 data=training,
                 method="rf",
                 trControl=fitControl,
                 metric="RMSE",
                 verbose=T)
rf_random
plot(rf_random)
#预测rf模型在测试集中的表现
pred2<-predict(object=rf_random,
               newdata = test)
pred2


#评估rf模型优劣
rmse(actual=test$tfs,
     predicted=pred2)

#random forest model's RMSE < boosting tree model , so random forest model is better


