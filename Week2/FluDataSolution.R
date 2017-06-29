FluTrain = read.csv("FluTrain.csv")
str(FluTrain)
max(FluTrain$ILI)
subset(FluTrain,ILI==max(ILI))
subset(FluTrain,Queries==max(Queries))
which.max(FluTrain$ILI)
FluTrain$Week[303]
hist(FluTrain$ILI,na.rm=TRUE)
plot(FluTrain$Queries,log(FluTrain$ILI))
FluTrend = lm(log(ILI)~ Queries, data=FluTrain)
summary(FluTrend)
Correlation=cor(FluTrain$Queries,log(FluTrain$ILI))
R2=0.709
R2==log(1/Correlation)
R2==exp(-0.5*Correlation)
R2==Correlation^2
FluTest = read.csv("FluTest.csv")
PredTest1 = exp(predict(FluTrend, newdata=FluTest))
Estimate_ILI=PredTest1[which(FluTest$Week=="2012-03-11 - 2012-03-17")]
Observed_ILI=FluTest$ILI[which(FluTest$Week=="2012-03-11 - 2012-03-17")]
(Observed_ILI - Estimate_ILI)/Observed_ILI
SSE = sum((PredTest1-(FluTest$ILI))^2)
RMSE = sqrt(SSE/nrow(FluTest))
RMSE
install.packages("zoo")
library(zoo)
ILILag2 = lag(zoo(FluTrain$ILI), -2, na.pad=TRUE)
FluTrain$ILILag2 = coredata(ILILag2)
summary(FluTrain)
?lag
?lag
?zoo
plot(log(FluTrain$ILILag2),log(FluTrain$ILI))
FluTrend2<-lm(log(ILI)~Queries + log(ILILag2),data=FluTrain)
summary(FluTrend2)
summary(FluTrend)
ILILag2 = lag(zoo(FluTest$ILI), k=-2, na.pad=TRUE)
?lag
summary(FluTest)
FluTest$ILILag2 = coredata(ILILag2)
FluTrain$ILI[1]
nrow(FluTrain)
FluTest$ILI[0]
PredTest2 = exp(predict(FluTrend2, newdata=FluTest))
SSE = sum((PredTest2 - FluTest$ILI)^2)
SSE = sum((PredTest2 - FluTest$ILI)^2)          
sqrt(mean((PredTest2-FluTest$ILI)^2))
