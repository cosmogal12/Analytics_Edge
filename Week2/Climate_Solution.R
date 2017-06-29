climate = read.csv("climate_change.csv")
training_06 = subset(climate, Year<=2006)
training_08 = subset(climate, Year>2006)
TempReg = lm(Temp ~ MEI+CO2+CH4+N2O+CFC.11+CFC.12+TSI+Aerosols,data=training_06)
summary(TempReg)
train = training_06
test = training_08
TempReg$coefficients
cor(train$N2O,train)
cor(train$CFC.11,train)
N20model = lm( Temp ~ MEI+ N2O + TSI + Aerosols, data=train)
summary(N20model)
N20model$coefficients[3]
?step
StepModel = step(TempReg)
summary(StepModel)
TempPredict = predict(StepModel,test)
summary(TempPredict)
TempPredict
SSE=sum((TempPredict-test$Temp)^2)
SST = sum ((mean(train$Temp)- test$Temp)^2)
R2 = 1-(SSE/SST)
R2
