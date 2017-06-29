data(state)
statedata = cbind(data.frame(state.x77), state.abb, state.area, state.center,  state.division, state.name, state.region)
summary(statedata)
str(statedata)
plot(statedata$x,statedata$y)
tapply(statedata$HS.Grad,statedata$state.region,mean)
?boxplot
boxplot(statedata$Murder ~ statedata$state.region)
northeast = subset(statedata,statedata$state.region=="Northeast")
table(northeast$state.abb,northeast$Murder)
lmScore = lm(Life.Exp ~ Population+Income+Illiteracy+Murder+HS.Grad+Frost+Area,data = statedata)
summary(lmScore)
lmScore$coefficients
plot(statedata$Income, statedata$Life.Exp)
lmScore = lm(Life.Exp ~ Population+Murder+HS.Grad+Frost,data = statedata)
summary(lmScore)
predictlife = predict(lmScore)
summary(predictlife)
sort(predictlife)
which.min(statedata$Life.Exp)
statedata$state.name[40]
which.max(statedata$Life.Exp)
statedata$state.name[11]
sort(abs(lmScore$residuals))
