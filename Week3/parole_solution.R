parole = read.csv("parole.csv")
table(parole$violator)
parole$state = as.factor(parole$state)
parole$crime = as.factor(parole$crime)
summary(parole$state)

# Split data

set.seed(144)
library(caTools)
split = sample.split(parole$violator, SplitRatio = 0.7)
train = subset(parole, split == TRUE)
test = subset(parole, split == FALSE)
202/(202+473)
paroleLog = glm(violator~.,data=train,family=binomial)
summary(paroleLog)
# A parolee who is male, of white race, aged 50 years at prison release,
# from the state of Maryland, served 3 months, had a maximum sentence of
# 12 months, did not commit multiple offenses, and committed a larceny.
# Obtain odds and probability that he is a violator. From the logistic
# regression equation, we have log(odds) = -4.2411574 + 0.3869904*male +
# 0.8867192*race - 0.0001756*age + 0.4433007*state2 + 0.8349797*state3 -
# 3.3967878*state4 - 0.1238867*time.served + 0.0802954*max.sentence +
# 1.6119919*multiple.offenses + 0.6837143*crime2 - 0.2781054*crime3 -
# 0.0117627*crime4. This parolee has male=1, race=1, age=50, state2=0,
# state3=0, state4=0, time.served=3, max.sentence=12, multiple.offenses=0,
# crime2=1, crime3=0, crime4=0. We conclude that log(odds) = -1.700629.
-4.241157 +0.3869904+0.886719-(50*0.0001756)-(0.123886*3)+(12*0.0802954)+0.683714
odds=exp(-1.700627)

# p = odds/(1 + odds)

p = odds/(1 + odds)

# Predict

predictTest = predict(paroleLog,newdata=test,type="response")
summary(predictTest)

#Confusion matrix

table(test$violator, predictTest> 0.30)
(167+13)/202
table(test$violator)

# Evaluating model

library(ROCR)
ROCRpred = prediction(predictTest,test$violator)
as.numeric(performance(ROCRpred,"auc")@y.values)
summary(ROCRpred)
