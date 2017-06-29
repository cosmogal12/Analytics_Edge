songs = read.csv("songs.csv")
str(songs)
songs_2010=subset(songs,year=="2010")
nrow(songs_2010)
table(songs$year)
top10MJ=subset(songs,artistname=="Michael Jackson" & Top10=="1")
top10MJ$songtitle[1:5]
top10MJ[c(“songtitle”, “Top10”)]
MichaelJackson = subset(songs, artistname == "Michael Jackson")
MichaelJackson[c( “Top10”)]
top10MJ[c("Top10","songtitle")]
table(songs$timesignature)
which.max(songs$tempo)
songs$songtitle[6206]
# Creating a prediction Model. We wish to predict whether or not a song will make it to the Top 10. To do this, first use the subset function to split the data into a training set "SongsTrain" consisting of all the observations up to and including 2009 song releases, and a testing set "SongsTest", consisting of the 2010 song releases.

SongsTrain = subset(songs,year<="2009")
SongsTest = subset(songs,year=="2010")

#First define a vector of variable names called nonvars - these are the variables that we won't use in our model.

nonvars = c("year", "songtitle", "artistname", "songID", "artistID")

# remove these variables from your training and testing sets

SongsTrain = SongsTrain[ , !(names(SongsTrain) %in% nonvars) ]

SongsTest = SongsTest[ , !(names(SongsTest) %in% nonvars) ]

# use the glm function to build a logistic regression model to predict Top10

Model1 = glm(Top10 ~. ,data=SongsTrain, family=binomial)
summary(Model1)

#  correlation between the variables "loudness" and "energy"

cor(SongsTrain)

# Create new model

SongsLog2 = glm(Top10 ~ . - loudness, data=SongsTrain, family=binomial)

# We just subtracted the variable loudness. We couldn't do this with the variables "songtitle" and "artistname", because they are not numeric variables, and we might get different values in the test set that the training set has never seen

summary(SongsLog2)

# Create new model

SongsLog3 = glm(Top10 ~ . - energy, data=SongsTrain, family=binomial)
summary(SongsLog3)

# Make predictions on the test set using Model 

predictTest = predict(SongsLog3,newdata=SongsTest,type="response")
summary(predictTest)

# Create a confusion matrix. What is the accuracy of Model 3 on the test set, using a threshold of 0.45?

table(SongsTest$Top10, predictTest > 0.45)
accuracy = (309+19)/(373)
accuracy
# Find baseline accuracy
table(SongsTest$Top10)
309/314
