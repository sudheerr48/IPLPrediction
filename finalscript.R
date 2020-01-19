install.packages("dplyr")
install.packages("caret") 
install.packages("e1071") 
require(xgboost)
library(dplyr)
library(caret) 
library(e1071)

#deliveries <- read.csv("~/Desktop/Github_Projetcs/IPL_project/data/deliveries.csv")
matches <- read.csv("~/Desktop/Github_Projetcs/IPL_project/data/matches.csv")
#df<-merge(deliveries, matches, by.x = "match_id", by.y = "id")
colnames(df)
print(summary(df)) 
df_train <- matches[,c("team1","team2","venue","toss_winner","toss_decision","result","winner")]
summary(df_train$winner)
#converting spaces into NA
df_train$winner[df_train$winner==""] <- NA
summary(df_train$winner)
#Selecting rows without NA's
df_train <- df_train[!is.na(df_train$winner),]
str(df_train)
#filtering matches without tie's
df_train <- df[df$result =='normal', ] 
#outputs classes distrubution
# summarize the class distribution
percentage <- prop.table(table(df_train$winner)) * 100
cbind(freq=table(df_train$winner), percentage=percentage)
#Rising Pune Supergiants problem
df_train$team1[df_train$team1 == 'Rising Pune Supergiant'] <- 'Rising Pune Supergiants'
df_train$team2[df_train$team2 == 'Rising Pune Supergiant'] <- 'Rising Pune Supergiants'
df_train$toss_winner[df_train$toss_winner == 'Rising Pune Supergiant'] <- 'Rising Pune Supergiants'
df_train$winner[df_train$winner == 'Rising Pune Supergiant'] <- 'Rising Pune Supergiants'

#reducing factors

df_train$winner_f <- factor(df_train$winner)
df_train$team1_f <- factor(df_train$team1)
df_train$team2_f <- factor(df_train$team2)
df_train$toss_winner_f<-factor(df_train$toss_winner)
df_train$result_f<-factor(df_train$result)

#Linear discriminant analysis
m1=lda(winner_f~team1_f+team2_f+toss_winner_f,df_train)

#Quadratic discriminant analysis
#m2=qda(winner_f~team1_f+team2_f+toss_winner_f,df_train)
#predict(m2,newdata=data.frame(team1_f = "Rising Pune Supergiants",team2_f = "Sunrisers Hyderabad",toss_winner_f = "Sunrisers Hyderabad"))

#Linear discriminant analysis
m2=lda(winner_f~team1_f+team2_f+toss_winner_f+toss_decision+venue,df_train)
predict(m2,newdata=data.frame(team1_f = "Sunrisers Hyderabad",team2_f = "Royal Challengers Bangalore",toss_winner_f = "Royal Challengers Bangalore",toss_decision = "bat",venue = "Rajiv Gandhi International Stadium, Uppal"))


#testing
predict(m1,newdata=data.frame(team1_f = "Rising Pune Supergiants",team2_f = "Sunrisers Hyderabad",toss_winner_f = "Sunrisers Hyderabad"))
predict(m1,newdata=data.frame(team1_f = df_train$team1_f[1:5],team2_f = df_train$team2_f[1:5],toss_winner_f = df_train$toss_winner_f[1:5]))

#working 
#n=length(df_train_nums$winner_f)
#nt=578
#train <- sample(1:n,nt)

#must_convert<-sapply(df_train,is.factor)       # logical vector telling if a variable needs to be displayed as numeric
#M2<-sapply(df_train[,must_convert],unclass)    # data.frame of all categorical variables now displayed as numeric
#df_train_nums<-cbind(df_train[,!must_convert],M2)
#df_train_nums$winner_f = df_train$winner_f

#nearest1 <- knn(train=df_train_nums[train,],test=df_train_nums[-train,], cl=df_train_nums$winner_f[train],k=1)

#library(plyr)
#mapvalues(df_train, from = c("Chennai Super Kings","Deccan Chargers","Delhi Daredevils",
                         #    "Gujarat Lions" ,"Kings XI Punjab","Kochi Tuskers Kerala" ,"Kolkata Knight Riders","Mumbai Indians",
                          #   "Pune Warriors","Rajasthan Royals","Rising Pune Supergiants","Royal Challengers Bangalore","Sunrisers Hyderabad"),to = c("1","2", "3","4","5","6","7","8","9","10","11","12","13"))
