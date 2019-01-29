library(randomForest)
library(mlbench)
library(caret)
library(e1071)
library(MASS)
library(ggparallel)
library(rpart)
library(rpart.plot)
library(RColorBrewer)
library(MatchIt)
library(splitstackshape)
library(gbm)
library(xgboost)
library(glmnet)
library(plyr)
library(dplyr)
library(lubridate)

setwd('C:/Users/rchang/OneDrive - FI Consulting/Kaggle/NBA')
df <- read.csv('training_set.csv')
td <- read.csv('test_set.csv')
player <- read.csv('player_data.csv')
game <- read.csv('game_data.csv')

# top countries by viewership in the 2017-18 season
top_country <- c('C176','C181','C169','C155','C183','C47','C49','C191','C123','C143','C114','C185','C78','C75','C9','C150','C154','C211','C208','C134','C69','C33','C86','C41','C117','C12','C106','C215','C122','C116','C72','C65','C182','C199','C207','C202','C225','C80','C124','C10','C79','C130','C190','C50','C103','C97','C56','C131','C77','C8','C83','C218','C168','C153','C71','C101','C213')
#remove records that don't have top countries
df_small <- df[df$Country %in% top_country,]

df_small_2016 <- df_small[df_small$Season == "2016-17",]
df_small_2017 <- df_small[df_small$Season == "2017-18",]
country_avg_2016 <- aggregate(df_small_2016$Rounded.Viewers, list(df_small_2016$Country), mean)
country_avg_2017 <- aggregate(df_small_2017$Rounded.Viewers, list(df_small_2017$Country), mean)

#function to count the number of countries where a team's intl viewership exceeds the average
teamCountry_2016 <- function(teamName){
  temp <- df_small_2016[df_small_2016$Home_Team == teamName,6:7]
  temp_avg <- aggregate(temp$Rounded.Viewers, list(temp$Country), mean)
  temp_country_avg <- merge(temp_avg,country_avg_2016,by="Group.1", all=TRUE)
  temp_country_avg$diff <- (temp_country_avg$x.x - temp_country_avg$x.y) / temp_country_avg$x.y 
  return(temp_country_avg$diff)
}

teamCountry_2017 <- function(teamName){
  temp <- df_small_2017[df_small_2017$Home_Team == teamName,6:7]
  temp_avg <- aggregate(temp$Rounded.Viewers, list(temp$Country), mean)
  temp_country_avg <- merge(temp_avg,country_avg_2017,by="Group.1", all=TRUE)
  temp_country_avg$diff <- (temp_country_avg$x.x - temp_country_avg$x.y) / temp_country_avg$x.y 
  return(temp_country_avg$diff)
}

# loops through every team to get the number of countries where viewership exceeds average
team_list <- levels(df$Home_Team)
c_2016 <- as.data.frame(matrix(rep(0),nrow=1,ncol=nrow(country_avg_2016)))
for(teamName in team_list){
  temp_exceed <- teamCountry_2016(teamName)
  c_2016 <- rbind(c_2016,temp_exceed)
}

c_2017 <- as.data.frame(matrix(rep(0),nrow=1,ncol=nrow(country_avg_2017)))
for(teamName in team_list){
  temp_exceed <- teamCountry_2017(teamName)
  c_2017 <- rbind(c_2017,temp_exceed)
}

# clean-up, get rid of top row, rename columns, cbind list of team names, replace NA with 0 (assumption is that NAs perform at or below league average)
c_2016 = data.frame(c_2016[-1,])
colnames(c_2016) <- country_avg_2016$Group.1
team_exceed_2016 = cbind(team_list,c_2016)
team_exceed_2016[is.na(team_exceed_2016)] <- 0

c_2017 = data.frame(c_2017[-1,])
colnames(c_2017) <- country_avg_2017$Group.1
team_exceed_2017 = cbind(team_list,c_2017)
team_exceed_2017[is.na(team_exceed_2017)] <- 0

# find each games' "country score", a measure of country interest in the game
df_teams <- unique(df[,c("Season", "Away_Team", "Home_Team")])
df_teams <- mutate(df_teams, id = rownames(df_teams))
df_teams_2016 = df_teams[df_teams$Season == "2016-17", ]
df_teams_2017 = df_teams[df_teams$Season == "2017-18", ]

# idea is that we have to find the max of the home team and away team country scores to get an overall measure of intl viewership for a particular game
colnames(team_exceed_2016)[1] <- "Away_Team"
temp_away_2016 <- merge(df_teams_2016,team_exceed_2016,by="Away_Team")
colnames(team_exceed_2016)[1] <- "Home_Team"
temp_home_2016 <- merge(df_teams_2016,team_exceed_2016,by="Home_Team")
temp_all_2016 <- rbind(temp_away_2016,temp_home_2016)
temp_all_2016 <- subset(temp_all_2016,select=-c(Away_Team, Home_Team))

colnames(team_exceed_2017)[1] <- "Away_Team"
temp_away_2017 <- merge(df_teams_2017,team_exceed_2017,by="Away_Team")
colnames(team_exceed_2017)[1] <- "Home_Team"
temp_home_2017 <- merge(df_teams_2017,team_exceed_2017,by="Home_Team")
temp_all_2017 <- rbind(temp_away_2017,temp_home_2017)
temp_all_2017 <- subset(temp_all_2017,select=-c(Away_Team, Home_Team))

temp_all <- rbind(temp_all_2016, temp_all_2017)
temp_all <- temp_all[with(temp_all,order(id, Season)),]
exceed_sum <- temp_all %>% group_by(id, Season) %>% summarise_all(funs(max))
merge_teams_exceed <- merge(df_teams,exceed_sum,by="id")


# now merge the exceed score for each game back to the train dataset. Country is not in the test dataset, so remove from train dataset
df2 <- ddply(df,.(Season, Game_ID, Game_Date, Away_Team, Home_Team),summarize,Total_Viewers=sum(Rounded.Viewers))
colnames(merge_teams_exceed)[2] <- "Season"
df2$month <- as.factor(month(as.POSIXlt(df2$Game_Date, format="%m/%d/%Y"))) # adding month as factor variable
df2$weekday <- as.factor(wday(as.POSIXlt(df2$Game_Date, format="%m/%d/%Y"))) # adding day of week as factor variable
df2$xmas <- 0
df2$xmas[as.POSIXlt(df2$Game_Date, format="%m/%d/%Y") == '2016-12-25'] <- 1
df2$xmas[as.POSIXlt(df2$Game_Date, format="%m/%d/%Y") == '2017-12-25'] <- 1
df3 <- merge(df2,merge_teams_exceed,by=c("Away_Team","Home_Team","Season"))

# work on dataframe of players, idea is that viewers increase/decrease if all-star players are playing in the game
player_small <- subset(player, select=c("Game_ID", "Name", "ASG_Team", "Active_Status", "International"))
player_small <- player_small[player_small$ASG_Team != "None" | player_small$International != 0, ]
player_small <- player_small[player_small$Active_Status != "Inactive", ]
df_players <- as.data.frame(unique(player_small[,c("Name")]))
colnames(df_players)[1] <- "Name"
player_small$suited_up <- rep(1,nrow(player_small)) #indicator variable

# function to identify when all-star players on international players are playing in a particular game
findPlayers <- function(gameID){
  temp <- player_small[player_small$Game_ID == gameID,c("Name","suited_up")]
  temp_suited_up <- merge(temp,df_players,by="Name", all=TRUE)
  return(temp_suited_up$suited_up)
}

# loops through every game to find all-star players playing in that game
game_list <- unique(player_small[,c("Game_ID")])
p <- as.data.frame(matrix(rep(0),nrow=1,ncol=nrow(df_players)))
for(gameID in game_list){
  player_ind <- findPlayers(gameID)
  p <- rbind(p,player_ind)
}

# clean-up, get rid of top row, rename columns, cbind list of gameIDs, replace NA with 0
p = data.frame(p[-1,])
df_players_char <- as.character(df_players$Name)
colnames(p) <- sort(df_players_char)
players_suited_up = cbind(game_list,p)
colnames(players_suited_up)[1] <- "Game_ID"
players_suited_up[is.na(players_suited_up)] <- 0
players_suited_up$num_all_stars <- rowSums(players_suited_up[,-1]) #counts number of all stars and internationals playing in game

# merge player indicators back to training datset
df4 <- merge(df3,players_suited_up,by=c("Game_ID"), all=TRUE)
df4<-df4[!(is.na(df4$Game_Date)),]
df4[is.na(df4)] <- 0
df4 <- subset(df4, select = -c(Game_ID, id, Season.y))
df4 <- subset(df4, select = -c(Game_Date)) #getting rid of game_date the variable is not realistic for future prediction


### now prepare the test dataset
td$month <- as.factor(month(as.POSIXlt(td$Game_Date, format="%m/%d/%Y"))) # adding month as factor variable
td$weekday <- as.factor(wday(as.POSIXlt(td$Game_Date, format="%m/%d/%Y"))) # adding day of week as factor variable
td$xmas <- 0
td$xmas[as.POSIXlt(td$Game_Date, format="%m/%d/%Y") == '2016-12-25'] <- 1
td$xmas[as.POSIXlt(td$Game_Date, format="%m/%d/%Y") == '2017-12-25'] <- 1

# loops through every team to get the number of countries where viewership exceeds average
team_list <- levels(td$Home_Team)
c_2016 <- as.data.frame(matrix(rep(0),nrow=1,ncol=nrow(country_avg_2016)))
for(teamName in team_list){
  temp_exceed <- teamCountry_2016(teamName)
  c_2016 <- rbind(c_2016,temp_exceed)
}

c_2017 <- as.data.frame(matrix(rep(0),nrow=1,ncol=nrow(country_avg_2017)))
for(teamName in team_list){
  temp_exceed <- teamCountry_2017(teamName)
  c_2017 <- rbind(c_2017,temp_exceed)
}

# clean-up, get rid of top row, rename columns, cbind list of team names, replace NA with 0 (assumption is that NAs perform at or below league average)
c_2016 = data.frame(c_2016[-1,])
colnames(c_2016) <- country_avg_2016$Group.1
team_exceed_2016 = cbind(team_list,c_2016)
team_exceed_2016[is.na(team_exceed_2016)] <- 0

c_2017 = data.frame(c_2017[-1,])
colnames(c_2017) <- country_avg_2017$Group.1
team_exceed_2017 = cbind(team_list,c_2017)
team_exceed_2017[is.na(team_exceed_2017)] <- 0

# find each games' "country score"
df_teams <- unique(td[,c("Season", "Away_Team", "Home_Team")])
df_teams <- mutate(df_teams, id = rownames(df_teams))
df_teams_2016 = df_teams[df_teams$Season == "2016-17", ]
df_teams_2017 = df_teams[df_teams$Season == "2017-18", ]

# idea is that we find the max of the home team and away team country scores to get an overall measure of intl viewership for a particular game
colnames(team_exceed_2016)[1] <- "Away_Team"
temp_away_2016 <- merge(df_teams_2016,team_exceed_2016,by="Away_Team")
colnames(team_exceed_2016)[1] <- "Home_Team"
temp_home_2016 <- merge(df_teams_2016,team_exceed_2016,by="Home_Team")
temp_all_2016 <- rbind(temp_away_2016,temp_home_2016)
temp_all_2016 <- subset(temp_all_2016,select=-c(Away_Team, Home_Team))

colnames(team_exceed_2017)[1] <- "Away_Team"
temp_away_2017 <- merge(df_teams_2017,team_exceed_2017,by="Away_Team")
colnames(team_exceed_2017)[1] <- "Home_Team"
temp_home_2017 <- merge(df_teams_2017,team_exceed_2017,by="Home_Team")
temp_all_2017 <- rbind(temp_away_2017,temp_home_2017)
temp_all_2017 <- subset(temp_all_2017,select=-c(Away_Team, Home_Team))

temp_all <- rbind(temp_all_2016, temp_all_2017)
temp_all <- temp_all[with(temp_all,order(id, Season)),]
exceed_sum <- temp_all %>% group_by(id, Season) %>% summarise_all(funs(max))
merge_teams_exceed <- merge(df_teams,exceed_sum,by="id")

# now merge the exceed score for each game back to the test dataset. 
colnames(merge_teams_exceed)[2] <- "Season"
td3 <- merge(td,merge_teams_exceed,by=c("Away_Team","Home_Team","Season"))


# now merge player indicators to the test dataset
td4 <- merge(td3,players_suited_up,by=c("Game_ID"), all=TRUE)
td4<-td4[!(is.na(td4$Game_Date)),]
td4[is.na(td4)] <- 0
td4 <- subset(td4, select = -c(Game_ID, id, Season.y))
td4 <- subset(td4, select = -c(Game_Date))

###

# prepare the train dataset for glmnet model
x_pred <- subset(df4, select = -c(Total_Viewers))
x_train <- model.matrix( ~ .-1, x_pred)
x_train <- as.data.frame(x_train)

# prepare the test dataset for glmnet model
y_pred <- subset(td4, select = -c(Total_Viewers))
y_train <- model.matrix( ~ .-1, y_pred)
y_train <- as.data.frame(y_train)

# game_date code - problem is the test dataset doesn't contain all the same game dates as train so it has fewer variables, will add those variables to test so it is same dimension as train
# additional_cols <- setdiff(names(x_train),names(y_train))
# dummy_cols <- as.data.frame(matrix(rep(0,nrow(y_train)*length(additional_cols)), ncol = length(additional_cols)))
# colnames(dummy_cols) <- additional_cols
# y_train <- cbind(y_train,dummy_cols)

# I don't see a need to have home and away indicators, will collapse them
x_train_away <- x_train[,1:30]
x_train_home <- x_train[,31:59]
x_train_home <- cbind(Home_TeamATL = 0, x_train_home) # Atlanta doesn't have a home team, have to manually add it
colnames(x_train_away) <- team_list
colnames(x_train_home) <- team_list
x_train_teams <- x_train_away + x_train_home
x_train <- x_train[-c(1:59)]
x_train <- cbind(x_train_teams, x_train)

y_train_away <- y_train[,1:30]
y_train_home <- y_train[,31:59]
y_train_home <- cbind(Home_TeamATL = 0, y_train_home) # Atlanta doesn't have a home team, have to manually add it
colnames(y_train_away) <- team_list
colnames(y_train_home) <- team_list
y_train_teams <- y_train_away + y_train_home
y_train <- y_train[-c(1:59)]
y_train <- cbind(y_train_teams, y_train)

## code to get interaction terms
#f <- as.formula(y ~ .*.)
f <- as.formula(y ~ .^2)
y <- df4$Total_Viewers
x_interact <- model.matrix(f, x_train)[, -1]
x_interact <- as.matrix(x_interact)

## code to get interaction terms
#f <- as.formula(y ~ .*.)
f <- as.formula(y ~ .^2)
y <- td$Game_ID # dummy variable
y_interact <- model.matrix(f, y_train)[, -1]
y_interact <- as.matrix(y_interact)

### estimate glmnet model for the train dataset

fit <-glmnet(x = x_interact, y = df4$Total_Viewers, alpha = 1) 
#plot(fit, xvar = "lambda")

crossval <-  cv.glmnet(x = x_interact, y = df4$Total_Viewers)
#plot(crossval)
penalty <- crossval$lambda.min #optimal lambda
penalty #minimal shrinkage
fit1 <-glmnet(x = x_interact, y = df4$Total_Viewers, alpha = 1, lambda = penalty ) #estimate the model with that
coef(fit1)
vi <- varImp(fit1, lambda = fit1$lambda)

### use model to predict the test dataset

results <- as.data.frame(predict(object=fit1, y_interact))
pred <- cbind(td$Game_ID, results)
colnames(pred)<- c("Game_ID","Total_Viewers")
td_final <- merge(pred, td, by="Game_ID")
td_final <- subset(td_final, select = -c(Total_Viewers.y))
colnames(td_final)[2] <- "Total_Viewers"

write.csv(td_final, file = "test_set_20180711.csv", row.names=FALSE)
