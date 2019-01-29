# NBA Hackathon
# Basketball Analytics
# Team name: Team FI
# Team members:
#  Mitchell Faber
#  Robert Chang

library(RJSONIO);library(matrixStats);library(data.table);library(dplyr);library(zoo)
options(stringsAsFactors = F)

# Import .txt data files
eventcodes <- read.delim("C:/Users/RiggleMonster/OneDrive/Analytics Projects/2018 NBA Hackathon/Basketball Analytics/NBA Hackathon - Event Codes.txt")
gamelineup <- read.delim("C:/Users/RiggleMonster/OneDrive/Analytics Projects/2018 NBA Hackathon/Basketball Analytics/NBA Hackathon - Game Lineup Data Sample (50 Games).txt")
pbp <- read.delim("C:/Users/RiggleMonster/OneDrive/Analytics Projects/2018 NBA Hackathon/Basketball Analytics/NBA Hackathon - Play by Play Data Sample (50 Games).txt")

# View imported data files
View(eventcodes)
View(gamelineup)
View(pbp[1:10,])

# Organize substitutions
pbp[, unique(gamelineup$Person_id)]<-0

pbp[pbp$Period==1,  unique(gamelineup$Person_id[gamelineup$status!="A"])]<-1
pbp$TimeElapsed<-ifelse(pbp$Period<=4,7200-pbp$PC_Time, 3000-pbp$PC_Time)
pbp$TimeElapsed<-ifelse(pbp$Period<=4, pbp$TimeElapsed+7200*(pbp$Period-1), pbp$TimeElapsed+7200*4+3000*(pbp$Period-3000))

is_sub<- which(pbp$Event_Msg_Type==8)
for(i in 1:nrow(pbp)){
  
  # put player in if they register a play and were never in for the quarter (exception is technical fouls or weird game-violations --can get these while not in play)
  'if(!i %in% is_sub &
     sum(is.na(pbp[i,grepl("DESCRIPT", colnames(pbp))]))!=3 &
     sum(grepl("T.FOUL|Ejection|Timeout|TECH.FOUL|Unsportsmanlike", pbp[i,grepl("DESCRIPT", colnames(pbp))]))==0 ){
    
    if(pbp$Person1[i]%in% colnames(pbp)){
      if(sum(pbp[pbp$Period==pbp$Period[i], as.character(pbp$Person1[i])])==0){
        pbp[pbp$Period==pbp$Period[i], as.character(pbp$Person1[i])]<-1
      }
    }
    if(pbp$Person2[i]%in% colnames(pbp)){
      if(sum(pbp[pbp$Period==pbp$Period[i], as.character(pbp$Person2[i])])==0){
        pbp[pbp$Period==pbp$Period[i], as.character(pbp$Person2[i])]<-1
      }
    }
  }'
  
  #handling substitution events
  if(i %in% is_sub ){
    #player enterring
    pbp[pbp$TimeElapsed>pbp$TimeElapsed[i]& pbp$Period==pbp$Period[i], as.character(pbp$Person2[i])]<-1  
    #player leaving
    pbp[pbp$TimeElapsed>pbp$TimeElapsed[i]& pbp$Period==pbp$Period[i], as.character(pbp$Person1[i])]<-0
  }
}
tail(pbp[,grepl("EVENTM|PCTIME|HOMED|VISITORD|PLAYER1_NAME|PLAYER1_TEAM_ID|POSS", colnames(pbp)) ],20)

###POSESSIONS AND SCORING##########

pbp$POSSESSION<-pbp$Team_id

#team rebound
pbp$POSSESSION[is.na(pbp$Team_id)& !is.na(pbp$Person1)& pbp$Person1%in% unique(gamelineup$Team_id)]<-
  pbp$Person1[is.na(pbp$Team_id)& !is.na(pbp$Person1)& pbp$Person1%in% unique(gamelineup$Team_id)]

#subs, timeouts, game stoppage don't affect possession
pbp$POSSESSION[is_sub]<-NA
pbp$POSSESSION[pbp$Event_Msg_Type==9| pbp$Person1==0]<-NA

#a foul means the non-fouling team is in posession
#an offensive foul means the fouling team is in posession
pbp$POSSESSION[pbp$Event_Msg_Type==6& pbp$Event_Msg_Type!=4& !is.na(pbp$POSSESSION)]<-
  sapply(pbp$POSSESSION[pbp$Event_Msg_Type==6& pbp$Event_Msg_Type!=4& !is.na(pbp$POSSESSION)], function(x) unique(gamelineup$Team_id[gamelineup$Team_id!=x]))


pbp$nextPOSS<-lead(pbp$POSSESSION)
pbp[ c("POSSESSION","nextPOSS")]<-sapply(pbp[,  c("POSSESSION","nextPOSS")], na.locf, fromLast=F, na.rm=F)
pbp$possComplete<-ifelse(pbp$POSSESSION!=pbp$nextPOSS,1, 0)
pbp$possComplete[pbp$Event_Msg_Type==12]<-0
pbp$possComplete[pbp$Event_Msg_Type==13]<-1

home<-tail(gamelineup$Team_id,1)
pbp[, unique(gamelineup$Person_id[gamelineup$Team_id!=home]) ][pbp[, unique(gamelineup$Person_id[gamelineup$Team_id!=home]) ]==1]<-(-1)

pbp[, c("HomePTS","AwayPTS")]<-NA

###Need help figuring out how to add up the scores###
pbp$HomePTS[!is.na(pbp$Option1)]<-as.numeric(sapply(strsplit(pbp$Option1[!is.na(pbp$Option1)], " - "),`[[`, 2))
pbp$AwayPTS[!is.na(pbp$Option1)]<-as.numeric(sapply(strsplit(pbp$Option1[!is.na(pbp$Option1)], " - "),`[[`, 1))
pbp[1, c("HomePTS","AwayPTS")]<-0
pbp[, c("HomePTS","AwayPTS")]<-sapply(pbp[,  c("HomePTS","AwayPTS")], na.locf, fromLast=F)
pbp[ ,c("HomePTS","AwayPTS")]<-sapply(pbp[,  c("HomePTS","AwayPTS")], as.numeric)


###OVERTIME ERROR FIX#########
#automatically fix OT problems by checking to see if there is a player off by exactly 5 minutes--happens if played whole OT w.o. stat

errors<-unname(which(rowSums(pbp[, unique(gamelineup$Person_id)])!=0))
if(length(errors)>0 ){ #if there is an error
  
  periodError<-unique(pbp$Period[errors])
  if(length(periodError)==1 &periodError>=5)  { #if it is an OT error
    stints<-getStint(gamelineup, pbp)
    
    minSums<-sapply(paste("X",unique(gamelineup$Person_id), sep=""), function(x) sum(stints$TimeEnd[stints[, x]!=0]-stints$TimeStart[stints[, x]!=0]))
    minSums<-data.frame(MINS=unname(minSums), ID=gsub("X", "", names(minSums)))
    minSums$gamelinupMIN<-gamelineup$MIN[match(minSums$Game_id, gamelineup$Person_id)]
    minSums$gamelineupMIN<-as.numeric(sapply(strsplit(minSums$gamelineupMIN,":"),  `[[`, 1))+as.numeric(sapply(strsplit(minSums$gamelineupMIN,":"),  `[[`, 2))/60
    minSums$gamelineupDiff<-minSums$MINS-minSums$gamelineupMIN
    fix<- minSums$ID[abs(minSums$boxDiff)>4.9 &abs(minSums$boxDiff)<5.1 ]
    nba[nba$Quarter==quarterError,fix]<-1
  }
  
}
pbp[, unique(gamelineup$Person_id[gamelineup$Team_id!=home]) ][pbp[, unique(gamelineup$Person_id[gamelineup$Team_id!=home]) ]==1]<-(-1)

#homeStart and timeStart should begin at the line before the lineChange, homeend and lineend should be the line before the next linechange, 
#possessions should not include the line before the linechange and should go to the line before the next linechange

getStint<-function(gamelineup, pbp){
  lineChange<-colDiffs(as.matrix(pbp[, unique(gamelineup$Person_id)]))
  lineChange<-c(1, which(sapply(1:nrow(lineChange), function(x) sum(lineChange[x, ]!=0)) !=0))
  
  stints<-data.frame(rbindlist(lapply( 1:length(lineChange), function(i){
    if(i!=1){
      start<-lineChange[i]+1
    } else{
      start<-1
    }
    if(i==length(lineChange)){
      end<-nrow(pbp)
    }else{
      end<-lineChange[i+1]
    }
    data<-data.frame(pbp[start, unique(gamelineup$Person_id)])
    if(start!=1){
      data$HomeStart<-pbp$HomePTS[(start-1)]
      data$AwayStart<-pbp$AwayPTS[(start-1)]
      data$TimeStart<-pbp$TimeElapsed[start-1]
    } else{
      data$HomeStart<-pbp$HomePTS[(start)]
      data$AwayStart<-pbp$AwayPTS[(start)]
      data$TimeStart<-pbp$TimeElapsed[start]
    }
    data$HomeEnd<-pbp$HomePTS[(end)]
    data$AwayEnd<-pbp$AwayPTS[(end)]
    data$TimeEnd<-pbp$TimeElapsed[end]
    
    data$POSSESSIONS<-sum(pbp$possComplete[(start):(end)])
    data$HomePOSS<-sum(pbp$possComplete[start:end][which(pbp$POSSESSION[start:end]==tail(gamelineup$Team_id,1)) ])
    data$AwayPOSS<-data$POSSESSIONS-data$HomePOSS
    
    data
  })))
  stints$Game_id<-gamelineup$Game_id[1]
  stints
}
plot( rowSums(pbp[, unique(gamelineup$Person_id)]), main=k)