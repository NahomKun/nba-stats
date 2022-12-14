setwd("/Users/olio/Downloads/Projection/NBA/prev seasons data/")
library(nbastatR)
library(stringi)
library(tidyverse)

#change seasons to desired year to scrape counted stats
gamelogs<-game_logs(
     seasons = c(2022),
     league = "NBA",
     result_types = "player",
     season_types = "Regular Season",
     nest_data = F,
     assign_to_environment = TRUE,
     return_message = T)
#cleaning up 
#remove last 6 columns
logs <- gamelogs[1:(length(gamelogs)-6)] %>%
  select(idGame, slugTeam,slugOpponent,idPlayer,namePlayer,fpts,minutes,pts,ast,treb,stl,blk,tov)
Cleanlogs <- logs %>% nest_by(idGame)
#pull unique Game Ids to scrape Advanced data
Gameids <- unique(Cleanlogs, by ="idGame")
#to speed up scraping process 
Sys.setenv(VROOM_CONNECTION_SIZE=90000000)
##Importing Advanced data
Adv <- box_scores(game_ids = Gameids$idGame,
                  league = "NBA",
                  box_score_types = c("Advanced"),
                  result_types = c("player"),
                  join_data = FALSE,
                  assign_to_environment = TRUE,
                  return_message = T)
Adv <- dataBoxScoreAdvancedPlayerNBA

#save the df to desired location 
write.csv(Adv,"21 Adv.csv", row.names = FALSE)

Pos  <- read.csv("13 POS.csv")
rawData <- merge(Adv, gamelogs, by.x=c("idGame", "idPlayer"), by.y=c("idGame", "idPlayer"))
data <- rawData %>% select(idGame,idPlayer,namePlayer.x,slugOpponent,isB2B,isB2BFirst,isB2BSecond,locationGame,minExact,pctUSG,ratioPIE,fpts)
data <- data %>%rename("Player" = "namePlayer.x") %>% 
  rename("PIE" = "ratioPIE") %>%
  rename("USEAGE" = "pctUSG") %>% 
  rename("MINS" = "minExact") %>% 
  rename("Opp" = "slugOpponent") %>% 
  rename("FP" = "fpts")%>%
  rename("B2B"="isB2B")%>%
  rename("FirstB2B" = "isB2BFirst")%>%
  rename("SecondB2B"= "isB2BSecond")%>%
  rename("HomeOrAway"= "locationGame")


Pos$Player <-word(Pos$Player, 1, 2)
rawData$Player <-word(rawData$namePlayer.x, 1, 2)
Pos$Player <- stri_trans_general(Pos$Player, "Latin-ASCII")
rawData$Player <- stri_trans_general(rawData$Player, "Latin-ASCII")
Pos <- filter(Pos,Pos != "Pos")
Pos$Pos <- sub("-.*","",Pos$Pos)
Pos <- unique(Pos)
Pos$Player <- gsub('\\.', '',Pos$Player)
rawData$Player <- gsub('\\.', '',rawData$Player)

Pos$Player[Pos$Player == "Nicolas Claxton"] <- "Nic Claxton"
Pos$Player[Pos$Player == "Cameron Thomas"] <- "Cam Thomas"
Pos$Player[Pos$Player == "PJ Dozier"] <- "P.J. Dozier"
Pos$Player[Pos$Player == "Juan Hernangomez"] <- "Juancho Hernangomez"
Pos$Player[Pos$Player == "Wesley Iwundu"] <- "Wes Iwundu"



match(rawData$Player,Pos$Player)
Pos$Pos[match(rawData$Player,Pos$Player)] 
rawData$Pos <- Pos$Pos[match(rawData$Player,Pos$Player)] 

na <-rawData[is.na(rawData$Pos), ]

rawData <-rawData[complete.cases(rawData$Pos), ]
data <- data%>% relocate(Pos, .before = Player)
data <- filter(data, FP>=10)

write.csv(rawData,"2015.16.csv",row.names = F)

######################################################
df1<- read.csv("2013.14.csv")
df2<- read.csv("2014.15.csv")
df3<- read.csv("2015.16.csv")
df4<- read.csv("2016.17.csv")
df5<- read.csv("2017.18.csv")
df6<- read.csv("2018.19.csv")
df7<- read.csv("2019.20.csv")
df8<- read.csv("2020.21.csv")


gamelogs<-game_logs(
  seasons = c(2021),
  league = "NBA",
  result_types = "player",
  season_types = "Regular Season",
  nest_data = F,
  assign_to_environment = TRUE,
  return_message = T)

gamelogs <- gamelogs %>%mutate(dbl = if_else(pts>=10 & treb >=10|pts >=10 & ast >=10|pts >=10 & blk >=10|
                                               pts >=10 & stl >=10|treb >=10 & ast >=10|treb >=10 & blk >=10|
                                               treb >=10 & stl >=10|ast >=10 & blk >= 10|ast >= 10 & stl >=10|
                                               blk>= 10 & stl>=10,1,0))%>%
                        relocate(dbl, .after = "pts")

gamelogs <- gamelogs %>%mutate(tdbl = if_else(pts>=10 & treb >=10 & ast >=10|
                                                pts >=10 & treb >=10 & blk >=10|
                                                pts >=10 & treb >=10 & stl >=10|
                                                pts >=10 & ast >=10 & stl >=10|
                                                pts >=10 & ast >=10 & blk >=10|
                                                pts >=10 & blk >=10 & stl >=10|
                                                treb >=10 & ast >=10 & blk >=10|
                                                treb >=10 & ast >=10 & stl >=10|
                                                treb >=10 & blk >=10 & stl >=10|
                                                ast >=10 & blk >= 10 & stl >=10,1,0))%>%
                        relocate(tdbl, .after = "dbl") %>%
                        mutate(dk = pts+0.5*fg3m+1.25*treb+1.5*ast+2*stl+2*blk+1.5*dbl+1.5*tdbl-.5*tov)

gamelogs <- gamelogs[,1:54] 

Adv<- read.csv("20 Adv.csv")

ftot <- merge(Adv,gamelogs, by.x=c("idGame", "idPlayer"), by.y=c("idGame", "idPlayer") ,all.x=TRUE )

ftot <- tot %>% select(idGame,idPlayer,namePlayer.x,slugOpponent,countDaysRestPlayer,locationGame,minExact,pctUSG,ratioPIE,fpts)
ftot <- ftot %>%rename("Player" = "namePlayer.x") %>% 
  rename("PIE" = "ratioPIE") %>%
  rename("USEAGE" = "pctUSG") %>% 
  rename("MINS" = "minExact") %>% 
  rename("Opp" = "slugOpponent") %>% 
  rename("FP" = "fpts")%>%
  rename("Rest"="countDaysRestPlayer")

Pos  <- read.csv("20 POS.csv")

Pos$Player <-word(Pos$Player, 1, 2)
ftot$Player <-word(ftot$namePlayer.y, 1, 2)
Pos$Player <- stri_trans_general(Pos$Player, "Latin-ASCII")
ftot$Player <- stri_trans_general(ftot$Player, "Latin-ASCII")
Pos <- filter(Pos,Pos != "Pos")
Pos$Pos <- sub("-.*","",Pos$Pos)
Pos <- unique(Pos)
Pos$Player <- gsub('\\.', '',Pos$Player)
ftot$Player <- gsub('\\.', '',ftot$Player)

Pos$Player[Pos$Player == "Nicolas Claxton"] <- "Nic Claxton"
Pos$Player[Pos$Player == "Juan Hernangomez"] <- "Juancho Hernangomez"
Pos$Player[Pos$Player == "Wesley Iwundu"] <- "Wes Iwundu"
Pos$Player[Pos$Player == "Enes Kanter"] <- "Enes Freedom"



match(ftot$Player,Pos$Player)
Pos$Pos[match(ftot$Player,Pos$Player)] 
ftot$Pos <- Pos$Pos[match(ftot$Player,Pos$Player)] 

ftot <-ftot[complete.cases(ftot$Pos), ]

na <-ftot[is.na(ftot$Pos), ]


write.csv(ftot,"2020.21.csv",row.names = F)

df13<- df1%>% select(Player,Pos,idGame,idPlayer,slugOpponent,countDaysRestPlayer,locationGame,minExact,pctUSG,ratioPIE,fpts)%>%
              rename("PIE" = "ratioPIE") %>%
              rename("USEAGE" = "pctUSG") %>% 
              rename("MINS" = "minExact") %>% 
              rename("Opp" = "slugOpponent") %>% 
              rename("FP" = "fpts")%>%
              rename("Rest"="countDaysRestPlayer")

data <- rbind.data.frame(df1,df2,df3,df4,df5,df6,df7,df8)
write.csv(data,"cleandata.csv",row.names = F)

test<-df1[,c(15,33,70,75:90)]
df6<-df6[,-c(3,37,42,48,55,67,85:90)]

teamAdv8<- read.csv("TeamAdv20_21.csv", header = T)
##############Opp Adv Stats
match(df8$slugOpponent,teamAdv8$TEAM)
teamAdv8$PACE[match(df8$slugOpponent,teamAdv8$TEAM)] 
df8$OppPace <- teamAdv8$PACE[match(df8$slugOpponent,teamAdv8$TEAM)] 

match(df8$slugOpponent,teamAdv8$TEAM)
teamAdv8$DEFRTG[match(df8$slugOpponent,teamAdv8$TEAM)] 
df8$DEFRTG <- teamAdv8$DEFRTG[match(df8$slugOpponent,teamAdv8$TEAM)]
write.csv(df8,"2020.21.csv",row.names = F)
