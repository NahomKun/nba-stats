#install packages
install.packages("nbastatsR")
install.packages("readr")

#create a folder to set a directory 
setwd("path to folder")
library(nbastatR)
library(readr)


#change seasons to desired year to scrape counted stats 
gamelogs<-game_logs(
     seasons = c(2014),
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

#to speed up scraping 
Sys.setenv(VROOM_CONNECTION_SIZE=9000000000)

##Importing Advanced data
Adv <- box_scores(game_ids = Cleanlogs$idGame,
                  league = "NBA",
                  box_score_types = c("Advanced"),
                  result_types = c("player"),
                  join_data = FALSE,
                  assign_to_environment = TRUE,
                  return_message = T)
Adv <- dataBoxScoreAdvancedPlayerNBA

#save the df to desired location 
write_csv(Adv,"filename.csv")
