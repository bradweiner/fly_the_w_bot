##FUNCTIONS GO HERE

# Load libraries
library(jsonlite)
library(rtweet)
library(tidyverse)
library(magrittr)
library(lubridate)
library(glue)

#clear out rtweet tokens
#system(command = "rm .rtweet_token*")

# Load helpers
'%ni%' <- Negate('%in%')

#load already tweeted and croncheck files

already_tweeted <- readRDS("data/transformed/already_tweeted_2022.rds")
croncheck <- readRDS("data/transformed/croncheck_2022.rds")

get_mlb_data <- function(date_option = "today"){
        
        if (date_option == "today") {
                
        #today game with pasted in date
        today_games <- fromJSON(paste0("http://statsapi.mlb.com/api/v1/schedule/games/?sportId=1&startDate=",
                                       today(),"&endDate=",today()),flatten = TRUE)
        
        } else if (date_option == "yesterday") {
                
        yesterday_games <- fromJSON(paste0("http://statsapi.mlb.com/api/v1/schedule/games/?sportId=1&startDate=",
                                           today()-1,"&endDate=",today()-1),flatten = TRUE)
        
        } else if (date_option == "test") {
                
        test <- fromJSON(paste0("http://statsapi.mlb.com/api/v1/schedule/games/?sportId=1&startDate=",
                                         "2022-03-30","&endDate=","2022-03-30"),flatten = TRUE)
                           
        } else {
                
        today_games <- fromJSON(paste0("http://statsapi.mlb.com/api/v1/schedule/games/?sportId=1&startDate=",
                                               today(),"&endDate=",today()),flatten = TRUE)
        
        }}

#this function gets the Cubs game ID for the day

get_cubs_game_id <- function(){
        
        a <- games[["dates"]][["games"]][[1]]
        a %<>% filter(teams.home.team.name == "Chicago Cubs" | teams.away.team.name == "Chicago Cubs")
        a %<>% slice(1)
        
        a <<- a
        today_cubs_game_id <<- a$gamePk
        
        }
        

get_live_feed_data <- function(){
        
        #live feed #seems like inning and status are in here
        lf <<- fromJSON(paste0("http://statsapi.mlb.com//api/v1.1/game/",today_cubs_game_id,"/feed/live"),flatten=TRUE)
        
}
        

create_live_feed_df <- function(){

#separate out the required data items and build a data frame
        
        gamepk <- lf[["gamePk"]]
        id <- lf[["gameData"]][["game"]][["id"]]
        home_team <- lf[["gameData"]][["teams"]][["home"]][["name"]]
        home_team_abbr <- lf[["gameData"]][["teams"]][["home"]][["teamName"]]
        away_team <- lf[["gameData"]][["teams"]][["away"]][["name"]]
        away_team_abbr <- lf[["gameData"]][["teams"]][["away"]][["teamName"]]
        game_nbr <- lf[["gameData"]][["game"]][["gameNumber"]]
        double_header <- lf[["gameData"]][["game"]][["doubleHeader"]]
        home_win <- a$teams.home.isWinner
        #not sure what this looks like test against 706838 outcome
        # home_loss <- mlb$data$games$game$home_loss
        away_win <- a$teams.away.isWinner
        #not sure what this looks like test against 706838 outcome
        # away_loss <- mlb$data$games$game$away_loss
        away_runs <- lf[["liveData"]][["boxscore"]][["teams"]][["away"]][["teamStats"]][["batting"]][["runs"]]
        home_runs <- lf[["liveData"]][["boxscore"]][["teams"]][["home"]][["teamStats"]][["batting"]][["runs"]]
        #not sure this exists
        # run_diff <- mlb$data$games$game$linescore.r.diff
        game_status <- lf[["gameData"]][["status"]][["detailedState"]]
        game_status_cd <- lf[["gameData"]][["status"]][["statusCode"]]
        status_inning <- lf[["liveData"]][["linescore"]][["currentInning"]]
        inning_state <- lf[["liveData"]][["linescore"]][["inningState"]]
        inning_ord <- lf[["liveData"]][["linescore"]][["currentInningOrdinal"]]
        venue <- lf[["gameData"]][["venue"]][["name"]]
        
        td <- lf[["gameData"]][["datetime"]][["officialDate"]]
        full_td <- lf[["gameData"]][["datetime"]][["dateTime"]]
        game_time <- lf[["gameData"]][["datetime"]][["time"]]
        
        winpitch <- lf[["liveData"]][["decisions"]][["winner"]][["fullName"]]
        losepitch <- lf[["liveData"]][["decisions"]][["loser"]][["fullName"]]
        
        attendance <- lf[["gameData"]][["gameInfo"]][["attendance"]]
        
        
        #force values if NULL
        
        away_win[is.null(away_win)] <- NA
        home_win[is.null(home_win)] <- NA
        inning_ord[is.null(inning_ord)] <- NA
        inning_state[is.null(inning_state)] <- NA
        status_inning[is.null(status_inning)] <- NA
        winpitch[is.null(winpitch)] <- NA
        losepitch[is.null(losepitch)] <- NA
        attendance[is.null(attendance)] <- NA
        
        
        #build a data frame here with all of the vectors captured above
        mlb <<- data.frame(gamepk,
                          today_cubs_game_id,
                          id,
                          td,
                          full_td,
                          game_time,
                          venue,
                          away_team,
                          away_team_abbr,
                          home_team,
                          home_team_abbr,
                          status_inning,
                          inning_ord,
                          inning_state,
                          away_runs,
                          home_runs,
                          game_nbr,
                          double_header,
                          game_status,
                          game_status_cd,
                          away_win,
                          home_win,
                          winpitch,
                          losepitch,
                          attendance)
        
        cubs <<- mlb %>%
                filter(home_team_abbr == "Cubs" | away_team_abbr == "Cubs") %>%
                mutate(cubs_runs = ifelse(home_team_abbr == "Cubs",home_runs,away_runs),
                       opponent_runs = ifelse(home_team_abbr != "Cubs",home_runs,away_runs),
                       cubs_w_or_l = ifelse(cubs_runs > opponent_runs,"Cubs Won",
                                            ifelse(cubs_runs == opponent_runs,"Cubs Tied","Cubs Lost"))) %>%
                as.data.frame(.)
        
        
        
}


start_ec2_instance <- function(){
        
        system(command = paste0("aws ec2 start-instances --instance-ids ",instance_id, " --region ",region),
               intern = F,ignore.stdout = T,ignore.stderr = T)
        
}


stop_ec2_instance <- function(){
        
        system(command = paste0("aws ec2 stop-instances --instance-ids ",instance_id, " --region ",region),
               intern = F,ignore.stdout = T,ignore.stderr = T)
        
        
}

get_ec2_status <- function(){
        
        system(command = paste0("aws ec2 describe-instance-status --instance-ids ",instance_id, " --region ",region),
               intern = F,ignore.stdout = F,ignore.stderr = F)
        
        
}

complete_game_statuses <- c("Final",
                            "Game Over",
                            "Completed Early: Rain")

create_w_tweet <- function(){
        
        w_tweet <<- glue("Cubs Win!\n",
                             {cubs$away_team},
                             " (",
                             {cubs$away_runs},
                             ")\n",
                             {cubs$home_team},
                             " (",
                             {cubs$home_runs},
                             ")\n",
                             "WP: ",
                             cubs$winpitch,
                             "\n",
                             "LP: ",
                             cubs$losepitch,
                             "\n",
                             "Venue: ",
                             cubs$venue,
                             "\n",
                             "Attendance: ",
                             cubs$attendance,
                             "\n",
                             "Date: ",
                             cubs$td,
                             "\n",
                             "#FlyTheWBot")
        
}

#only use me once

create_initial_already_tweeted_df <- function(){
        
        
        games <- get_mlb_data(date_option = "test")
        
        get_cubs_game_id()
        
        get_live_feed_data()
        
        create_live_feed_df()
        
        head(cubs)
        
        #test the following stop procedure
        
        cubs %<>% filter(.,venue == "Mars")
        
        saveRDS(cubs,"data/transformed/already_tweeted_2022.rds")
        
}


create_initial_croncheck_df <- function(){
        
        
        games <- get_mlb_data(date_option = "test")
        
        get_cubs_game_id()
        
        get_live_feed_data()
        
        create_live_feed_df()
        
        #test the following stop procedure
        
        cubs %<>% mutate(timestamp = now(tzone = "US/Mountain")) %>% 
                relocate(timestamp) %>%
                as.data.frame()
        
        cubs %<>% filter(.,venue == "Mars")
        
        saveRDS(cubs,"data/transformed/croncheck_2022.rds")
        
}


add_to_croncheck <- function(){
        
        cc <- cubs %>%
                mutate(timestamp = now(tzone = "US/Mountain")) %>% 
                relocate(timestamp) %>%
                as.data.frame()
        
        croncheck <- bind_rows(croncheck,cc)
        
        saveRDS(croncheck,"data/transformed/croncheck_2022.rds")
        
}




