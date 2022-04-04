##RUN AND DO STUFF HERE

#clear memory
rm(list=ls())

#set directory for AWS instance
setwd("~/fly_the_w_bot/")

#set directory for local version
# setwd("~/Dropbox/Side Projects/fly_the_w_bot")

#source locally from aws or relative path
source("code/r/0_creds.R")
source("code/r/1_load.R")

#source from github
#source_url("https://raw.githubusercontent.com/bradweiner/fly_the_w_bot/master/code/r/3_clean.R")

shut_down_signal <- FALSE

games <- get_mlb_data()

get_cubs_game_id()

get_live_feed_data()

create_live_feed_df()

head(cubs)

# Shut down server if Cubs aren't playing today

if(nrow(cubs) == 0){
        
        stop_ec2_instance()
        
}

# Post Tweet if Cubs Won

if (cubs$cubs_w_or_l == "Cubs Won" &
    cubs$game_status %in% complete_game_statuses &
    cubs$id %ni% already_tweeted$id &
    nchar(cubs$winpitch) > 0){
        
        create_w_tweet()
        
        post_tweet(
                status = w_tweet,
                token = token,media = "cubs_w_flag.png")
        
        already_tweeted <- rbind(already_tweeted,cubs)
        
        saveRDS(already_tweeted,"data/transformed/already_tweeted_2022.rds")
        
        shut_down_signal <- TRUE
}


# Shut Down Server if Cubs Lost

###ADD A SYSTEM SHUT DOWN SIGNAL IF THE GAME IS OVER AND THE CUBS LOST

if(cubs$cubs_w_or_l == "Cubs Lost" &
   cubs$game_status %in% complete_game_statuses){
        
        shut_down_signal <- TRUE
}

# Add Row to croncheck log

add_to_croncheck()

# Remove Rtweet tokens

system(command = "rm /home/rstudio/.rtweet_token*")

# Shut down server if shut down signal is TRUE

if(shut_down_signal == TRUE){
        
        ## put code here to push data files to git
        
        ## wait ten seconds
        
        # stop ec2 instnace
        stop_ec2_instance()
        
        
}

rm(list=ls())
