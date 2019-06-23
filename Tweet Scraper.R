##############################################

# Script that scrapes tweets from the public twitter api and saves them
# To do so search terms need to be provided

#############################################

########### Load packages ###########
library(twitteR)
library(dplyr)

########## initialize functions ##########

get_tweets <- function(search_term, start_date = Sys.Date()-1, end_date = Sys.Date(), location = '37.706017,-92.008812,1700mi', language = "en", 
                      sleep_time = 60, tweets_per_call = 100)
{
  twitter_df <- data.frame()
  cur_max_ID <- NULL
  cur_nr_tweets_queried <- tweets_per_call 
  
  while((cur_nr_tweets_queried-tweets_per_call) == 0) {
    if(as.numeric(getCurRateLimitInfo()$remaining[64]) > (tweets_per_call/50)) {
      search_results <- searchTwitter(search_term,n=tweets_per_call,lang=language,  geocode=location, since = as.character(start_date), until = as.character(end_date)
                                  , maxID = cur_max_ID)
      twitter_df_temp <- twListToDF(search_results) 
      twitter_df <- union_all(twitter_df, twitter_df_temp)
      cur_max_ID <- twitter_df$id[nrow(twitter_df)]
      cur_nr_tweets_queried <- nrow(twitter_df_temp)
    } else {
      print(paste0("Maximum number of queries reached, sleeping for ", sleep_time, " seconds."))
      Sys.sleep(sleep_time)
    }
  }
  twitter_df <- distinct(twitter_df)
  save(twitter_df, file = paste0(getwd(),"\\data\\tweets raw\\", search_term, as.character(start_date),".RData"))
}


########## Init ###########

# Load a list of search terms 
search_terms <- read.csv2("search terms.csv", sep = ";", header = F, col.names = "search_terms", colClasses = "character")

#If it not yet exists create a sub-directory for the data
dir.create(file.path(getwd(), "data\\tweets raw"), showWarnings = FALSE)

# Get authentication keys and connect to the API
twitter_api_auth <- read.csv2("twitter api auth.csv", sep = ";", header = T, row.names = 1, colClasses = "character")

consumer_key <- twitter_api_auth["consumer_key", 1]
consumer_secret <- twitter_api_auth["consumer_secret", 1]
access_token <- twitter_api_auth["access_token", 1]
access_secret <- twitter_api_auth["access_secret", 1]

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)


########### Get tweets ############
lapply(search_terms$search_terms, get_tweets)

