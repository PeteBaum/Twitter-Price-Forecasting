##############################################

# Script that processes the tweets that are scraped by the scraper plus the price data

#############################################

########### Load packages ###########
Sys.setenv(LANG = "en")

library(readr)
library(dplyr)
library(lubridate)
library(tm)
library(qdap)
library(stringr)

########## initialize functions ##########
process_price_data <- function(path = paste0(getwd(), "\\data\\price data raw\\"), start_date = as.Date("2000-01-01"), end_date = as.Date("2040-01-01"))
{
  raw_price_data <- read.csv2(paste0(path, "\\example data raw.csv"), dec = ".")
  price_data$Date <- as.Date(price_data$Date, tryFormats = c("%d-%m-%Y"))
  price_data <- dplyr::mutate(price_data, date_time = as.POSIXct(paste(Date, Time, sep=" "), tzone = "UTC"))
  price_data <- dplyr::filter(price_data, Date >= start_date, Date < end_date)
  price_data <- price_data[order(price_data$Date),]
  save(price_data, file = paste0(getwd(), "\\data\\price data processed\\processed price data.RData"))
}

process_raw_tweets <- function(path = paste0(getwd(), "\\data\\tweets raw\\"),  start_date = as.Date("2000-01-01"), end_date = as.Date("2040-01-01"), include_retweets = F)
{
  filelist <- list.files(path)
  tweets <- data.frame()
  
  for(i in 1:length(filelist))
  {
    env <- new.env()
    tweets_temp <- load(paste0(path, filelist[i]), env)
    tweets_temp <- env[[tweets_temp]]
    ifelse(include_retweets == F, tweets_temp <- dplyr::filter(tweets_temp, isRetweet == "FALSE"), "")
    tweets <- rbind(tweets_temp, tweets)
  }
  
  tweets <- dplyr::filter(tweets, created >= start_date, created < end_date)
  
  tweets_processed <- dplyr::select(tweets, created, text)
  tweets_processed$text <- gsub("[^\\s]*@[^\\s]*", "", tweets_processed$text, perl=T) 
  tweets_processed$text <- gsub("[^\\s]*http[^\\s]*", "", tweets_processed$text, perl=T)
  tweets_processed$text <- str_replace_all(tweets_processed$text,"[^[:graph:]]", " ") 
  tweets_processed$text <- removePunctuation(tweets_processed$text)
  tweets_processed$text <- removeNumbers(tweets_processed$text)
  tweets_processed$text <- tolower(tweets_processed$text)
  tweets_processed$text <- removeWords(tweets_processed$text, stopwords("en"))
  tweets_processed$text <- stripWhitespace(tweets_processed$text)
  tweets_processed$text <- stemDocument(tweets_processed$text, language = "english")
  
  save(tweets_processed, file = paste0(getwd(), "\\data\\tweets processed\\processedTweets.RData"))
}

create_model_input <- function(path_tweets = paste0(getwd(), "\\data\\tweets processed\\"), path_price_data = paste0(getwd(), "\\data\\price data processed\\"),  
                               tweets_per_obs = 100, word_count = 100, tweet_sample = 500000)
{
  load(paste0(path_tweets, "processedTweets.RData"))
  load(paste0(path_price_data, "processed price data.RData"))
  ifelse(tweet_sample == 0, "", tweets_processed <- sample_n(tweets_processed, tweet_sample))
  tweets_processed <- dplyr::filter(tweets_processed, created >= min(price_data$date_time), created < max(price_data$date_time))
  price_data <- dplyr::filter(price_data, date_time >= min(tweets_processed$created), date_time < max(price_data$date_time))
  term_freq <- as.data.frame(termFreq(tweets_processed$text))
  term_freq$term <- row.names(term_freq)
  names(term_freq) <- c("count", "term")
  term_freq <- term_freq[order(term_freq$count, decreasing = T),]
  remove_terms <- term_freq$term[1:word_count]
  
  inputs <- list()
  
  pb <- txtProgressBar(min = 0, max = length(price_data), style = 3) #status bar of loop
  
  for(i in 1:nrow(price_data))
  {
    setTxtProgressBar(pb, i)
    starttime <- price_data$date_time[i] - 30*60
    endtime <- starttime - 30*60
    
    tweets_temp <- dplyr::filter(tweets_processed, created >= endtime, created <= starttime)

    if(nrow(tweets_temp) == 0)
    { inputs[[i]] <- matrix()}
    else
    {
      vs <- VectorSource(tweets_temp$text)
      corpus <- Corpus(vs)
      tweets_temp_dtm <- DocumentTermMatrix(corpus, control = list(dictionary = remove_terms))
      tweets_temp_matrix <- as.matrix(tweets_temp_dtm)
      tweets_temp_matrix <-  tweets_temp_matrix[,order(colnames(tweets_temp_matrix))]
      tweets_temp_df <- as.data.frame(tweets_temp_matrix)
      row_sums <- sort(rowSums(tweets_temp_df), decreasing = T)
      tweets_temp_df <- tweets_temp_df[names(row_sums[1:min(nrow(tweets_temp_df),tweets_per_obs)]),]
      tweets_temp_matrix <- as.matrix(tweets_temp_df)
      inputs[[i]] <- tweets_temp_matrix
    }
  }
  
  save(inputs, file = paste0(getwd(),"\\data\\model input\\input_matrices.RData"))
  save(price_data, file = paste0(getwd(),"\\data\\model input\\input_prices.RData"))
}


########## Init ###########

#If it not yet exists create a sub-directory for the data
dir.create(file.path(getwd(), "data\\tweets processed"), showWarnings = FALSE)
dir.create(file.path(getwd(), "data\\price data processed"), showWarnings = FALSE)
dir.create(file.path(getwd(), "data\\model input"), showWarnings = FALSE)


####### Process Data ##########
process_price_data()

process_raw_tweets()

create_model_input()