#SV Scarpino
#Playing Wordle Randomly
#Jan 13th 2022

###########
#Libraries#
###########
library(rvest)
library(stringdist)
library(igraph)
library(dplyr)
library(ggplot2)
library(wesanderson)

#########
#Globals#
#########
run_all <- FALSE #run all words set to TRUE
run_word <- ((as.numeric(round(Sys.time() - strptime("2021-06-19", format = "%Y-%m-%d")), unit = "days"))-6):as.numeric(round(Sys.time() - strptime("2021-06-19", format = "%Y-%m-%d")), unit = "days")  #run a specific word (need run_all == FALSE), set as is will run today's word.
max_tries <- 15 #max number of tries to make sure the while loop doesn't go forever
N <- 1000 #number of starts
store_dictionary_length <- TRUE #set TRUE to store the length of each dictionary everytime a word is played, this slows things down a bit.
start_date <- strptime("2021-06-19", format = "%Y-%m-%d")

######
#Data#
######
goal_words_df <- read.csv("../data/1641830985.33453_goal_wordles.csv")
goal_words <- as.character(goal_words_df[,1])
non_goal_words_df <- read.csv("../data/1641830985.33453_non_goal_wordles.csv")
non_goal_words <- as.character(non_goal_words_df[,1])
all_words <- c(goal_words, non_goal_words)


###########
#Acc Funcs#
###########
source("acc_funcs.R")

######
#Play#
######
if(run_all == TRUE){
  loop_var <- 1:length(goal_words)
}else{
  loop_var <- run_word
}

pb <- txtProgressBar(1, length(loop_var), style=3)
counter <- 1
tries <- c()
dates <- c()
big_words_remaining <- c()
big_tries <- c()
big_dates <- c()
for(w in loop_var){
  goal <- goal_words[w]
  date_w <- start_date + w * 60*60*24
  date_w <- as.character(date_w)
  date_w <- substr(date_w, 1, 10)
  
  for(rambo in 1:N){
    dictionary <- all_words
    status <- "sad"
    i <- 0
    words_tried <- c()
    words_remaining <- length(dictionary)
    while(status != "victory" & i < max_tries){
      test_word <- sample(x = dictionary, size = 1)
      
      words_tried <- c(words_tried, test_word)
      
      mt.i <- match_words(word1 = goal, word2 = test_word)
      
      if(length(mt.i$match) == nchar(goal)){
        status <- "victory"
      }else{
        dictionary <- get_next_dict(dictionary = dictionary, word_match = mt.i, word = test_word, i = i)
        dictionary <- dictionary$dictionary
        
        if(length(which(dictionary == goal)) != 1){
          stop("goal word removed from network")
        }
        words_remaining <- c(words_remaining, length(dictionary))
        i <- i + 1
      }
    }
    if(store_dictionary_length == TRUE){
      big_words_remaining <- c(big_words_remaining, words_remaining)
      big_tries <- c(big_tries, 1:length(words_remaining))
      big_dates <- c(big_dates, rep(date_w, length(words_remaining)))
    }
    tries <- c(tries, i)
    dates <- c(dates, date_w)
  }
  setTxtProgressBar(pb, counter)
  counter <- counter + 1
}

plot.dat <- data.frame(dates, tries)
plot.dat$dates <- as.Date(plot.dat$dates, format = "%Y-%m-%d")
plot.dat$dates_plot <- format(plot.dat$dates, format = "%b-%d")
by_mu <- by(data = plot.dat$tries, INDICES = plot.dat$dates, FUN = mean, na.rm = TRUE)
plot.dat$Mean <- rep(as.numeric(by_mu), each = N)
cols <- wes_palette(name = "Royal1", n = 6, type = "continuous")

ggplot(plot.dat, aes(x = as.factor(dates_plot), y = tries, fill = Mean)) + geom_boxplot(outlier.shape = NA) + geom_jitter(color=cols[1], size=0.4, alpha=0.2) + scale_fill_gradient2(low = cols[1], mid = cols[2], high = cols[3], midpoint = 5, guide_legend(title = "Mean guesses")) + xlab("2022") + ylab("Number of guesses needed to win") + theme(legend.position = "none", legend.key = element_rect(fill = "#f0f0f0"), legend.background = element_rect(fill = "#ffffffaa", colour = "black"), panel.background = element_rect(fill = "white", colour = "black"), axis.text.y = element_text(colour = "black", size = 14), axis.text.x = element_text(colour = "black", size = 10), axis.title = element_text(colour = "black", size = 15), panel.grid.minor = element_line(colour = "#00000050",linetype = 3), panel.grid.major = element_line(colour = "#00000060", linetype = 3)) + scale_y_continuous(expand = c(0.01,0.01), limits = c(0, max(plot.dat$tries))) + geom_hline(yintercept = 6, linetype = "dashed", color = cols[4], size = 1) + ggtitle(label = paste0("Wordle difficulty: week of ", format(min(plot.dat$dates), "%b %d, %Y")))

if(store_dictionary_length == TRUE){
  plot.dat.2 <- data.frame(big_dates, big_tries, big_words_remaining)
  
  ggplot(plot.dat.2, aes(x = as.factor(big_tries), y = big_words_remaining))  + geom_jitter(color=cols[1], size=0.4, alpha=0.2) + geom_boxplot(outlier.shape = NA, fill = cols[2], alpha = 0.6) + xlab("Number of guesses") + ylab("Number of remaining words (log 10 scale)") + theme(legend.position = "none", legend.key = element_rect(fill = "#f0f0f0"), legend.background = element_rect(fill = "#ffffffaa", colour = "black"), panel.background = element_rect(fill = "white", colour = "black"), axis.text.y = element_text(colour = "black", size = 14), axis.text.x = element_text(colour = "black", size = 10), axis.title = element_text(colour = "black", size = 15), panel.grid.minor = element_line(colour = "#00000050",linetype = 3), panel.grid.major = element_line(colour = "#00000060", linetype = 3)) + scale_y_log10(expand = c(0.01,0.01)) + ggtitle("Wordle: Rapid decrease in dictionary size with random guesses")
}
