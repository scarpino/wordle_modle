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
run_word <- 206:212 #run a specific word (need run_all == FALSE)
max_tries <- 15 #max number of tries to make sure the while loop doesn't go forever
N <- 1000 #number of starts
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
for(w in loop_var){
  goal <- goal_words[w]
  date_w <- start_date + w * 60*60*24
  date_w <- as.character(date_w)
  date_w <- substr(date_w, 1, 10)
  
  for(rambo in 1:N){
    dictionary <- all_words
    status <- "sad"
    i <- 1
    words_tried <- c()
    
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
        
        i <- i + 1
      }
    }
    tries <- c(tries, i)
    dates <- c(dates, date_w)
  }
  counter <- counter + 1
  setTxtProgressBar(pb, counter)
}

plot.dat <- data.frame(dates, tries)
plot.dat$dates <- as.Date(plot.dat$dates, format = "%Y-%m-%d")
plot.dat$dates <- format(plot.dat$dates, format = "%b-%d")
by_med <- by(data = plot.dat$tries, INDICES = plot.dat$dates, FUN = median, na.rm = TRUE)
plot.dat$Median <- rep(as.numeric(by_med), each = N)
cols <- wes_palette(name = "Royal1", n = 6, type = "continuous")

ggplot(plot.dat, aes(x = as.factor(dates), y = tries, fill = as.factor(Median))) + geom_boxplot() + scale_fill_manual(values =  cols, guide_legend(title = "Median guesses")) + xlab("2021") + ylab("Number of words needed to win") + theme(legend.position = c(0.15,0.1), legend.key = element_rect(fill = "#f0f0f0"), legend.background = element_rect(fill = "#ffffffaa", colour = "black"), panel.background = element_rect(fill = "white", colour = "black"), axis.text.y = element_text(colour = "black", size = 14), axis.text.x = element_text(colour = "black", size = 10), axis.title = element_text(colour = "black", size = 15), panel.grid.minor = element_line(colour = "#00000050",linetype = 3), panel.grid.major = element_line(colour = "#00000060", linetype = 3)) + scale_y_continuous(expand = c(0.01,0.01), limits = c(0, max(plot.dat$tries))) + geom_hline(yintercept = 6, linetype = "dashed", color = "#4d4d4d", size = 1.1)
