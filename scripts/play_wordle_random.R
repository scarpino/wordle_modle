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

#########
#Globals#
#########
run_all <- FALSE #run all words set to TRUE
run_word <- 209 #run a specific word (need run_all == FALSE)
max_tries <- 15 #max number of tries to make sure the while loop doesn't go forever
N <- 1000 #number of starts

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
goal_cent <- rep(NA, length(goal_words))
success_number <- rep(NA, length(goal_words))

if(run_all == TRUE){
  loop_var <- 1:length(goal_words)
}else{
  loop_var <- run_word
}

pb <- txtProgressBar(1, N, style=3)
tries <- c()
for(rambo in 1:N){
  dictionary <- all_words
  for(w in loop_var){
    goal <- goal_words[w]
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
    success_number[w] <- i

  }
  
  tries <- c(tries, i)
  setTxtProgressBar(pb, rambo)
}
