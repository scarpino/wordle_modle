#SV Scarpino
#Playing Wordle
#Jan 6th 2022

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
run_word <- 206 #run a specific word (need run_all == FALSE)
max_tries <- 15 #max number of tries to make sure the while loop doesn't go forever

######
#Data#
######
#source("build_dict.R") #the website location for the JS keeps shifting.
goal_words_df <- read.csv("../data/1641830985.33453_goal_wordles.csv")
goal_words <- as.character(goal_words_df[,1])
non_goal_words_df <- read.csv("../data/1641830985.33453_non_goal_wordles.csv")
non_goal_words <- as.character(non_goal_words_df[,1])
all_words <- c(goal_words, non_goal_words)

get_network_file <- list.files("../network", full.names = TRUE)

if(length(get_network_file) != 1){
  stop("either not network is present or more than 1 network is present in the network dir.")
}

load(get_network_file)

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
  
  pb <- txtProgressBar(1, length(loop_var), style=3)
}else{
  loop_var <- run_word
  
  pb <- txtProgressBar(1, length(loop_var)+1, style=3)
}



for(w in loop_var){
  graph_object <- net_data
  cent <- graph_object$centrality$vector
  goal <- goal_words[w]
  find_goal_word <- which(names(graph_object$centrality$vector) == goal)
  
  goal_cent[w] <- cent[find_goal_word]
  
  status <- "sad"
  
  i <- 1
  words_tried <- c()
  
  while(status != "victory" & i < max_tries){
    if(length(graph_object$degree) == length(net_data$degree)){
      test_word <- names(cent)[order(cent, decreasing = TRUE)][i]
    }else{
      test_word <- names(cent)[which.max(cent)]
    }
    
    words_tried <- c(words_tried, test_word)
    
    mt.i <- match_words(word1 = goal, word2 = test_word)
    
    if(length(mt.i$match) == nchar(goal)){
      status <- "victory"
    }else{
      graph_object <- get_next_net(graph_object = graph_object, word_match = mt.i, word = test_word, i = i)
      
      cent <- graph_object$centrality$vector
      
      if(length(which(names(cent) == goal)) != 1){
        stop("goal word removed from network")
      }
      
      i <- i + 1
    }
  }
  success_number[w] <- i
  setTxtProgressBar(pb, w)
}
