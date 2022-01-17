#SV Scarpino
#Acc. Functions for Playing Wordle
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

###########
#Functions#
###########
match_words <- function(word1, word2){
  split1 <- strsplit(x = word1, split = "")[[1]]
  split2 <- strsplit(x = word2, split = "")[[1]]
  
  exact_match <- which(split2 == split1)
  in_word <- which(split2 %in% split1)
  
  filter_exact <- which(in_word %in% exact_match)
  if(length(filter_exact) > 0){
    in_word_filt <- in_word[-filter_exact]
  }else{
    in_word_filt <- in_word
  }
  
  if(length(exact_match) > 0 & length(in_word_filt) > 0){
    filter_exact_not_present <- which(!split2[in_word_filt] %in% split1[-exact_match])
    if(length(filter_exact_not_present) > 0){
      in_word_filt_final <- in_word_filt[-filter_exact_not_present]
    }else{
      in_word_filt_final <- in_word_filt
    }
  }else{
    in_word_filt_final <- in_word_filt
  }
  
  out <- list("match" = exact_match, "present" = in_word_filt_final)
  
  return(out)
}

find_exact_letters <- function(sep_word, positions, letters){
  if(length(sep_word) != length(letters)){
    stop("word is not properly separated into letters")
  }
  keep <- 1
  i <- 1
  while(keep == 1 & i <= length(positions)){
    if(sep_word[positions[i]] != letters[positions[i]]){
      keep <- 0
    }else{
      i <- i + 1
    }
  }
  return(keep)
}

find_words_with_wrong_letters <- function(sep_word, word_match, letters){
  out_letters <- letters[which(! letters %in% letters[c(word_match$match, word_match$present)])]
  if(length(which(out_letters %in% sep_word)>0)){
    keep <- 0
  }else{
    keep <- 1
  }
  return(keep)
}

find_present_letters <- function(sep_word, present, exact, letters){
  if(length(sep_word) != length(letters)){
    stop("word is not properly separated into letters")
  }
  keep <- 1
  while(keep == 1 & i < length(letters)){
    if(i %in% present | i%in% exact){
      if(length(which(sep_word %in% letters[i]))==0){
        keep <- 0
      }else{
        i <- i + 1
      }
    }else{
      test_pres <- which(sep_word %in% letters[i])
      if(length(test_pres)!=0 & length(which(test_pres %in% exact)) != length(test_pres)){
        keep <- 0
      }else{
        i <- i + 1
      }
    }
  }
  return(keep)
}

get_words <- function(dictionary, word_match, word){
  words.in <- strsplit(x = dictionary, split = "")
  
  keep_words_present <- lapply(X = words.in, FUN = find_present_letters, present = word_match$present, exact = word_match$match, letters = word)
  words.in <- words.in[which(keep_words_present == 1)]
  
  if(length(word_match$match) > 0){
    keep_words_exact <- lapply(X = words.in, FUN = find_exact_letters, positions = word_match$match, letters = word)
    if(sum(unlist(keep_words_exact)) == 0){
      stop("exact match reported, but not found in remaining words")
    }
    words.in <- words.in[which(keep_words_exact == 1)]
  }
  
  if(length(word_match$present) > 0 | length(word_match$match) > 0){
    keep_words_correct_letters <- lapply(X = words.in, FUN = find_words_with_wrong_letters, word_match = word_match, letters = word)
    words.in <- words.in[which(keep_words_correct_letters == 1)]
  }
  
  return(words.in)
}

build_net <- function(graph_object, word_match, word){
  graph.2 <- graph_object$network
  vertex.names <- get.vertex.attribute(graph_object$network, "name")
  
  words.to.keep <- get_words(dictionary = vertex.names, word_match = word_match, word = word)
  
  words.to.keep <- lapply(words.to.keep, paste, collapse = "")
  word.to.remove <- paste(word, collapse = "")
  
  vertices.to.remove <- which(!vertex.names %in% unlist(words.to.keep))
  vertices.to.remove <- c(vertices.to.remove, which(vertex.names == word.to.remove ))
  graph.2.filt <- delete_vertices(graph = graph.2, v = vertices.to.remove)
  
  return(graph.2.filt)
}

build_dict <- function(dictionary, word_match, word){
  words.to.keep <- get_words(dictionary = dictionary, word_match = word_match, word = word)
  
  words.to.keep <- lapply(words.to.keep, paste, collapse = "")
  word.to.remove <- paste(word, collapse = "")
  
  words.to.remove <- which(!dictionary %in% unlist(words.to.keep))
  words.to.remove <- c(words.to.remove, which(dictionary == word.to.remove ))
  dictionary.2 <- dictionary[-words.to.remove]
  
  return(dictionary.2)
}

get_next_net <- function(graph_object, word_match, word, i){
  word <- strsplit(x = word, split = "")[[1]]
  new_net <- build_net(graph_object = graph_object, word_match = word_match, word = word)
  new_cent <- eigen_centrality(new_net)
  new_deg <- degree(new_net)
  out <- list("network" = new_net, "centrality" = new_cent, "degree" = new_deg)
  return(out)
}

get_next_dict <- function(dictionary, word_match, word, i){
  word <- strsplit(x = word, split = "")[[1]]
  new_dict <- build_dict(dictionary = dictionary, word_match = word_match, word = word)

  out <- list("dictionary" = new_dict)
  return(out)
}