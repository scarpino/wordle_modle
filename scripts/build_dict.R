#SV Scarpino
#Building Eordle dictionary
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


######
#Data#
######
raw_site <- read_html("https://www.powerlanguage.co.uk/wordle/main.db1931a8.js") %>% html_text() #read in the raw JS that has the word dictionaries 

#a kind of lazy way to zero in on the dictionaries
split_site <- strsplit(x = raw_site, split = "[;]")[[1]]
find_dictionaries <- grep(pattern = "var Aa=", x = split_site)
dictionaries <- split_site[find_dictionaries]
split_dictionaries <- strsplit(x = dictionaries, split = "]")[[1]] 

#non goal words
find_non_goal_words <- grep(pattern = "La=", x = split_dictionaries)
non_goal_words_raw <- split_dictionaries[find_non_goal_words]
non_goal_words_combine <- gsub(pattern = ",La=[", replacement = "", non_goal_words_raw, fixed = TRUE)
non_goal_words_split <- strsplit(x = non_goal_words_combine, split = ",")[[1]] 
non_goal_words <- gsub(pattern = "[^A-Za-z0-9]", replacement = "", non_goal_words_split)

#goal words
find_goal_words <- grep(pattern = "var Aa=", x = split_dictionaries)
goal_words_raw <- split_dictionaries[find_goal_words]
goal_words_combine <- gsub(pattern = "var Aa=[", replacement = "", goal_words_raw, fixed = TRUE)
goal_words_split <- strsplit(x = goal_words_combine, split = ",")[[1]] 
goal_words <- gsub(pattern = "[^A-Za-z0-9]", replacement = "", goal_words_split)

all_words <- c(non_goal_words, goal_words)