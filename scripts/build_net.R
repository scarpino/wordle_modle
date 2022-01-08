#SV Scarpino
#Building Wordle network
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
do_plot <- FALSE
do_save <- FALSE
timestamp <- as.numeric(Sys.time())

######
#Data#
######
source("build_dict.R")

##########
#Analysis#
##########
d_dist <- stringdistmatrix(all_words, all_words, useNames = TRUE)
d_weight <- 1/d_dist
diag(d_weight) <- 0
g <- graph.adjacency(d_weight, weighted = "weight")

deg <- strength(g)
cent <- eigen_centrality(g)

net_data <- list("network" = g, "centrality" = cent, "degree" = deg)

if(do_save == TRUE){
  save(net_data, file = paste0(timestamp, "_wordlenet.RData"))
}

if(do_plot == TRUE){
  png("wordle_net.png")
  plot(g, vertex.size = 1, vertex.label = NA, color = "#00000075", edge.arrow.mode = 0, edge.width = deg/max(deg), edge.color = "#00000075")
  dev.off()
}
