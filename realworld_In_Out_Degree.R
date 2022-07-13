library(dplyr)
rm(list=ls())
getwd()
setwd('C:/data/Hsi')
load('realworld.matrix.RData')

#In-degree or number of prey of all species in the network
InDegree <- function(M){
  return(colSums(M));
}

#Out-degree or number of predators of all species in the network
OutDegree <- function(M){
  return(rowSums(M));
}

#realworld InDegree
realworld_indegree <- list()
for(i in 1:74){
  realworld_indegree[[i]] <- InDegree(realworld.matrix[[i]])
}

#realworld OutDegree
realworld_outdegree <- list()
for (i in 1:74){
  realworld_outdegree[[i]] <- OutDegree(realworld.matrix[[i]])
}

save(realworld_indegree,realworld_outdegree,file = 'realworld_inout_degree.RData')

realworld_meanInDegree <- c()
for(i in 1:74){
  realworld_meanInDegree <- c(realworld_meanInDegree,mean(realworld_indegree[[i]]))
}

realworld_meanOutDegree <- c()
for(i in 1:74){
  realworld_meanOutDegree <- c(realworld_meanOutDegree,mean(realworld_outdegree[[i]]))
}     

realworld_mean_inout_degree <- as.data.frame(realworld_meanInDegree)
realworld_mean_inout_degree$realworld_meanOutDegree <- realworld_meanOutDegree
write.csv(realworld_mean_inout_degree,file = 'realworld_mean_inout_degree.csv')

#Degree
realworld_Degree <- list()
for(i in 1:74){
  realworld_Degree[[i]] <- realworld_outdegree[[i]]+realworld_indegree[[i]]
}
realworld_list_Degree <- save(realworld_Degree,file = 'realworld_list_Degree.RData')

realworld_total_Degree <- c()
for(i in 1:74){
  realworld_total_Degree <- c(realworld_total_Degree,realworld_Degree[[i]])
}

realworld_total_Degree <- as.data.frame(realworld_total_Degree)
write.csv(realworld_total_Degree,file = 'realworld_total_Degree.csv')


