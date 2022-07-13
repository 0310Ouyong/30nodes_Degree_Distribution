library(dplyr)
rm(list=ls())
getwd()
setwd('C:/data')

load('EcoBuildersummary30nodes.RData')
load('researchworld_binary_matrix.RData')

level1_number <- level1_summary$web.name
level2_number <- level2_summary$web.name-397
level3_number <- level3_summary$web.name-679

#In-degree or number of prey of all species in the network
InDegree <- function(M){
  return(colSums(M));
}

#Out-degree or number of predators of all species in the network
OutDegree <- function(M){
  return(rowSums(M));
}

#level1
level1_indegree <- list()
for(i in level1_number){
  level1_indegree[[i]] <- InDegree(level1_binary_matrix[[i]])
}

level1_mean_indegree <- c()
for(i in level1_number){
  level1_mean_indegree <- c(level1_mean_indegree,mean(level1_indegree[[i]]))
}

level1_outdegree <- list()
for(i in level1_number){
  level1_outdegree[[i]] <- OutDegree(level1_binary_matrix[[i]])
}

level1_mean_outdegree <- c()
for(i in level1_number){
  level1_mean_outdegree <- c(level1_mean_outdegree,mean(level1_outdegree[[i]]))
}

#level2
level2_indegree <- list()
for(i in level2_number){
  level2_indegree[[i]] <- InDegree(level2_binary_matrix[[i]])
}

level2_mean_indegree <- c()
for(i in level2_number){
  level2_mean_indegree <- c(level2_mean_indegree,mean(level2_indegree[[i]]))
}

level2_outdegree <- list()
for(i in level2_number){
  level2_outdegree[[i]] <- OutDegree(level2_binary_matrix[[i]])
}

level2_mean_outdegree <- c()
for(i in level2_number){
  level2_mean_outdegree <- c(level2_mean_outdegree,mean(level2_outdegree[[i]]))
}

#level3
level3_indegree <- list()
for(i in level3_number){
  level3_indegree[[i]] <- InDegree(level3_binary_matrix[[i]])
}

level3_mean_indegree <- c()
for(i in level3_number){
  level3_mean_indegree <- c(level3_mean_indegree,mean(level3_indegree[[i]]))
}

level3_outdegree <- list()
for(i in level3_number){
  level3_outdegree[[i]] <- OutDegree(level3_binary_matrix[[i]])
}

level3_mean_outdegree <- c()
for(i in level3_number){
  level3_mean_outdegree <- c(level3_mean_outdegree,mean(level3_outdegree[[i]]))
}

#total mean in/out degree
EcoBuilder_mean_in_degree <- c(level1_mean_indegree,level2_mean_indegree,level3_mean_indegree)
EcoBuilder_mean_out_degree <- c(level1_mean_outdegree,level2_mean_outdegree,level3_mean_outdegree)

EcoBuilder_mean_inout_degree <- as.data.frame(EcoBuilder_mean_in_degree)
EcoBuilder_mean_inout_degree$EcoBuilder_mean_out_degree <- EcoBuilder_mean_out_degree
write.csv(EcoBuilder_mean_inout_degree,file = '30EcoBuilder_mean_inout_degree.csv')

#Degree
total_indegree <- c()
for(i in level1_number){
  total_indegree <- c(total_indegree,level1_indegree[[i]])
}
for(i in level2_number){
  total_indegree <- c(total_indegree,level2_indegree[[i]])
}
for(i in level3_number){
  total_indegree <- c(total_indegree,level3_indegree[[i]])
}

total_outdegree <- c()
for(i in level1_number){
  total_outdegree <- c(total_outdegree,level1_outdegree[[i]])
}
for(i in level2_number){
  total_outdegree <- c(total_outdegree,level2_outdegree[[i]])
}
for(i in level3_number){
  total_outdegree <- c(total_outdegree,level3_outdegree[[i]])
}

Degree <- total_indegree+total_outdegree
write.csv(Degree, file = '30EcoBuilder_total_Degree.csv')
save(level1_indegree,level1_outdegree,level2_indegree,level2_outdegree,level3_indegree,level3_outdegree,file = '30EcoBuilder_inout_degree_list.RData')
