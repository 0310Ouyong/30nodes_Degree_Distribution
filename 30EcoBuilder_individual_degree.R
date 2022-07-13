rm(list=ls())
getwd()
setwd('C:/data')

library(dplyr)
library(ggplot2)
library(ggpubr)
library(ggpmisc)
library(gridExtra)
library(cowplot)

load('EcoBuilder_inout_degree_list.RData')
load('EcoBuildersummary30nodes.RData')

level1_number <- level1_summary$web.name
level2_number <- level2_summary$web.name-397
level3_number <- level3_summary$web.name-679

level1_dataframe <- as.data.frame(level1_number)
EcoBuilder <- list()
for(i in level1_number){
  EcoBuilder[[which(level1_dataframe$level1_number==i)]] <- level1_indegree[[i]]+level1_outdegree[[i]]
}

level2_dataframe <- as.data.frame(level2_number)
for(i in level2_number){
  EcoBuilder[[which(level2_dataframe$level2_number==i)+3]] <- level2_indegree[[i]]+level2_outdegree[[i]]
}

level3_dataframe <- as.data.frame(level3_number)
for(i in level3_number){
  EcoBuilder[[which(level3_dataframe$level3_number==i)+103]] <- level3_indegree[[i]]+level3_outdegree[[i]]
}

EcoBuilder_Degree <- as.data.frame(table(EcoBuilder[[1]]))
EcoBuilder_Degree[,1] <- as.numeric(levels(EcoBuilder_Degree$Var1)[EcoBuilder_Degree$Var1])
EcoBuilder_Degree[,1] <- log10(EcoBuilder_Degree$Var1)
EcoBuilder_Degree[,2] <- log10(EcoBuilder_Degree$Freq)
length <- rep(1,dim(EcoBuilder_Degree)[[1]])
EcoBuilder_Degree$group <- length
names(EcoBuilder_Degree) <- c('x','y','group')

for(i in 2:112){
  data <- as.data.frame(table(EcoBuilder[[i]]))
  data[,1] <- as.numeric(levels(data$Var1)[data$Var1])
  data[,1] <- log10(data$Var1)
  data[,2] <- log10(data$Freq)
  length <- rep(i,dim(data)[[1]])
  data$group <- length
  names(data) <- c('x','y','group')
  EcoBuilder_Degree <- rbind(EcoBuilder_Degree,data)
}

#plot
for(i in 1:112){
  my.formula <- y~x
  EcoBuilder_Degree2 = subset(EcoBuilder_Degree,group==i)
  assign(paste0('p',i),
         ggplot(data=EcoBuilder_Degree2,
                aes(x=x,y=y))+
           geom_smooth(method = 'lm',se=FALSE,
                       color='black',formula = my.formula)+
           stat_poly_eq(formula = my.formula,
                        aes(label=paste(..eq.label..,
                                        ..rr.label..,
                                        sep='~~~')),
                        parse=TRUE,label.x = 0.5,size=2)+
           geom_point(size=4,
                      alpha=0.7,
                      color='#fe654c')+
           coord_cartesian(ylim=c(0,1.5),
                           xlim=c(0,1.5))+
           theme_bw()+
           geom_smooth(method='lm',
                       color='#558ebd',
                       fill='lightgray',
                       alpha=0.7,
                       size=0.8,se=T,
                       formula = y~x)+
           theme(panel.grid = element_blank())+
           labs(x='log10 Degree',y='log10 frequency')+
           theme(axis.title.x = element_text(size=9))+
           theme(axis.title.y = element_text(size=9)))
}


pdf('30EcoBuilder_individual_degree1.pdf',width=8,height = 10)
plot_grid(p1, p2, p3, p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15,p16,p17,p18,p19,p20,ncol = 4, nrow = 5)
dev.off()

pdf('30EcoBuilder_individual_degree2.pdf',width=8,height = 10)
plot_grid(p21,p22,p23,p24,p25,p26,p27,p28,p29,p30,p31,p32,p33,p34,p35,p36,p37,p38,p39,p40,ncol = 4, nrow = 5)
dev.off()

pdf('30EcoBuilder_individual_degree3.pdf',width=8,height = 10)
plot_grid(p41,p42,p43,p44,p45,p46,p47,p48,p49,p50,p51,p52,p53,p54,p55,p56,p57,p58,p59,p60,ncol = 4, nrow = 5)
dev.off()

pdf('30EcoBuilder_individual_degree4.pdf',width=8,height = 10)
plot_grid(p61,p62,p63,p64,p65,p66,p67,p68,p69,p70,p71,p72,p73,p74,p75,p76,p77,p78,p79,p80,ncol = 4, nrow = 5)
dev.off()

pdf('30EcoBuilder_individual_degree5.pdf',width=8,height = 10)
plot_grid(p81,p82,p83,p84,p85,p86,p87,p88,p89,p90,p91,p92,p93,p94,p95,p96,p97,p98,p99,p100,ncol = 4, nrow = 5)
dev.off()


pdf('30EcoBuilder_individual_degree6.pdf',width=8,height = 6)
plot_grid(p101, p102, p103, p104,p105,p106,p107,p108,p109,p110,p111,p112,ncol = 4, nrow = 3)
dev.off()
