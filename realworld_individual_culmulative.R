rm(list=ls())
getwd()
setwd('C:/data/Hsi')

library(ggplot2)
library(ggpubr)
library(ggpmisc)
library(gridExtra)
library(cowplot)
library(dplyr)

load('realworld_list_Degree.RData')

#arrange data
realworld <- as.data.frame(table(realworld_Degree[[1]]))
realworld[,1] <- as.numeric(levels(realworld$Var1)[realworld$Var1])
length <- rep(1,dim(realworld)[[1]])
realworld$group <- length
names(realworld) <- c('x','y','group')

for(i in 2:74){
  data <- as.data.frame(table(realworld_Degree[[i]]))
  data[,1] <- as.numeric(levels(data$Var1)[data$Var1])
  length <- rep(i,dim(data)[[1]])
  data$group <- length
  names(data) <- c('x','y','group')
  realworld <- rbind(realworld,data)
}

total_proportion <- c()
for(j in 1:74){
  data <- filter(realworld,group==j)
  proportion <- c()
  for(i in 1:1:dim(data)[1]-1){
    proportion[i+1] <- (sum(data$y)-sum(data$y[1:i]))/sum(data$y)
  }
  proportion[1] <- 1
  total_proportion <- c(total_proportion,proportion)
}


realworld$y <- total_proportion
realworld[,1] <- log10(realworld$x)

for(i in 1:74){
  my.formula <- y~x
  realworld2 = subset(realworld,group==i)
  assign(paste0('p',i),
         ggplot(data=realworld2,
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
           coord_cartesian(ylim=c(0,1.1),
                           xlim=c(0,1.8))+
           theme_bw()+
           geom_smooth(method='lm',
                       color='#558ebd',
                       fill='lightgray',
                       alpha=0.7,
                       size=0.8,se=T,
                       formula = y~x)+
           theme(panel.grid = element_blank())+
           labs(x='log10 Degree',y='culmulative distribution')+
           theme(axis.title.x = element_text(size=9))+
           theme(axis.title.y = element_text(size=9))+
           geom_hline(aes(yintercept=1),colour='#990000',linetype='dashed'))
}
pdf('realworld_individual_culmulative1.pdf',width=8,height = 10)
plot_grid(p1, p2, p3, p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15,p16,p17,p18,p19,p20,ncol = 4, nrow = 5)
dev.off()

pdf('realworld_individual_culmulative2.pdf',width=8,height = 10)
plot_grid(p21,p22,p23,p24,p25,p26,p27,p28,p29,p30,p31,p32,p33,p34,p35,p36,p37,p38,p39,p40,ncol = 4, nrow = 5)
dev.off()

pdf('realworld_individual_culmulative3.pdf',width=8,height = 10)
plot_grid(p41,p42,p43,p44,p45,p46,p47,p48,p49,p50,p51,p52,p53,p54,p55,p56,p57,p58,p59,p60,ncol = 4, nrow = 5)
dev.off()

pdf('realworld_individual_culmulative4.pdf',width=8,height = 10)
plot_grid(p61,p62,p63,p64,p65,p66,p67,p68,p69,p70,p71,p72,p73,p74,ncol = 4, nrow = 5)
dev.off()








