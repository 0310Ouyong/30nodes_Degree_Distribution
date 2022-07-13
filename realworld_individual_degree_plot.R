rm(list=ls())
getwd()
setwd('C:/data/Hsi')

library(ggplot2)
library(ggpubr)
library(ggpmisc)
library(gridExtra)
library(cowplot)

load('realworld_list_Degree.RData')

#arrange data
realworld <- as.data.frame(table(realworld_Degree[[1]]))
realworld[,1] <- as.numeric(levels(realworld$Var1)[realworld$Var1])
realworld[,1] <- log10(realworld$Var1)
realworld[,2] <- log10(realworld$Freq)
length <- rep(1,dim(realworld)[[1]])
realworld$group <- length
names(realworld) <- c('x','y','group')

for(i in 2:74){
  data <- as.data.frame(table(realworld_Degree[[i]]))
  data[,1] <- as.numeric(levels(data$Var1)[data$Var1])
  data[,1] <- log10(data$Var1)
  data[,2] <- log10(data$Freq)
  length <- rep(i,dim(data)[[1]])
  data$group <- length
  names(data) <- c('x','y','group')
  realworld <- rbind(realworld,data)
}

#plot
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

pdf('realworld_individual_degree1.pdf',width=8,height = 10)
plot_grid(p1, p2, p3, p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15,p16,p17,p18,p19,p20,ncol = 4, nrow = 5)
dev.off()

pdf('realworld_individual_degree2.pdf',width=8,height = 10)
plot_grid(p21,p22,p23,p24,p25,p26,p27,p28,p29,p30,p31,p32,p33,p34,p35,p36,p37,p38,p39,p40,ncol = 4, nrow = 5)
dev.off()

pdf('realworld_individual_degree3.pdf',width=8,height = 10)
plot_grid(p41,p42,p43,p44,p45,p46,p47,p48,p49,p50,p51,p52,p53,p54,p55,p56,p57,p58,p59,p60,ncol = 4, nrow = 5)
dev.off()

pdf('realworld_individual_degree4.pdf',width=8,height = 10)
plot_grid(p61,p62,p63,p64,p65,p66,p67,p68,p69,p70,p71,p72,p73,p74,ncol = 4, nrow = 5)
dev.off()



