
##########################################################
# Title: Testing plot functions
# Authors: Abel Camacho Guardian
# Date: 27
#  Version 1 (27.04.2019): Testing plot functions
#
##########################################################

#library(png)
library(grid)
library(ggplot2)
library(magick)
setwd('C:\\Users\\abel\\Desktop\\Projects\\plots')
# Functions
source('plot_functions.R')


test.data<-
  data.frame(groups=c('Group1','Group2','Group3','Group4'),total=c(30,30,20,20))



g<-matrix_perc_plot(data=test.data,
                    colors=c('#F76C6C','#F8E9A1',"#A8D0E6","#374785"),
                    pictures=c("images\\family.png",
                               "images\\man.png","images\\parents.png",
                               "images\\girl.png"), 
                    figure_size=.7,background_size=1,
                    manual_size=F, n=10,m=10)


g+theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background =element_rect(fill='white'),
        axis.text = element_text(size=rel(0) ))

ggsave('output\\test.png',height=10,width=10)



# ####################################################################
#
# ####################################################################




dates<-
  c(0:10)+as.Date('2018-01-01','%Y-%m-%d')
sales<-abs(rnorm(sd=9,n=3*length(dates)))
groups<-c(rep('g1',length(dates)),rep('g2',length(dates)),rep('g3',length(dates)))



test.data<-data.frame('dates'=c(dates,dates,dates),
                      'y'=sales, 
                      'groups'=groups)




polygon_trend(test.data,c('red','yellow','yellow'))+
  xlab('')+ylab('')





