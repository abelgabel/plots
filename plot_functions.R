
##########################################################
# Title: Plot functions
# Authors: Abel Camacho Guardian
# Date: 27
#  Version 1 (27.04.2019): Create the plot function matrix_perc_plot
#
##########################################################


# Plot a matrix where in each cell a customer segment is depicted with a particular figure & picture
matrix_perc_plot<-function(data, colors,pictures='',
                           background_size=1, figure_size=.7, manual_size=T,
                           n=NA,m=NA){
  
  # The sum of total is equal 100
  data$total<- round(100*data$total/sum(data$total))
  
  if(!file.exists(pictures[1]))
  {
    pictures<-"images\\man-user.png"
  }
  
  figures<-list()
  for(i in 1:length(pictures)){
  figures[[i]] <- image_read(pictures[i]) 
  figures[[i]] <- rasterGrob(figures[[i]], interpolate=TRUE)
  }
  
names(data)<-c('groups','total')


g<-rep(0,dim(data)[1])
gd<-rep(0,dim(data)[1])

for(i in 1:length(g))
g[i]<-data$total[i]



if(!is.na(n) & !is.na(m)){
  for(i in 1:length(g))
  gd[i]<-round((n*m)*g[i]/100)
}

if(is.na(n) |  is.na(m)){
if(manual_size){
d<-gcd(g[1],g[2])



} else
{
  d<-1
  }
for(i in 1:length(g))
gd[i]<-g[i]
zd<-sum(gd)
m<-lpf(zd)
n<-zd/m
}


points<-matrix(0,nrow=n*m,ncol=2)
points<-data.frame(points)
names(points)<-c('x','y')

for(i in 1:dim(points)[1]){
  # i=m*p+k
  # i--> (k,p) 
  coord.x<-(i%%m)
  coord.y<-(i-coord.x)/m
  
  if(coord.x==0){
    coord.x<-m
    coord.y<-coord.y-1
  }
  
  points[i,'x']<-coord.x
  points[i,'y']<-coord.y

}



init.val<-1
end.val<-gd[1]
points[c(init.val:end.val),'id']<-"1"
points[c(init.val:end.val),'group']<-as.character(data[1,"groups"])
for(i in 2:length(g)){
init.val<-end.val+1
end.val<-end.val+gd[i]
points[c(init.val:end.val),'id']<-as.character(i)
points[c(init.val:end.val),'group']<-as.character(data[i,"groups"])

}


chosen.colors<-colors
names(chosen.colors)<-as.character(data[,'groups'])

g_plot<-ggplot(data=points)+xlim(1,m+1)+ylim(0,n)+
  geom_rect(aes(xmin=x,ymin=y,xmax=x+background_size,
                ymax=y+background_size, fill=group))+
  scale_fill_manual(values=chosen.colors, guide=F)
  
 
for(i in 1:dim(points)[1]){
k<-as.integer(points[i,'id'] )
    g_plot<-g_plot+annotation_custom(figures[[k]],
                     xmin=points$x[i],ymin=points$y[i],
                         xmax=points$x[i]+figure_size,ymax=points$y[i]+figure_size)
}

return(g_plot)
}







# #############################################################
# Functions needed to calculate optimal plot structure
# #############################################################

gcd <- function(a, b) {
  while(b) {
    temp = b
    b = a %% b
    a = temp
      }
  return(a)
}



lpf<-function(z){
  possible.divisors<-c(10,9,7,5,3,2)
  output=10
  for( i in possible.divisors)
  {
    if(z%%i==0){
      output=i
    break}
  }
  return(output)
}




# ##################################################################
#
# 
#
# ##################################################################



polygon_trend<-function(data, colors){
  
  names(data)<-c('dates','y','groups')
  
  all.groups<-as.character(unique(data[,'groups']))
  
  polygon_data<-list()
  
  
  
  polygon_data[[1]]<-
    data[data$groups==all.groups[1],]
  
  first.row<-data.frame('dates'=min(data[data$groups==all.groups[1],'dates']),
                        y=0)
  last.row<-data.frame('dates'=max(data[data$groups==all.groups[1],'dates']),
                       y=0)
  
  polygon_data[[1]]<-rbind(first.row,polygon_data[[1]][,c('dates','y')])
  polygon_data[[1]]<-rbind(polygon_data[[1]][,c('dates','y')],last.row)
  polygon_data[[1]]<-cbind(polygon_data[[1]],all.groups[1])
  
  names(polygon_data[[1]])[3]<-'groups'
  
  
  polygon_output<-polygon_data[[1]]
  
  
  
  for(i in 2:length(all.groups)){
    polygon_data[[i]]<-data%>%
      group_by(dates)%>%
      filter(groups %in% all.groups[1:i-1])%>%
      summarise(y=sum(y))%>%
      ungroup()
    
    
    temp_data<-data%>%
      group_by(dates)%>%
      filter(groups %in% all.groups[1:i])%>%
      summarise(y=sum(y))%>%
      ungroup()%>%
      arrange(desc(dates))
    
    
    polygon_data[[i]]<-rbind(polygon_data[[i]],temp_data)
    
    polygon_data[[i]]<-cbind(polygon_data[[i]],all.groups[i])
    names(polygon_data[[i]])[3]<-'groups'
    
    
    polygon_output<-rbind(polygon_output,polygon_data[[i]])
    
  }
  
  
  
  
  # #############################
  # Create plot function
  # #############################
  g_plot<-ggplot()
  
  for(i in 1:length(all.groups)){
    g_plot<-g_plot+
      geom_polygon(data=polygon_data[[i]],aes(x=dates,y=y,fill=groups))
  }  
  
  
  
  chosen.colors<-colors
  names(chosen.colors)<-all.groups
  
  
  g_plot+scale_fill_manual(values=chosen.colors, guide=F)  
  
  
  return(g_plot)
  
}






