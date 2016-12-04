library("ggmap")
library(maptools)
library(maps)
library(dplyr)
library(stringr)
library(ggplot2)
library(gridExtra)
#visualization part:
city_information<-function(city_sel){
q1table6_2015<-read.csv('2015_q1_table6.csv',header=TRUE,stringsAsFactors=FALSE)

# Graph on the map all the airline that departs from city1
# load city1 data(just one city) into data frame
#start_city<-"New York City, NY (Metropolitan Area)"
start_city<-city_sel
start<-gsub("\\s*\\([^\\)]+\\)","",as.character(start_city))
ll.start<-geocode(start)
star<-data.frame(start,ll.start)

#load city2 data( all cities related to city1 into a data frame
#visited_city<-filter(q1table6_2015,q1table6_2015$city1=="New York City, NY (Metropolitan Area)")
#visited<-gsub("\\s*\\([^\\)]+\\)","",as.character(visited_city$city2))
visited<-c("Boston,MA")
ll.visited<-geocode(visited)
visited.fare.table<-filter(q1table6_2015,q1table6_2015$city1=="New York City, NY (Metropolitan Area)")
visited.fare<-visited.fare.table$fare
geo<-data.frame(visited,ll.visited,visited.fare)

new<-data.frame(star,geo)

#plot the state included us map first
# them add points showing the longtitude and lattitude information of city 1
# the add points of all city2
# connect city 1 and city 2s with lines
all_states <- map_data("state")
p<-ggplot()
p<-p + geom_polygon(data=all_states, aes(x=long, y=lat, group = group),colour="white", fill="grey10")
p_city<-p+geom_point( data=geo, aes(x=lon, y=lat), color="red",size=2)
p_withpoint<-p_city+geom_point( data=star, aes(x=lon, y=lat),color="blue",size=5) + geom_segment(data=new,aes(x=lon,y=lat,xend=lon.1,yend=lat.1),color=visited.fare, arrow=arrow(length=unit(0.3,"cm")),show.legend = TRUE)
p_withtext<-p_withpoint+geom_text(aes(x=lon, y=lat,label = visited),data=geo,color="pink",size=2.5)
return(p_withtext)
}

#statistical analysis part:
# For a given city in city1, like 'NYC', select all the cities that have flight from the
# given city, store the data in 'citydataframe'
statistical_departp1<-function(stat_city_sel){
  #cityname<-"New York City, NY"
  cityname<-stat_city_sel
  table6<-q1table6_2015
  citydata<-table6 %>% filter(str_detect(city1, cityname))
  citydataframe<-as.data.frame(citydata)
  
  # plot the graph of average fare versus destination cities (city2)
  titlenames_1<-paste('The average fares of airlines from ',cityname,' to other cities')
  p1<-ggplot(citydataframe, aes(city2, fare)) + 
    xlab('fare') + ylab('destination')  + ggtitle(titlenames_1) +
    geom_bar(stat='identity', fill="#FF9999", colour="black") +coord_flip()
  return(p1)
}

statistical_departp2<-function(stat_city_sel){
  #cityname<-"New York City, NY"
  cityname<-stat_city_sel
  table6<-q1table6_2015
  citydata<-table6 %>% filter(str_detect(city1, cityname))
  citydataframe<-as.data.frame(citydata)
  
  
  #  plot the graph of average passenger versus destination cities (city2)
  titlenames_2<-paste('The average passenger of airlines from ',cityname,' to other cities')
  p2<-ggplot(citydataframe, aes(city2, passengers)) + 
    xlab('passengers') + ylab('destination')  + ggtitle(titlenames_2) +
    geom_bar(stat='identity', fill="red", colour="black") +coord_flip()
  return(p2)
  
}

statistical_departp3p4<-function(stat_city_sel){
  #cityname<-"New York City, NY"
  cityname<-stat_city_sel
  table6<-q1table6_2015
  citydata<-table6 %>% filter(str_detect(city1, cityname))
  citydataframe<-as.data.frame(citydata)
  
  # draw the scatterplot of fare versus city distance for a given city1
  p3<-ggplot(citydataframe,aes(nsmiles,fare)) + geom_point() + geom_smooth() 
  #the correlation between fare and distance
  cor_fare_dis<-cor(citydataframe$nsmiles,citydataframe$fare)
  
  p4<-ggplot(citydataframe,aes(passengers,fare)) + geom_point() + geom_smooth()
  cor_fare_pasger<-cor(citydataframe$passengers,citydataframe$fare)
  grid.arrange(p3,p4,nrow=2)
  return(grid.arrange(p3,p4,nrow=2))
  #return(grid.arrange(p1,p2,p3,p4,nrow=2,ncol=2))
}

statistical_depart_infor<-function(stat_city_sel1,stat_city_sel2){
  #cityname<-"New York City, NY"
  cityname1<-stat_city_sel1
  cityname2<-stat_city_sel2
  table6_wholeyear<-read.csv('2015_table6.csv',header=TRUE,stringsAsFactors=FALSE)
  
  
  selec_data<-table6_wholeyear %>% filter(str_detect(city1, cityname1)) %>% 
    filter(str_detect(city2, cityname2)) 
  
  
  selectdataframe<-as.data.frame(selec_data)
  
  
  ggplot(selectdataframe,aes(quarter,fare)) + geom_bar(stat='identity',fill="pink", colour="yellow",width = 0.7) + 
    coord_cartesian(ylim=c(350,400))+geom_line(size=3,colour="grey") + ylab('average fare') +
    ggtitle('quarterly airline average fare change in year 2015') 
}

