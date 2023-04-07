setwd("D:/archive")

dane1 <- read.csv("titles.csv",encoding = "UTF-8",na.strings=c("","NA"))

library(dplyr)
library(stringr)
library(ggplot2)
library(treemap)
library(RColorBrewer)


dane1$horror<-ifelse(str_detect(dane1$genres, "horror")==1,1,0)
dane1$US <- ifelse(str_detect(dane1$production_countries, "['US']")==1,1,0)
dane1$countries <- substr(dane1$production_countries,3,4)
dane1$imdb <- ifelse(!round(dane1$imdb_score,digits=0)%in%c(2:9),0,round(dane1$imdb_score))
dane1$age_certification <- factor(dane1$age_certification,levels = c(NA,"NC-17","R","PG-13","PG","G"))

#1
ggplot(dane1,aes(x=imdb_score,fill=factor(horror)))+geom_histogram(binwidth = 1,bins=8,position = position_jitterdodge(),center=1)+
  labs(caption = "Plot 1",fill="Horror") + xlab("imdb scores") + theme_bw()

#2
ggplot(dane1,aes(x=imdb_score,fill=factor(US)))+geom_density(alpha=0.5)+
  labs(title="")+theme_bw()+labs(caption = "Plot 2",fill="US") + xlab("imdb scores")
#3
ggplot(dane1,aes(x=imdb_score,fill=factor(age_certification)))+geom_density(alpha=0.9,position = position_fill() )+
  labs(title="")+theme_bw()+labs(caption = "Plot 3",fill="Age certification") +theme(panel.grid = element_blank())

#4
x<-dane1 %>% group_by(countries)%>%summarise(n=n(),imdb_score=mean(imdb_score,na.rm=TRUE))%>%filter(n>30)
x$countries=c("NA","CA","GB","US")
x$countries <- factor(x$countries,levels = c("US","GB","CA","NA"))
ggplot(x,aes(x=countries,y=n,fill=countries))+geom_bar(stat = "identity")+scale_fill_manual(values = c("lightblue","grey","grey","grey"))+
  ylab("Count")+labs(caption = "Plot 4",fill = "Countries")+theme(axis.title.x = element_blank(),
                                                                  axis.text.y = element_blank(),
                                                                  axis.ticks = element_blank(),
                                                                  panel.background = element_blank(),
                                                                  legend.position = c(0.9,0.7))+
geom_text(aes(x=countries,y=n+15,label=n))




#5
x3 <- dane1 %>% filter(imdb>2)%>%group_by(imdb) %>% summarise(n=n(),votes=mean(imdb_votes,na.rm=TRUE))

ggplot(x3,aes(imdb,votes,fill=imdb))+geom_col(fill=brewer.pal(6,"Greys"))+labs(caption = "Plot 5")+xlab("imdb score")+theme(
  panel.grid.major.y = element_line(color = "grey"),
  panel.grid = element_blank(),
  panel.background = element_blank())+scale_x_continuous(breaks = seq(3,8,1))



#6
ggplot(dane1,aes(imdb_score,tmdb_score))+geom_jitter()+geom_smooth(size=1.3,se=FALSE)+scale_x_continuous(breaks = seq(0,10,2.5))+labs(caption = "Plot 6")+
  xlab("imdb score")+ylab("tmdb score") + geom_abline(color="red",size=1, lty=2) + theme_bw()

#7

z <- dane1 %>% group_by(horror,age_certification)%>%summarise(score=mean(imdb_score,na.rm=TRUE))
z$age_certification = factor(z$age_certification,levels = c(NA,"NC-17","R","PG-13","PG","G"), ordered = TRUE)
global_mean <-mean(z$score)

ggplot(z,aes(y=factor(age_certification),x=score,color=factor(horror)))+geom_point(size=5)+labs(caption = "Plot 7", color="Horror")+
  geom_segment(aes(xend = 4, yend = age_certification), size = 2)+scale_x_continuous(expand = c(0,0),limits = c(4,6),position="top")+
  theme(panel.grid.major.y = element_blank(),
        axis.title.y.right = element_text(),
        axis.ticks.y = element_blank(),
        axis.title = element_blank())+
  geom_vline(xintercept = global_mean, color = "grey40", linetype = 3)+
  annotate("text",x = 5.3, y = 2.4,label = "The\nglobal\naverage",
    vjust = 1, size = 5, color = "grey40")

#8
c <- dane1 %>%filter(release_year %in% c("2010":"2020")) %>%group_by(release_year,horror)%>%summarise(n=mean(imdb_score,na.rm=TRUE))

ggplot(c,aes(x=release_year,y=n,color=factor(horror)))+geom_line(size=1.3)+labs(caption = "Plot 8",color="Horror")+
  xlab("Year")+ylab("average imdb score")+scale_x_continuous(breaks = seq(2010,2020,2))+theme_bw()
