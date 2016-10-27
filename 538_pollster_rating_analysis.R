library(tidyverse)
library(httr)
library(data.table)
library(forcats)

polls<-fread('http://projects.fivethirtyeight.com/general-model/president_general_polls_2016.csv')
polls$grade<-polls$grade %>% factor(levels=c('A+','A','A-','B+','B','B-','C+','C','C-','D',''))
polls$createddate<- polls$createddate %>%  as.Date(format='%m/%d/%g')

states<-read_csv('upshot_state_est.csv')

analysis<-polls %>% 
  mutate(clinton_advantage=rawpoll_clinton-rawpoll_trump) %>% 
  select(forecastdate, state, clinton_advantage, pollster, grade, createddate)

adjust<-function(state, advantage){
  if(state=='U.S.') advantage
  else advantage-states$D_Estimate[which(states$State==state)]
}

analysis$adjusted_clinton_advantage<-mapply(adjust, analysis$state, analysis$clinton_advantage)

grade_analysis<-filter(analysis, state=='U.S.', createddate > '2016-10-05') %>% 
  group_by(grade) %>% 
  summarise(avg_c_adv=mean(clinton_advantage), poll_count=n())
grade_analysis<-grade_analysis[order(grade_analysis$grade)]

gg_analysis<-filter(analysis, state=='U.S.', createddate < Sys.Date()) %>% select(grade, createddate, clinton_advantage)
gg_analysis$collapse<-sapply(gg_analysis$grade,function(i){
  if(i=='A+'|i=='A'|i=='A-') 'A'
  else if(i=='B+'|i=='B'|i=='B-') 'B'
  else if(i=='C+'|i=='C'|i=='C-') 'C'
  else i
}) %>% factor(levels=c('A','B','C'))
gg_analysis$APlus<-sapply(gg_analysis$grade,function(i){
  if(i=='A+') T
  else F
})

ggplot(gg_analysis, aes(x=createddate, y=clinton_advantage, group=grade, color=grade))+
  geom_point(size=0.1)+
  stat_smooth(fill='#DCDCDC', alpha=0.01)+
  ggtitle('Clinton Advantage by 538 Pollster Ranking')

ggplot(gg_analysis, aes(x=createddate, y=clinton_advantage, group=collapse, color=collapse))+
  geom_point(size=0.1)+
  stat_smooth(fill='#DCDCDC', alpha=0.01)+
  ggtitle('Clinton Advantage by 538 Pollster Ranking (Collapsed A, B, C)')

ggplot(gg_analysis, aes(x=createddate, y=clinton_advantage, group=APlus, color=APlus))+
  geom_point(size=0.1)+
  stat_smooth(fill='#DCDCDC', alpha=0.01)+
  ggtitle('Clinton Advantage by 538 Pollster Ranking (A+ versus all Others)')
