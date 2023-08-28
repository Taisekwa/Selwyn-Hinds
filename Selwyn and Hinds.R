df<-read.csv("C:/Users/chikazhet/selwynhinds.csv")

# All mitigations plotted
MyData<- df
#MyData$nlosschange <- is.numeric(MyData$nlosschange)
library(tidyverse)
library(ggplot2)
library("reshape2")
library("ggplot2")

MyData$mitigation <- as.character(MyData$mitigation)

ggplot()+        
  geom_point(data=MyData, mapping=aes(y = profitchange, x = nlosschange, colour=mitigation))
ls()
# filter GMP mitigations
temp0 <- MyData %>% 
  filter(mitigation %in% c("Base", "irrigation efficiency"))
ggplot()+
geom_point(data=temp0, mapping=aes(y = profitchange, x = nlosschange, colour=mitigation))

# filter autumn management mitigations
temp1 <- MyData %>%
  filter(mitigation %in% c("Base", "Cull early " ,"autumn nfert with grain"))

# filter less N fertiliser
temp2 <- MyData %>%
  filter(mitigation %in% c("Base", "less N same pasture " ,"less N less pasture+barley",  "less N less pasture less SR"))

# filter De-intensification
temp3 <- MyData %>%
  filter(mitigation %in% c("Base", "Less SR same production " ,"Less SR less utulisation2",  "less SR less utulisation5", "less SR, less production"))

# filter infrustructure
temp4 <- MyData %>%
  filter(mitigation %in% c("feed pad","New irrig","Base"))


foo <- subset(MyData, select = c(mitigation, nlosschange, profitchange))

foo$mitigation

keys <- data.frame("option"= c('Base','autumn management','De-intensification','De-intensification','De-intensification','Less N fertiliser','autumn management','Less N fertiliser','Less N fertiliser',
                               'infrustructure','De-intensification','GMP','infrustructure'),
                   "mitigation"=c("Base","Cull early","Less SR same production","Less SR less utulisation2",
                            "less SR less utulisation5","less N same pasture","autumn nfert with grain",
                            "less N less pasture+barley","less N less pasture less SR","feed pad",
                            "less SR, less production","irrigation efficiency","New irrig"))

foo <- merge(foo, keys, by=c('mitigation'), all=TRUE)
foo$comb <- paste(foo$option,"_", foo$mitigation)

ggplot(foo, aes(x=nlosschange, y=profitchange, group=mitigation, color=mitigation, shape=option))+
  geom_point()


ggplot(foo)+
  geom_point(aes(x=nlosschange, y=profitchange, group=comb, color=comb, size=comb))+
  scale_size_manual(labels = foo$comb,
                     values =c(foo$comb[1]=1,
                               foo$comb[1]=2,
                               foo$comb[1]=3,
                               foo$comb[1]=3,
                               foo$comb[1]=3,
                               foo$comb[1]=4,
                               foo$comb[1]=2,
                               foo$comb[1]=4,
                               foo$comb[1]=4,
                               foo$comb[1]=5,
                               foo$comb[1]=3,
                               'GMP'=6,
                               'infrustructure'=7))

# simple way of doing it without filtering 
temp <-  rbind(temp0 %>% mutate(Option = "GMP"), 
               temp1 %>% mutate(Option = "autumn management"),
               temp2 %>% mutate(Option = "Less N fertiliser"),
               temp3 %>% mutate(Option = "De-intensification"),
               temp4 %>% mutate(Option = "infrustructure"))
temp$Option<-  as.factor(temp$Option)

ggplot()+        
  geom_point(data=temp, mapping=aes(y = profitchange, x = nlosschange, colour=Option))
ls()

#Plotting the two lines in colour

ggplot(data = temp, mapping = aes(y = profitchange, x = nlosschange)) +
  facet_wrap(~ Option)

# plotting a black solid & a brocken line
  
ggplot(data = temp, mapping = aes(y = profitchange, x = nlosschange)) +
  geom_point(data = temp, mapping = aes(y = profitchange, x = nlosschange)) +
  theme(legend.position="bottom")+
  labs(y="% change in operating profit", x="% change in N leaching")+
facet_wrap(~ Option)



    