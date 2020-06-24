library(ggplot2)
library(RColorBrewer)
library(dplyr)
library(tidyr)
library(ggthemes)

tab <- readr::read_csv('data/ab3.csv')

levs <- c('Much worse', 'Somewhat worse', 'Almost the same', 'Somewhat better',  'Much better')

dat <- 
  tab %>%
  mutate(neg_score = 1/2*`Almost the same` + `Somewhat worse` + `Much worse`) %>%
  pivot_longer(`Much worse`:`Much better`, 
               names_to = 'level', values_to='pct') %>%
  mutate(level = factor(level, levs),
         lev_num = as.numeric(level))


highs <- 
  dat %>%
  filter(lev_num>=3) %>%
  mutate(pct = if_else(lev_num == 3, pct/2, pct))

lows <-
  dat %>%
  filter(lev_num <= 3) %>%
  mutate(pct = if_else(lev_num ==3, -pct/2, -pct))


mytitle<-"\"What do you think will be the economic situation in your country during the next\nfew years (3-5 years) compared to the current situation?\"\n"
pal <-brewer.pal(length(levs),"RdBu")
# Change middle value 
pal[ceiling(length(levs)/2)]<-"#DFDFDF"


ggplot()+
  geom_bar(data=highs, aes(reorder(Country, neg_score), pct, fill=level),
           position = position_stack(reverse = TRUE), stat='identity')+
  geom_bar(data=lows, aes(reorder(Country, neg_score), pct, fill=level),
           position = 'stack', stat='identity')+
  geom_hline(yintercept = 0, color =c("white")) +
  scale_fill_manual(values = pal, breaks = levs)+
  scale_y_continuous(breaks=seq(-1,1, 0.25), labels = scales::percent)+ 
  coord_flip()+
  theme_fivethirtyeight()+
  labs(title = mytitle, x='',y='', fill='')+
  theme(plot.title = element_text(size=14, hjust=0.5))+
  theme(axis.text.y = element_text(hjust=0))+
  theme(legend.position = "bottom")
