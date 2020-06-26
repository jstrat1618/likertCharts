library(tidyverse)
library(scales)
library(ggthemes)
library(readxl)

dat <- read_excel('data/P18-Hotel-Survey-Data.xlsx', sheet = "Responses")

qmet_dat <- read_excel('data/P18-Hotel-Survey-Data.xlsx', sheet='Question Metadata')

ans_choices <- c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")

tdat <- 
  dat %>%
  pivot_longer(Q1:Q10, names_to='qid', values_to = 'score') %>%
  mutate(answer = case_when(score==1~ans_choices[1], score==2~ans_choices[2],
                score==3~ans_choices[3], score==4~ans_choices[4],
                score==5~ans_choices[5]),
         neg_score= case_when(score < 3~1, score==3~0.5, score>3~0))

# Get cumulative totals for total_responses and tot_neg_responses
qdat <-
  tdat %>%
  group_by(qid) %>%
  summarise(tot_responses = n(),
            tot_neg_scores = sum(neg_score),
            .groups="drop") %>%
  full_join(qmet_dat %>%
              rename(qid = QuestionID), by = 'qid') %>%
  mutate(qnum = as.integer(substring(qid, 2)))%>%
  arrange(qnum)


fdat <- 
  tdat %>%
  group_by(qid, answer) %>%
  summarise(
    score = min(score),
    num =n(), 
    .groups="drop") %>%
  full_join(qdat, by="qid") %>% 
  mutate(prop = num/tot_responses,
         answer = factor(answer, ans_choices)) %>%
  arrange(answer, qnum)

positives <- 
  fdat %>%
    filter(score >= 3) %>%
  mutate(prop = if_else(score == 3, prop/2, prop))

negatives <- 
  fdat %>%
  filter(score <= 3) %>%
  mutate(prop = if_else(score == 3, -prop/2, -prop))


#my_cols <- c('#544914', '#A79129', '#A6A6A6', '#558ED5', '#17375F')
#my_cols <- c('#544914', '#A79129', '#A6A6A6', '#B5CCA8', '#8EB379')
my_cols <- c('red', 'pink', 'gray', 'cyan', 'blue')

ggplot() + 
  geom_bar(data=positives, 
           aes(x = reorder(Question, tot_neg_scores), y=prop, fill=answer), 
           position=position_stack(reverse = TRUE), stat="identity") +
  geom_bar(data=negatives,
           aes(x =reorder(Question, tot_neg_scores), y=prop, fill=answer) , 
           position="stack", stat="identity") +
  scale_fill_manual(values = my_cols, breaks = ans_choices) +
  geom_hline(yintercept = 0, color =c("white"), lty=2)+
  scale_y_continuous(limits = c(-1,1), labels = scales::percent) +
  coord_flip()+
  theme_tufte()+
  labs(x ='', y='Percent', fill='', title = 'Hotel Survey') +
  theme(plot.title = element_text(hjust = 0.5, size = 16, family = 'Arial', face='bold'),
        legend.position = 'bottom',
        legend.key.size = unit(0.5, 'line'),
        legend.text = element_text(size = 9, family = 'Arial'),
        axis.text.y = element_text(size = 11, family = 'Arial')) 
