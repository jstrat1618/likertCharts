tutor_df <- data.frame(worker = c('Justin', 'Abby', 'Jane', 'John', 'Amy',  'George'),
                 starts = c(0800, 0900, 1100, 1100, 1200, 1300),
                 ends = c(1000, 1300, 1500, 1300, 1600, 1600),
                  expertise = c('Statistics', 'Cal II', 'Cal III', 'Cal III',
                                'Cal I', 'Pre-Cal'))


ggplot(tutor_df, aes(x = starts, xend=ends, y=worker, yend=worker))+
  geom_segment(size=8)


ggplot(tutor_df, aes(x = starts, xend=ends, y=worker, yend=worker, col=expertise))+
  geom_segment(size=8)
