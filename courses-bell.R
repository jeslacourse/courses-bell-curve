x <- seq(-3, 4, by = .1)
y <- dnorm(x, mean = 0, sd = 1)
data.frame(x,y) -> bell 

term <- summaryM$term
  xe <- summaryM$statistic 
  ye <- dnorm(xe, mean =0, sd = 1)
data.frame(term, xe,ye) -> belle 
 
belle %>% 
  filter(!str_detect(term, "(X|Y|L)$")) %>% 
  ggplot(aes(x = xe, y=ye, group = 1)) + 
  geom_line(data = bell, aes(x = x, y = y, group = 1), alpha = 0.3)+ 
  geom_vline(aes(xintercept = -2), linetype ="dashed", alpha =0.2) +
  geom_vline(aes(xintercept = 0), linetype ="dashed", alpha =0.2) +
  geom_vline(aes(xintercept = 2), linetype ="dashed", alpha =0.2) +
  geom_point() + 
  ggrepel::geom_text_repel(aes(label = term, hjust = -0.1), angle = 0, size = 3, 
                           max.overlaps = 12, min.segment.length = 0.1) + 
  scale_y_continuous(limits = c(0, 0.45)) + 
  sierra.plot::theme_sierra_simple() + 
  labs(title = "Award-Earning by Major Course-Taking, Biology", 
       x = "Award Likelihood", 
       y = "")
