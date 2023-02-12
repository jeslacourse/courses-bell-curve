
#
# Prep Bell Curve
#

# https://stackoverflow.com/questions/48877475/
make_stars <- function(pval) {
  stars = ""
  if(pval <= 0.001)
    stars = "***"
  if(pval > 0.001 & pval <= 0.01)
    stars = "**"
  if(pval > 0.01 & pval <= 0.05)
    stars = "*"
  if(pval > 0.05 & pval <= 0.1)
     stars = "."
  stars
}

# Use the Barrier data to look at non-major coursework
# Course counts (we'll want more than `n` students in a course
nonMajor %>% 
  group_by(Course) %>% 
  summarize(counts = n_distinct(ID)) %>% arrange(Course) ->counts

# Logistic model 
m <- glm(hasAwardOrTrans ~ Course, family = binomial, data = nonMajor)

m %>% broom::tidy() %>% as.data.frame %>% 
  mutate(term = stringr::str_remove(term, "Course"), 
         term = ifelse(term == "(Intercept)", 
                       as.character(nonMajor$Course[order(nonMajor$Course)[1]]), term),
         signif = sapply(p.value, function(x) make_stars(x)))-> summaryM

# Make pretty summary table                         
summaryM %>% 
  arrange(-estimate) %>% 
  mutate(p.value = scales::comma(p.value), 
         estimate = round(estimate, 3), 
         std.error= round(std.error, 3), 
         statistic = round(statistic, 3)
         ) %>% 
  kableExtra::kable(booktabs = T, 
        col.names = c("Course", "Estimate", "Std Error", "Z-score", "P-value", "Significance"), 
        caption = "Non-Major Statistics"
        ) %>% 
  kableExtra::footnote(general = " Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1" ,footnote_as_chunk = T)



#
# Start Bell Plot
#
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
  # TODO: scale_x_cts : [Very] Un|Likely on [-4,4]
  #sierra.plot::theme_sierra_simple() + 
  theme_bw() + 
  labs(title = "Award-Earning by GE, Major Prep Course-Taking, Biology", 
       x = "Award Likelihood", 
       y = "")
