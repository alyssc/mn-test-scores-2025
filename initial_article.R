setwd("/Users/alyssachen/Desktop/Projects/mn-reformer/test-scores-2025")

library(tidyverse)

######## INITIAL ARTICLE ######## 
# https://minnesotareformer.com/2025/08/29/minnesota-students-academic-performance-stagnates-after-pandemic-declines/ 

test_scores <- read.csv("data/northstar/over_time_scores.csv")

math <- test_scores %>% 
  select("Year","Group","Math") %>%
  pivot_wider(names_from = Group, values_from = Math)

reading <- test_scores %>% 
  select("Year","Group","Reading") %>%
  pivot_wider(names_from = Group, values_from = Reading)

write.csv(math, "forviz/math.csv",row.names=FALSE)
write.csv(reading, "forviz/reading.csv", row.names=FALSE)

schools <- read.csv("data/northstar/schools.csv")
head(schools)

schools <- schools %>%
  filter(Year==2025) %>%
  filter(Group %in% c("ALL","FRP")) %>%
  select(District.Name, School.Name, Group, Stage.1..Math.Ach, Stage.1..Reading.Ach) %>%
  rename(math = "Stage.1..Math.Ach", reading = "Stage.1..Reading.Ach") 

schools <- schools %>%
  pivot_wider(names_from = Group, values_from = c(math,reading))

write.csv(schools, "forviz/schools_processed.csv", row.names=F)