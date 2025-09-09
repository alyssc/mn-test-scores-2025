setwd("/Users/alyssachen/Desktop/Projects/mn-reformer/test-scores-2025")

library(tidyverse)


######## ######## ######## ######## 
######## PANDEMIC RECOVERY ######## 
######## ######## ######## ######## 
# s_scores = scores by school; d_scores = scores by district

s_scores <- read.csv("intermediary/school_scores_2017-2025.csv")
d_scores <- read.csv("intermediary/district_scores_2017-2025.csv")


# Analysis by school ---------------------------------------------------------------

s_scores <- s_scores %>%
  filter(Year %in% c(2019,2022,2025),
         Group == "ALL") %>% 
  select(Year,District.Number,District.Type,
         District.Name,School.Number,School.Type,School.Name,
         Stage.1..Math.Ach, Stage.1..Reading.Ach, Stage.3..Cons.Att) %>%
  rename(math = "Stage.1..Math.Ach", reading = "Stage.1..Reading.Ach", 
         attendance = "Stage.3..Cons.Att") 

a <- s_scores %>% filter(Year==(2019))
b <- s_scores %>% filter(Year==(2025))

scores_2019_to_2025 <- inner_join(a,b,by = c("District.Number","District.Type","School.Number"))
dim(inner_join(a,b,by = c("District.Number","District.Type","School.Number")))
dim(full_join(a,b,by = c("District.Number","District.Type","School.Number")))

summary(scores_2019_to_2025$School.Name.x==scores_2019_to_2022$School.Name.y)

# Viewing schools whose names changed from 2019 to 2025
name_change <- scores_2019_to_2025 %>% filter (School.Name.x!=School.Name.y) %>%
  select(School.Name.x, School.Name.y) 
write.csv(name_change, "intermediary/school_name_changed_2019_2025.csv",row.names=F)

colnames(scores_2019_to_2025)

percent_changes_schools_2019_2025 <- scores_2019_to_2025 %>%
  filter(math.y!=0, math.x!=0) %>%
  mutate(math_percent_change = (math.y-math.x)/math.x,
         math_percent_point_change = math.y-math.x,
         reading_percent_change = (reading.y-reading.x)/reading.x,
         reading_percent_point_change = reading.y-reading.x)

percent_changes_schools_2019_2025 <- percent_changes_schools_2019_2025 %>% 
  mutate(quintile_math_x = ntile(math.x, 5),
         quintile_math_y = ntile(math.y, 5),
         quintile_read_x = ntile(reading.x, 5),
         quintile_read_y = ntile(reading.y, 5),)

write.csv(percent_changes_schools_2019_2025,"intermediary/percent_changes_schools_2019_2025.csv", row.names=F)


# Analysis by district ---------------------------------------------------------------

percent_changes_districts_19_22_25 <- read.csv("intermediary/percent_changes_districts_19_22_25.csv")


head(d_enroll19)


colnames(percent_changes_districts_19_22_25)
write.csv(percent_changes_districts_19_22_25,"intermediary/percent_changes_districts_19_22_25.csv", row.names=F)








# For later: schools + demographics

s_enroll_2025 <- s_enroll_2025 %>%
  select(Data.Year, District.Number, District.Type, District.Name, School.Number, School.Name,
         Total.Enrollment, Total.White.Percent, Total.Students.Eligible.for.Free.or.Reduced.Priced.Meals.Percent)
s_enroll_2019 <- s_enroll_2019 %>%
  select(Data.Year, District.Number, District.Type, District.Name, School.Number, School.Name,
         Total.Enrollment, Total.White.Percent, Total.Students.Eligible.for.Free.or.Reduced.Priced.Meals.Percent)


percent_changes_schools_2019_2025 <- left_join(percent_changes_schools_2019_2025,
                                                 s_enroll_2019,
                                                 suffix = c("","2019enroll"),by = c("District.Number","District.Type"))

head(percent_changes_districts_2019_2025)
write.csv(percent_changes_districts_2019_2025,"percent_changes_districts_2019_2025.csv", row.names=F)







