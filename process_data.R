setwd("/Users/alyssachen/Desktop/Projects/mn-reformer/test-scores-2025")

library(tidyverse)

####### Processing North Star Accountability Data Files

# Making one CSV for school scores
s_scores2019 <- read.csv("data/northstar/scores_schools_2019.csv", skip=4)
head(s_scores2019)

s_scores <- read.csv("data/northstar/scores_schools_2022-2025.csv", skip=4)
s_scores <- s_scores %>% 
  select(-c(X,X.1))  # Idk what these columns are doing here 

s_scores2019 <- s_scores2019 %>%
  rename(Year = "Accountability.Year")

colnames(s_scores2019)==colnames(s_scores) # make sure column names are the same

s_scores <- rbind(s_scores, s_scores2019) 
write.csv(s_scores,"intermediary/northstar/school_scores_2017-2025.csv",row.names=F)


# one CSV for districts

d_scores2019 <- read.csv("data/northstar/scores_districts_2019.csv", skip=4)
head(d_scores2019)

d_scores <- read.csv("data/northstar/scores_districts_2022-2025.csv", skip=4)
head(d_scores)

d_scores <- d_scores %>% 
  select(-c(X,X.1))  # Idk what these columns are doing here 

d_scores2019 <- d_scores2019 %>%
  rename(Year = "Accountability.Year")

colnames(d_scores2019)==colnames(d_scores) # make sure column names are the same

d_scores <- rbind(d_scores, d_scores2019) %>% 
  select(Year,District.Number,District.Type,
         District.Name,
         Stage.1..Math.Ach, Stage.1..Reading.Ach, Stage.3..Cons.Att) %>%
  rename(math = "Stage.1..Math.Ach", reading = "Stage.1..Reading.Ach", 
         attendance = "Stage.3..Cons.Att") 


write.csv(d_scores,"intermediary/northstar/district_scores_2017-2025.csv",row.names=F)

d_scores <- d_scores %>%
  filter(Year %in% c(2019,2022,2025),
         Group == "ALL")

a <- d_scores %>% filter(Year==(2019)) %>%
  rename(math2019 = math, reading2019 = reading, 
         attendance2019 = attendance, name2019 = District.Name) 
b <- d_scores %>% filter(Year==(2022)) %>%
  rename(math2022 = math, reading2022 = reading, 
         attendance2022 = attendance, name2022 = District.Name) 
c <- d_scores %>% filter(Year==(2025)) %>%
  rename(math2025 = math, reading2025 = reading, 
         attendance2025 = attendance, name2025 = District.Name) 

dscores_19_22_25 <- inner_join(a, b, by=c("District.Number","District.Type")) %>%
  inner_join(., c, by=c("District.Number","District.Type")) %>%
  select(-c(Year.x, Year.y, Year) ) %>%
  select(District.Number, District.Type, name2019, name2022, name2025, math2019, reading2019,attendance2019, 
         math2022, reading2022,attendance2022,
         math2025, reading2025,attendance2025)

head(dscores_19_22_25)

summary(dscores_2019_to_2025$District.Name.x==dscores_2019_to_2025$District.Name.y)
dname_change <- dscores_2019_to_2025 %>% filter (District.Name.x!=District.Name.y) %>%
  select(District.Name.x, District.Name.y) 
write.csv(dname_change, "intermediary/district_name_changed_2019_2025.csv",row.names=F)

percent_changes_districts_19_22_25 <- dscores_19_22_25 %>%
  filter(math2019 != 0, math2022 != 0,math2025 != 0,reading2019 != 0,reading2022 != 0,reading2025 != 0,) %>%
  mutate(math_perc_19_22 = (math2022-math2019)/math2019,
         math_perc_pt_19_22 = math2022-math2019,
         math_perc_22_25 = (math2025-math2022)/math2022,
         math_perc_pt_22_25 = math2025-math2022,
         math_perc_19_25 = (math2025-math2019)/math2019,
         math_perc_pt_19_25 = math2025-math2019,
         reading_perc_19_22 = (reading2022-reading2019)/reading2019,
         reading_perc_pt_19_22 = reading2022-reading2019,
         reading_perc_22_25 = (reading2025-reading2022)/reading2022,
         reading_perc_pt_22_25 = reading2025-reading2022,
         reading_perc_19_25 = (reading2025-reading2019)/reading2019,
         reading_perc_pt_19_25 = reading2025-reading2019)

head(percent_changes_districts_19_22_25)

write.csv(percent_changes_districts_19_22_25,"intermediary/percent_changes_districts_19_22_25.csv", row.names=F)



####### Processing MCA/MTAS files
dat <- read.csv("data/mca/districts/math19.csv")
head(dat$Grade,15)
colnames(dat)

process_mca <- function(dat){
  dat <- dat %>%
    filter(Grade==0, Group.Category=="All Categories",
           Student.Group=="All students") %>%
    mutate(year = substr(Data.Year,4,5),
           Subject = tolower(Subject)) %>%
    select(District.Name, District.Type, District.Number, year, Subject, Percent.Proficient)
  
  oldnames <- c("District.Name",
                "Percent.Proficient")
  newnames <- sapply(c("name", "enrollment", "perc_white", "perc_frp"), function(x) paste(x, dat$year[[1]], sep=""))
}





