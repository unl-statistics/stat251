
library(tidyverse)
library(lubridate)

# Create a calendar for your syllabus ----
# Source: http://svmiller.com/blog/2020/08/a-ggplot-calendar-for-your-semester/

# 1) what is the first Monday of the semester?
# Any number of ways to identify dates in R, but we'll use {lubridate} and the ymd() function here.
# Format: YYYYMMDD. In this example, 4 January 2022.

# Weekday(s) of class
class_wdays <- c("Tue", "Thu")

# Spring Break was 13 March 2022 to 20 March 2022.
not_here_dates <- c(
  # ymd("20220117"),
  # Spring Break
  seq(ymd(20230312),ymd(20230318), by=1))

# You can adjust this as you see fit. Basically: add assignment types (e.g. papers, quizzes).
# My intro class was fairly simple: just exams.
exam_dates <- c(ymd(20230309), ymd(20230518))

# What are the full dates of the semester? Here, I'll exclude exam week as I like to do.
# In this case: 6 January to 23 April
semester_dates <- seq(ymd(20230123), ymd(20230519), by=1)

exam_week <- seq(ymd(20230515), ymd(20230519), by = 1)

# Custom function for treating the first day of the month as the first week
# of the month up until the first Sunday (unless Sunday was the start of the month)
wom <- function(date) {
  first <- wday(as.Date(paste(year(date),month(date),1,sep="-")))
  return((mday(date)+(first-2)) %/% 7+1)
}

# Create a data frame of dates, assign to Cal
Cal <- tibble(date = seq(ymd(20230101), ymd(20230530), by=1))  %>%
  mutate(mon = lubridate::month(date, label=T, abbr=F), # get month label
         wkdy = weekdays(date, abbreviate=T), # get weekday label
         wkdy = fct_relevel(wkdy, "Sun", "Mon", "Tue", "Wed", "Thu","Fri","Sat"), # make sure Sunday comes first
         semester = date %in% semester_dates, # is date part of the semester?
         exams = date %in% exam_dates, # is it an exam?
         not_here = date %in% not_here_dates, # is it a day off?
         exam_wk = date %in% exam_week,
         day = lubridate::mday(date), # get day of month to add later as a label
         # Below: our custom wom() function
         week = wom(date))

# Create a category variable, for filling.
# I can probably make this a case_when(), but this will work.

Cal <- Cal %>%
  mutate(category = case_when(
    exams ~ "Exam",
    not_here ~ "UNL holiday",
    semester & wkdy %in% class_wdays & !not_here & !exam_wk ~ "Class Day",
    semester ~ "Semester",
    TRUE ~ "NA"
  ))
# mutate(category = NA,
#        category = ifelse(semester == 1, "Semester", category),
#        category = ifelse(semester == 1 & wkdy %in% c("Wed"), "Class Day", category),
#        category = ifelse(exams == 1, "Exams", category),
#        category = ifelse(is.na(category) | (semester == 1 & not_here == 1), "NA", category)) -> Cal

class_cal <- Cal %>%
  ggplot(.,aes(wkdy, week)) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        legend.position = c(1, 0), legend.justification = c(1,0), legend.direction = "vertical", legend.title = element_blank(),
        axis.title.y = element_blank(), axis.title.x = element_blank(), axis.ticks.y = element_blank(), axis.text.y = element_blank()) +
  # geom_tile and facet_wrap will do all the heavy lifting
  geom_tile(alpha=0.8, aes(fill=category), color="black", linewidth=.45) +
  facet_wrap(~mon, scales = "free", ncol=3) +
  # fill in tiles to make it look more "calendary" (sic)
  geom_text(aes(label=day, color = semester&(!not_here))) +
  # put your y-axis down, flip it, and reverse it
  scale_y_reverse(breaks=NULL) +
  # manually fill scale colors to something you like...
  scale_color_manual(values = c("FALSE" = "white", "TRUE" = "black"), guide = "none") +
  scale_fill_manual(values=c("Class Day"="purple",
                             "Semester"="white",
                             "UNL holiday" = "grey10",
                             "NA" = "white", # I like these whited out...
                             "Exam"="orange"),
                    #... but also suppress a label for a non-class semester day
                    breaks=c("Semester", "UNL holiday", "Class Day","Exam"))
# class_cal

exam_days <- filter(Cal, category == "Exam") %>%
  mutate(topic = c("Midterm", "Final"),
         time = c("In Class", "1-3 pm"))

class_days <- filter(Cal, category == "Class Day") %>%
  mutate(topic = c(
    "Getting Started",
    "Version Control",
    "Review: General Programming",
    "Review: General Programming",
    "Data Input",
    "Data Input",
    "Data Visualization",
    "Data Visualization",
    "Data Verbs",
    "Data Verbs",
    "Data Cleaning",
    "Data Cleaning",
    "Strings",
    "Reshaping Data",
    "Reshaping Data",
    "Joining Data",
    "Joining Data",
    "Dates and Times",
    "Dates and Times",
    "Lists",
    "Lists",
    "Spatial Data",
    "Spatial Data",
    "Special Topics",
    "Special Topics",
    "Special Topics",
    "Special Topics",
    "Special Topics",
    "Special Topics")) %>%
  bind_rows(exam_days) %>%
  arrange(date)
