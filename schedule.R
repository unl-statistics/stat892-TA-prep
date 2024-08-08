library(dplyr)
library(tibble)
library(ggplot2)
library(lubridate)
library(forcats)
library(magrittr)
library(tidyr)

# Create a calendar for your syllabus ----
# Source: http://svmiller.com/blog/2020/08/a-ggplot-calendar-for-your-semester/

# 1) what is the first Monday of the semester?
# Any number of ways to identify dates in R, but we'll use {lubridate} and the ymd() function here.
# Format: YYYYMMDD. In this example, 4 January 2022.

# Weekday(s) of class

# What are the full dates of the semester? Here, I'll exclude exam week as I like to do.
# In this case: 6 January to 23 April
sem_boundary <- c(ymd(20240826), ymd(20241220))

semester_dates <- seq(sem_boundary[1], sem_boundary[2], by=1)

exam_week <- seq(ymd(20241216), ymd(20241220), by = 1)

cal_boundary <- seq(
  floor_date(min(sem_boundary), "month"),
  ceiling_date(max(sem_boundary), "month") - days(1), by = 1)


# Days where class is scheduled outside of normal times
extra_days <- c()

class_wdays <- c("Thu")

not_here_dates <- c(ymd(20240902), ymd(20241021), ymd(20241022),
                    seq(ymd(20241127), ymd(20241129), by = "day"))

# You can adjust this as you see fit. Basically: add assignment types (e.g. papers, quizzes).
# My intro class was fairly simple: just exams.
due_dates <- c(ymd(20241011), ymd(20241213))

# Custom function for treating the first day of the month as the first week
# of the month up until the first Sunday (unless Sunday was the start of the month)
wom <- function(date) {
  first <- wday(as.Date(paste(year(date),month(date),1,sep="-")))
  return((mday(date)+(first-2)) %/% 7+1)
}

# Create a data frame of dates, assign to Cal
Cal <- tibble(date = cal_boundary)  %>%
  mutate(mon = lubridate::month(date, label=T, abbr=F), # get month label
         wkdy = weekdays(date, abbreviate=T), # get weekday label
         wkdy = fct_relevel(wkdy, "Sun", "Mon", "Tue", "Wed", "Thu","Fri","Sat"), # make sure Sunday comes first
         semester = date %in% semester_dates, # is date part of the semester?
         due = date %in% due_dates, # is it an exam?
         not_here = date %in% not_here_dates, # is it a day off?
         exam_wk = date %in% exam_week,
         day = lubridate::mday(date), # get day of month to add later as a label
         # Below: our custom wom() function
         week = wom(date))

# Create a category variable, for filling.
# I can probably make this a case_when(), but this will work.

Cal <- Cal %>%
  mutate(category = case_when(
    due ~ "Due Date",
    not_here ~ "UNL holiday",
    semester & (wkdy %in% class_wdays | date %in% extra_days) & !not_here & !exam_wk ~ "Class Day",
    semester & exam_wk ~ "Finals",
    semester ~ "Semester",
    TRUE ~ "NA"
  )) |>
  mutate(week_of_year = week(date))

Sem_Week <- Cal |>
  filter(category == "Class Day") |>
  mutate(sem_week = 1:n()) |>
  select(week_of_year, sem_week)

Cal <- Cal |>
  left_join(Sem_Week, by = "week_of_year")


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
                             "Due Date"="orange",
                             "Finals" = "grey"),
                    #... but also suppress a label for a non-class semester day
                    breaks=c("Semester", "UNL holiday", "Class Day","Due Date", "Finals"))
# class_cal

exam_days <- filter(Cal, category == "Due Date") %>%
  mutate(topic = c("**Teaching Portfolio (1st 3 weeks)**",
                   "**Teaching Portfolio (1st 8 weeks)**"),
         time = c("12pm", "12pm"))

class_days <- filter(Cal, category == "Class Day") %>%
  mutate(topic = c(
    "Introductory Statistics Concepts",
    "Variability",
    "Variability",
    "Variability",
    "Sampling Distributions",
    "Sampling Distributions",
    "Sampling Distributions",
    "Interval Estimation",
    "Hypothesis Testing",
    "Hypothesis Testing",
    "Two-sample hypothesis testing",
    "The Big Picture",
    "The Big Picture",
    "Final Exams/Grading",
    "Wrap-up"),
    technique = c("Reflective writing, discussion",
                  "Assessment construction, peer review",
                  "In-class activities, Lesson plan construction",
                  "Peer review, class discussion",
                  "Collaborative definition construction, compare/contrast",
                  "Simulation and discussion",
                  "Benchmarks for sampling distributions",
                  "Assignment construction, rubrics",
                  "Reading, discussion, reflective writing",
                  "Reflective writing",
                  "Discussion",
                  "Projects, Discussion",
                  "Benchmarks for Student Learning",
                  "Activities and Assessments in Journals",
                  "Discussion")) %>%
  # bind_rows(project_days) %>%
  bind_rows(exam_days) %>%
  arrange(sem_week) %>%
  select(Date = date, Week = sem_week, Topic = topic, "Through the use of..." = technique) %>%
  arrange(Date)

