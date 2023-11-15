# get data & pkgs ####

library(tidyverse)
library(magrittr)

link <- "https://harrypotter.fandom.com/wiki/House_points#1996%E2%80%931997_school_year"
pointer <- read_html(link)

table <- pointer %>% html_elements(".wikitable") %>% html_table()

hp_tables <- tibble()

# only HP schoolyears
for (i in seq(4,9)) {
  hp_tables <- bind_rows(table[[i]] %>% mutate(schoolyear_end = 1988+i)
                         , hp_tables)
}

# mutate points to number ####

hp_tables %<>% mutate(points = case_when(str_starts(`Points gained/lost`, "Several") ~ NA, 
                                         TRUE ~ ifelse(str_detect(`Points gained/lost`, "deducted"),
                                                       parse_number(`Points gained/lost`) * -1,
                                                       parse_number(`Points gained/lost`)
                                                       )
                                         ),
                      plus_minus = ifelse(str_detect(`Points gained/lost`, "deducted"),
                                          "deducted",
                                          "awarded")
                      )


# solve special cases ####

special_cases <- hp_tables %>% filter(str_detect(`Points gained/lost`, "each"))

hp_tables %<>% anti_join(., special_cases) # filter out special cases

special_cases %<>% mutate(Student = str_split(Student, "\n|and")) %>% unnest() # points for each
special_cases %<>% mutate(points = ifelse(str_detect(`Points gained/lost`, "10 for Neville") & Student == "Neville Longbottom",
                                         10,
                                         points))

hp_tables %<>% bind_rows(., special_cases) # rejoin

# shared points:
shared_points <- hp_tables %>% filter(str_detect(Student, "\n|and"))
hp_tables %<>% anti_join(., shared_points) # filter out special cases
shared_points %<>% mutate(Student = str_split(Student, "\n|and"),
                         points = points / length(Student)
                         ) %>% unnest()
hp_tables %<>% bind_rows(., shared_points)
hp_tables %<>% mutate(Student = str_trim(Student),
                      Authority = sub(" \\(on Severus Snape's behalf\\)", "", Authority))



# analysis ####
hp_tables %>% group_by(Student) %>% summarise(sum_points = sum(points, na.rm = T)) %>% arrange(desc(sum_points))

hp_tables %>% filter(plus_minus == "awarded") %>% group_by(Student) %>% summarise(sum_points = sum(points, na.rm = T)) %>% arrange(desc(sum_points))

hp_tables %>% filter(plus_minus == "deducted") %>% group_by(Student) %>% summarise(sum_points = sum(points, na.rm = T)) %>% arrange(sum_points)

hp_tables %>% group_by(Authority, House) %>% summarise(sum_points = sum(points, na.rm = T))

hp_tables %>% group_by(House) %>% summarise(sum_points = sum(points, na.rm = T)) %>% arrange(desc(sum_points))
