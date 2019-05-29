library(dplyr)

#download.file("https://www.seattleschools.org/UserFiles/Servers/Server_543/File/District/Departments/REA/school_reports/current/School_Report_Data_ADA.xlsx",
#              destfile = "School_Report_Data_ADA.xlsx")
raw_data <- readxl::read_excel("School_Report_Data_ADA.xlsx")

add_ordinal <- function(x){
  suff <-  case_when(x %in% c(11,12,13) ~ "th/73",
                     x %% 10 == 1 ~ 'st/73',
                     x %% 10 == 2 ~ 'nd/73',
                     x %% 10 == 3 ~'rd/73',
                     TRUE ~ "th/73")
  paste0(as.character(x), suff)
}

dec_percent <- function(x){paste0(as.character(floor(x * 100)), "%")}

es_raw <-
  raw_data %>%
  rename(name = `School name`, student_count = `# Students`, teacher_count = `# Teachers`,
  ela3 = `Proficient on state ELA test - Grade 3`,
  ela3_last = `Proficient on state ELA test - Grade 3 - Last year`,
  ela3_two = `Proficient on state ELA test - Grade 3 - Two years ago`,
  ela4 = `Proficient on state ELA test - Grade 4`,
  ela4_last = `Proficient on state ELA test - Grade 4 - Last year`,
  ela4_two = `Proficient on state ELA test - Grade 4 - Two years ago`,
  ela5 = `Proficient on state ELA test - Grade 5`,
  ela5_last = `Proficient on state ELA test - Grade 5 - Last year`,
  ela5_two = `Proficient on state ELA test - Grade 5 - Two years ago`,
  math3 = `Proficient on state Math test - Grade 3`,
  math3_last = `Proficient on state Math test - Grade 3 - Last year`,
  math3_two = `Proficient on state Math test - Grade 3 - Two years ago`,
  math4 = `Proficient on state Math test - Grade 4`,
  math4_last = `Proficient on state Math test - Grade 4 - Last year`,
  math4_two = `Proficient on state Math test - Grade 4 - Two years ago`,
  math5 = `Proficient on state Math test - Grade 5`,
  math5_last = `Proficient on state Math test - Grade 5 - Last year`,
  math5_two = `Proficient on state Math test - Grade 5 - Two years ago`) %>%
  mutate(ES_ZONE = trimws(gsub("Elementary", "", name))) %>%
  subset(GradeLevelID == "ES" & name != "District")

es_working <-
  es_raw %>%
  mutate(avg_attendance = as.numeric(`Average daily attendance`)) %>%
  mutate(att_rank = add_ordinal(rank(1-.$avg_attendance))) %>%
  mutate(student_teacher_ratio = student_count/as.numeric(teacher_count)) %>%
  mutate(student_teacher_ratio_rank = add_ordinal(rank(.$student_teacher_ratio))) %>%
  mutate(math_overall = as.numeric(`Math Proficiency: ALL`)) %>%
  mutate(math_rank = add_ordinal(rank(1-.$math_overall))) %>%
  mutate(ela_overall = as.numeric(`Reading Proficiency: ALL`)) %>%
  mutate(ela_rank = add_ordinal(rank(1-.$ela_overall))) %>%
  select(name, ES_ZONE, avg_attendance, att_rank, student_teacher_ratio, student_teacher_ratio_rank,
         ela3, ela4, ela5, math3, math4, math5,
         math_overall, math_rank,
         ela_overall, ela_rank, student_count, teacher_count)

es_linecharts <- es_raw %>% select(ES_ZONE,
         ela3, ela3_last, ela3_two,
         ela4, ela4_last, ela4_two,
         ela5, ela5_last, ela5_two,
         math3, math3_last, math3_two,
         math4, math4_last, math4_two,
         math5, math5_last, math5_two)

es_writeout <-
  es_working %>%
  select(ES_ZONE, att_rank, student_teacher_ratio_rank,
         math_overall, math_rank,
         ela3, ela4, ela5,
         math3, math4, math5, ela_overall,
         ela_rank, student_teacher_ratio, student_count, teacher_count) %>%
  mutate(ES_ZONE = gsub("School", "", ES_ZONE)) %>%
  mutate(ES_ZONE = gsub("West Seattle", "West Seattle Elem", ES_ZONE)) %>%
  mutate(ES_ZONE = gsub("International", "Int'l", ES_ZONE)) %>%
  mutate(ES_ZONE = gsub("Bailey Gatzert", "Gatzert", ES_ZONE)) %>%
  mutate(ES_ZONE = gsub("Daniel Bagley", "Bagley", ES_ZONE)) %>%
  mutate(ES_ZONE = gsub("Martin Luther King Jr." ,"MLK Jr.", ES_ZONE)) %>%
  mutate(ES_ZONE = gsub("John Hay", "Hay", ES_ZONE)) %>%
  mutate(ES_ZONE = gsub("Frantz Coe", "Coe", ES_ZONE)) %>%
  mutate(ES_ZONE = trimws(ES_ZONE, which = "both")) %>%
  mutate( math_overall = dec_percent(math_overall), ela3 = dec_percent(ela3),
          ela4 = dec_percent(ela4), ela5 = dec_percent(ela5),
          math3 = dec_percent(math3), math4 = dec_percent(math4),
          math5 = dec_percent(math5), ela_overall = dec_percent(ela_overall)) %>%
  mutate(student_teacher_ratio = paste0("1:", as.character(floor(student_teacher_ratio))))

write.csv(es_writeout, "es_stats.csv", row.names = F)
