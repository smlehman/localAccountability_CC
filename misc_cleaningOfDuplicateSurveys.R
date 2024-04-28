
schoolNumlookup <- data.frame(
  stringsAsFactors = FALSE,
                               School = c("CCHS","CCMS","CES",
                                          "Harrison","Lincoln","McKinley",
                                          "Washington"),
                      cdeSchoolNumber = c(1266L,1262L,7950L,3802L,
                                          5166L,5704L,9248L)
                   ) %>% 
  mutate(cdeSchoolNumber = as.character(cdeSchoolNumber))

ratingResponses_fix <- ratingResponses %>% 
  left_join(schoolNumlookup)


#################

surveylookup_wNew <- surveylookup %>% 
  mutate(link = case_when(
    endYear == 2022 & cdeSchoolNumber == "1262"& type == "rating" ~ "https://docs.google.com/spreadsheets/d/1VDoTIGPUQ1P_O30cVqLGad2dzHDS-z7Y4DrgCt_6RaE/edit#gid=167419926",
  T ~ link))


filter(surveylookup_wNew, endYear == 2022 & cdeSchoolNumber == "1262"& type == "rating")$link


https://docs.google.com/spreadsheets/d/1VDoTIGPUQ1P_O30cVqLGad2dzHDS-z7Y4DrgCt_6RaE/edit#gid=167419926

saveRDS(surveylookup_wNew, "data/surveylookup.rds")
###############


surveylookupRev <- readRDS("data/surveylookup.rds") %>% 
  mutate(surveyMonkey = case_when(
    type == "survey"&str_detect(link, ".*.xlsx") ~ T,
    type == "survey"&is.na(surveyMonkey) ~ F,
    type=="rating" ~ NA,
    T ~ surveyMonkey
  ))


surveylookupRev <- surveylookup %>% 
  mutate(edLevel = case_when(
    str_detect(tolower(link), ".*elementary.*|.*grade 2-5.*")~"elementary",
    str_detect(tolower(link), ".*middle.*")~"middle",
    T ~ edLevel
  ))

sum(is.na(surveylookup$edLevel))
sum(is.na(surveylookupRev$edLevel))

saveRDS(surveylookupRev, "data/surveylookup.rds")




test <- count(ratingResponses, endYear, cdeSchoolNumber, School)
test1 <- count(ratingResponses, endYear)

test2 <- surveylookup %>% 
  filter(endYear == 2023, cdeSchoolNumber %in% c("1262"), type == "rating")

unique(surveylookup$edLevel)



surveylookupDedup <- surveylookup %>% 
  mutate(edLevel = case_when(
    edLevel == "elementry" ~ "elementary",
    edLevel == "High School" ~ "high",
    T ~ edLevel
  )) %>% 
  add_count(endYear, cdeSchoolNumber, type, respondents, edLevel)

dups <- surveylookupDedup %>% 
  group_by(endYear, cdeSchoolNumber, type, respondents, edLevel) %>% 
  filter(n>1) %>% 
  filter(surveyID == min(surveyID))

surveylookupRev <- surveylookup %>% 
  anti_join(dups)


ratingResponsesRev <- ratingResponses %>% 
  semi_join(surveylookup, by = "surveyID")

saveRDS(ratingResponsesRev, "data/ratingResponses.rds")

surveyResponseRev <- surveyResponses %>% 
  semi_join(surveylookup, by = "surveyID")

saveRDS(surveyResponseRev, "data/surveyResponses.rds")

surveyQlookupRev <- surveyQlookup %>% 
  semi_join(surveylookup, by = "surveyID")

saveRDS(surveyQlookupRev, "data/surveyQlookup.rds")

surveyQlookup <- readRDS("data/surveyQlookup.rds")


test <- surveyResponses %>% 
  count(endYear, cdeSchoolNumber, surveyID ) %>% 
  left_join(surveylookup, by = c("surveyID", "endYear")) %>% 
  filter(endYear == 2024)

test3 <- surveylookup %>% 
  add_count(endYear, cdeSchoolNumber, type, respondents, edLevel)

surveyraw1 <- unique(surveyDataRaw1$`What School does your child attend?`)
surveyraw1
[1] "Response"                                        "Canon Exploratory School (CES)"                 
[3] "Lincoln School of Science and Technology (LSST)" "CCHS"                                           
[5] "CCMS"                                            "McKinley"                                       
[7] NA                                                "Harrison K-8"                                   
[9] "Mountain View Core Knowledge"                    "Washington"  


