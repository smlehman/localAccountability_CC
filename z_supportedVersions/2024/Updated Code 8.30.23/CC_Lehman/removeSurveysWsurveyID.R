## Identify the surveyIDs of the duplicated surveys using the surveylookup table.
## Create a vector of these ids

duplicateSurveyIDs <- 128:130
# or if discontinuous.. 
duplicateSurveyIDs <- c( 214) 

## now filter them out of the three dataframes where data from those surveys is stored.

# remove surveys from surveylookup
surveylookup <- surveylookup %>% 
  filter(!surveyID %in% duplicateSurveyIDs) #keep the surveyIDs that DO NOT match 
# those in duplicateSurveyIDs (the bang (!) reverses the match as in != )
## save the fixed version
saveRDS(surveylookup, "data/surveylookup.rds")

#remove questions from surveyQlookup
surveyQlookup <- surveyQlookup %>% 
  filter(!surveyID %in% duplicateSurveyIDs)
## save the fixed version
saveRDS(surveyQlookup, "data/surveyQlookup.rds")

## remove responses from surveyResponses
surveyResponses <- surveyResponses %>% 
  filter(!surveyID %in% duplicateSurveyIDs)
## save the fixed version
saveRDS(surveyResponses, "data/surveyResponses.rds")

## I left this as repetative code rather than creating a function so it would 
## be easier to see what's happening. 