#  create_surveyID()  function
library(tidyverse)

## create surveyIDs
#' Title create_surveyID
#'  This creates a unique id for surveys.
#' @param data -- the surveylookup table
#'
#' @return -- a table with a unique ID for each survey that didn't have one
#' @export
#'
#' @examples
#' create_surveyID(surveylookup)
create_surveyID <- function(data) {
  print("inside create_surveyID function")
  
  #find largest ID
  if(sum(is.na(data$surveyID)) == length(data$surveyID)){
    maxID <- 0
  } else {
    maxID <- max(data$surveyID, na.rm = T)
  }
  # print("max ID = ")
  # print(maxID)
  qs_WO_surveyIDs <- data %>% 
    filter(is.na(surveyID)) %>% 
    mutate(noID_counter = row_number(),
           surveyID = noID_counter + maxID) %>% 
    select(-noID_counter) 
  
  updatedlookup <- data %>% 
    anti_join(select(qs_WO_surveyIDs, -surveyID)) %>%  # remove surveyIDs because otherwise the antiJoin won't work  --this drops the entries that have updated surveyIDs
    bind_rows(qs_WO_surveyIDs)
  
  ## test for distinct surveyID values
  if(length(unique(updatedlookup$surveyID)) == length(updatedlookup$surveyID)){
    return(updatedlookup)
  } else {
    #Identify the rows that have duplicated surveyIDs so that the user can see them
    dups <- updatedlookup %>% 
      group_by(surveyID) %>% 
      mutate(n = n()) %>% 
      filter(n>1)
    print("SurveyIDs are not distinct. See below.")
    print(dups)
    
    stop("IDs not distinct, surveyIDs not created.")
  }
  
}

# surveylookup <- create_surveyID(surveylookup)

