### survey and rating data processing
### Overveiw: This script creates a set of data structures that are able to hold
# most types of survey/rating information. The flexibility of this type 
# of system relies on the a set of lookup tables that enable an abstracted 
# dataset. It is important to note that, although the survey data function can read in 
# excel/csv data from archived survey results, it is most heavily tested with googlesheets, 
# since this appears to be the format to be used going forward. 
#
# There are four lookup tables.  
## 1. Lookup table for surveys/ratings - surveylookup.rds
## 2. Lookup table for questions - surveyQlookup.rds
## 3. Lookup table for question options - optionsQlookup.
## 4. Lookup specifically for survey demographic questions and their options - demosTable
# These inform information stored in two datasets: 
## Rating Data: ratingResponses.rds
## Survey Data: surveyResponses.rds
# Dataset data are long--one row for each response.  This enables easy group_by's
# and plotting. 

## Outline of Code. 
## The first half of the code cleans, stores and shows display examples of Survey data.
# The second half of the code cleans, stores and shows display examples of Rating data


library(tidyverse)
library(fst)
library(googlesheets4)
library(googledrive)
library(fuzzyjoin)

#### Addins that you will want
# install.packages("editData")
require(editData)
require(datapasta)
### editData instructions  https://cran.r-project.org/web/packages/editData/vignettes/editData.html

## For convenience I often create simple lookup tables in excel or googlesheets, copy them and use the datapasta addin to paste as a tribble or dataframe
# install.packages(datapasta)

### load the google table with survey and rating sheet links
drive_auth()

gs4_auth() ## choose 0 and reauthorize if you're getting blocked


# 

# Process: 
## Prework: check to determine extent to which data are consistent with prior data
# 1. Add surveys or rating sheets as rows in surveylookup table.
# 2. Run surveyID creator function to load it into the environment
# 2a. Check relevant variables as prompted by script.
# 3. Run either the survey or rating extraction function(s)
# 4 Save data to local files

surveylookup <- readRDS("data/surveylookup.rds")


# Code to add records to surveylookup.rds ---------------------------------

# ###### use this to create survey ids if needed .............................
### same basic funtion as in create_surveyIDs_fun.R
# source("create_surveyIDs_fun.R", local = T)
source("utilities.R",local = T)
schoolinfo <- schoolsTable %>% 
  select(cdeSchoolNumber, edLevel, schoolName, schoolNameShort)

#find current EndYear
if(lubridate::week(Sys.Date())<33){ ##week 33 is mid-August
  surveyendyear <- lubridate::year(Sys.Date())} else {surveyendyear <- lubridate::year(Sys.Date())+1}

# data <- surveylookup ## set data to surveylookup if running the code inside of the create surveyIDs function 
## create surveyIDs
#' Title create_surveyID
#'  This creates a unique id for surveys.
#' @param data -- new entries to be added to the surveylookup table
#' @param lookupTable -- the surveylookup table 
#'
#' @return -- a table with a unique ID for each survey that didn't have one
#' @export
#'
#' @examples
#' create_surveyID(surveylookup)
create_surveyID <- function(data, lookupTable = surveylookup) {
  print("inside create_surveyID function")
  
  #debugging values
  # data <- surveylookup_wNew #-- before using create_surveyID in the pipe
  # lookupTable <- surveylookup
  
  #find largest ID
  if(sum(is.na(lookupTable$surveyID)) == length(lookupTable$surveyID)){
    maxID <- 0
  } else {
    maxID <- max(lookupTable$surveyID, na.rm = T)
  }
  # print("max ID = ")
  # print(maxID)
  #find surveys matching incoming surveys
  naturalKey <- c("cdeSchoolNumber", "respondents", "type", "endYear", "edLevel")
  dups <- lookupTable %>% 
    semi_join(data, by = naturalKey) %>%    # find duplicates so we can grab their surveyIDs
    select(all_of(naturalKey), surveyID)
  
  #drop the dups from surveyLookup
  lookupNoDups <- lookupTable %>% 
    anti_join(dups, by = "surveyID")
 
  #join existing surveyID to entries replacing dups + bind old rows
  data_wReplacedIDs <- data %>% 
    left_join(dups, by = naturalKey) %>%  #this will add the existing id numbers to new entries
    filter(!is.na(surveyID))
  
  
  updatedlookup <- data %>% 
    anti_join(dups, by = naturalKey) %>% 
    mutate(noID_counter = row_number(),
           surveyID = noID_counter + maxID) %>% 
    select(-noID_counter) %>% ## new data now has surveyID
    bind_rows(lookupNoDups) %>% ## add old surveylookup data
    bind_rows(data_wReplacedIDs)  ## add old surveylookup data with refreshed links and 
  

  
  ## test for distinct surveyID values
  if(length(unique(updatedlookup$surveyID)) == length(updatedlookup$surveyID)){
    return(updatedlookup)
  } else {
    #Identify duplicated surveyIDs and send error message
    dups <- updatedlookup %>% 
      group_by(surveyID) %>% 
      mutate(n = n()) %>% 
      filter(n>1)
    print("SurveyIDs are not distinct")
    print(dups)
    
    stop("IDs not distinct, surveyIDs not created.")
  }
  
}
## testing function...run the line below if you'd like to test the create_surveyID function
# surveylookup <- create_surveyID(surveylookup)


### TODO this needs to be formalized for both surveys and ratings....

####### ADDING RATINGS (or SURVEYS) to surveylookup ##########
## get info for populating surveylookup



## This is a manual part. Ensure that everything is correct or things will go wrong.
## The simplest way to do this is to create a table in a spreadsheet and then convert it to an 
## R object using datapasta. Below is an example.

## get surveylookup starter table https://docs.google.com/spreadsheets/d/1C0Q6tCJxbOisMxdaoUSGi3AiTSjrKw9ZR88V_kMFG4s/edit?usp=sharing 
# or use the table below as a template for adding in instances of surveys or ratings
#
## add survey files or rating files to the spreadsheet. Then copy and paste below using dataPasta
############ Example of creating base surveylookup info in spreadsheet and datapasta-ing it in.############
## Lookup column definitions: 
# link = link to google file or path to local file
# respondents : one of unique(surveylookup$respondents) --c("educator", "parent" ,  "student"  ,"raters" )
# type: one of c("survey", "rating")
# final: binary (1/0) whether the file is a final file or an intermediate file (intermediate files are not loaded in the current code)
### datapasta in the table you've created in a spreadsheet


## This is just an EXAMPLE  it is overwritten in the next code block __________________________________
# surveylookup_wNew <- data.frame(
#      stringsAsFactors = FALSE,
#   school_fromFileName = c("CCMS", "MCKI", "CCHS"),
#                  link = c("https://docs.google.com/spreadsheets/d/1S4f00m2wXOdD5XcWBWp6ITS88F06Ahr7BcZlZfZzDeM/edit#gid=167419926", "https://docs.google.com/spreadsheets/d/1KtGSScDTeniOyx2vu8ZxQgX9niAhAB-Hx1Duig-vFbE/edit#gid=167419926", "https://docs.google.com/spreadsheets/d/1_0xJPqwSrJF087-5CgE6oTiVgNVjmmeZXeHhow1oPec/edit#gid=167419926")
# ,
#       cdeSchoolNumber = c(1262L, 5704L, 1266L),
#           respondents = c("raters", "raters", "raters"),
#                  type = c("rating", "rating", "rating"),
#               endYear = c(2023L, 2023L, 2023L),
#                 final = c(1L, 1L, 1L),
#               edLevel = c(NA, NA, NA),
#          surveyMonkey = c(FALSE, FALSE, FALSE)
# ) %>% 
#   mutate(cdeSchoolNumber = as.character(cdeSchoolNumber)) %>% 
#   left_join(schoolinfo, by = "cdeSchoolNumber") %>% ## add in school information
#   mutate(edLevel = case_when(
#     is.na(edLevel.x) ~ edLevel.y,
#     TRUE ~ edLevel.x
#   )) %>% ## fill in edLevel values from schoolinfo for school-specific data
#   select(-edLevel.x, -edLevel.y) ## drop join-created columns
  
  

### Code to paste datapasta-ed table into. It's same as code above...
### 1. highlight/delete **PASTE_DATAPASTA_HERE** 
### 2. replace with the code that datapasta generates.-- 
###    a. you do this by clicking "Addins" > "Paste as dataframe "  (works better than `paste as tribble` because it's easier to add values to the `link` column afterward (see a1. below)     
            ### a1. Some links won't paste as tribble or DF -- 1) cut the links from the table created above (leave the column header) and paste below the table you created. 2) Copy the main table. 3) Use `paste as DF` to paste the table code below...this will leave NAs where the links should be. 4)  Go back to your spread sheet, highlight and copy the links (do not include a header row), 5) Paste the links using "paste as vector" to replace the NAs in the df you've created by highlighting the c(NA....NA) of the 'link = ' vector.

# select "data.frame(....)" and use Addins > "Paste as data.frame" to add your new lookup information

utils::browseURL("https://docs.google.com/spreadsheets/d/1C0Q6tCJxbOisMxdaoUSGi3AiTSjrKw9ZR88V_kMFG4s/edit?usp=sharing")

surveylookup_wNew1 <- data.frame(
     stringsAsFactors = FALSE,
  school_fromFileName = c("CCMS","MCKI","CCHS",
                          "LINC","HARR","CES","WASH",NA,NA,NA,NA,"CCHS",
                          "CCMS","CES","HARR","LINC","MCKI","MVCK","WASH",
                          NA,NA,NA,NA),
                 link = c("https://docs.google.com/spreadsheets/d/1S4f00m2wXOdD5XcWBWp6ITS88F06Ahr7BcZlZfZzDeM/edit#gid=167419926", "https://docs.google.com/spreadsheets/d/1KtGSScDTeniOyx2vu8ZxQgX9niAhAB-Hx1Duig-vFbE/edit#gid=167419926", "https://docs.google.com/spreadsheets/d/1_0xJPqwSrJF087-5CgE6oTiVgNVjmmeZXeHhow1oPec/edit#gid=167419926", "https://docs.google.com/spreadsheets/d/1uvLHTrR2TmUjjvTMC0_5kBP4zTkf7TJHxw-ien1hWGQ/edit#gid=167419926", "https://docs.google.com/spreadsheets/d/1UXfsN_n7QO8z0vhiYwnNrfI1_uh57aFQyMKOmd1uK8U/edit#gid=167419926", "https://docs.google.com/spreadsheets/d/1s-zb_3u3q6UScv__M8UVB2RaNjcD5wypKSscmGZO5w8/edit#gid=167419926", "https://docs.google.com/spreadsheets/d/1yR8VnwdGF-pBFlT6zxJwbhpXl116RaYC5eIhLQ11cQY/edit#gid=167419926", "https://docs.google.com/spreadsheets/d/1MXY-eXKkG-Nvfeb3FdvPn-j6jtQ0ncgL6xR4cGAm3YE/edit#gid=1118038864", "https://docs.google.com/spreadsheets/d/1R5UdBjgqLQXseNMDL2gMPqkMwtcnUJGvB_sXyRMtQZ8/edit#gid=1058061178", "https://docs.google.com/spreadsheets/d/1APDvD_j24XakjT3Q5FEO83ht7I4DtHnh_G8BL6xoU2A/edit#gid=1887856803", "https://docs.google.com/spreadsheets/d/1FlXC9iqRjZPz2hRPwZCj6FqDGdGr6R4Boa4zy6tGjt0/edit#gid=967405629", "https://docs.google.com/spreadsheets/d/1CbSSTXS87MDuQpk3v4vsZTet1WfLFx42Ef5L0gaUyfk/edit#gid=167419926", "https://docs.google.com/spreadsheets/d/1FISa0JHDbqClQE_DhgfaRst3V4SQJmSaoM9l9mQA73w/edit#gid=167419926", "https://docs.google.com/spreadsheets/d/17P8NVKLl57LLFjwxQ1LRIyQGcDUWOvPIvJv1D5uJrVI/edit#gid=167419926", "https://docs.google.com/spreadsheets/d/1KP1N-J1EBhPlmvnwRbzuEW-OR0gGbF9q7kblRyFg-LQ/edit#gid=167419926", "https://docs.google.com/spreadsheets/d/1TqffYRjETqgJ1iP2d3wmTX6YAqD2Ur_dsq1DqZvbc-c/edit#gid=167419926", "https://docs.google.com/spreadsheets/d/14ckwBve1JrwUPEb1iT_7PTA8teyFQhq2Zom1RNIu-10/edit#gid=167419926", "https://docs.google.com/spreadsheets/d/1LR1-GRgKB9ojt9M_p9te5WuPPQ_qmb_CdYTbbaAVlyg/edit#gid=167419926", "https://docs.google.com/spreadsheets/d/1wOFs9zljosgGp6DFZrqNW4gwhSpDTudLdKMxYIkBHFA/edit#gid=167419926", "https://docs.google.com/spreadsheets/d/1vmJP_H4TjczONf-PKsti2rlV2vpPhiFrsFq4WtPoaw0/edit#gid=72888980", "https://docs.google.com/spreadsheets/d/1XDtbsQLQ3cF_prt9vsF8MF8evLYnVXSwFoExUZ3d658/edit#gid=1533868882", "https://docs.google.com/spreadsheets/d/1C0Y5NmBl8mTQ35D4Qab-J1kNjkWUIurrtEKIykJKd2I/edit#gid=1889648776", "https://docs.google.com/spreadsheets/d/1ObKMwtjFfXq6Isl1zsHSs4HEgfA9DmfsgnewucDbnQQ/edit#gid=628143413")
,
      cdeSchoolNumber = c(1262L,5704L,1266L,
                          5166L,3802L,7950L,9248L,NA,NA,NA,NA,1266L,1262L,
                          7950L,3802L,5166L,5704L,6752L,9248L,NA,NA,NA,
                          NA),
          respondents = c("raters","raters",
                          "raters","raters","raters","raters","raters","student",
                          "parent","student","student","raters","raters",
                          "raters","raters","raters","raters","raters",
                          "raters","parent","student","student","student"),
                 type = c("rating","rating",
                          "rating","rating","rating","rating","rating","survey",
                          "survey","survey","survey","rating","rating",
                          "rating","rating","rating","rating","rating",
                          "rating","survey","survey","survey","survey"),
              endYear = c(2023L,2023L,2023L,
                          2023L,2023L,2023L,2023L,2023L,2023L,2023L,2023L,
                          2024L,2024L,2024L,2024L,2024L,2024L,2024L,2024L,
                          2024L,2024L,2024L,2024L),
                final = c(1L,1L,1L,1L,1L,1L,
                          1L,1L,1L,1L,1L,1L,1L,1L,1L,1L,1L,1L,1L,1L,
                          1L,1L,1L),
              edLevel = c(NA,NA,NA,NA,NA,NA,
                          NA,"elementary",NA,"middle","high",NA,NA,NA,NA,
                          NA,NA,NA,NA,NA,"elementary","middle","high"),
         surveyMonkey = c(FALSE,FALSE,FALSE,
                          FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,
                          FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,
                          TRUE,TRUE,TRUE,TRUE)
) %>% 
  mutate(cdeSchoolNumber = as.character(cdeSchoolNumber)) %>% 
  left_join(schoolinfo, by = "cdeSchoolNumber") %>% ## add in school information
  mutate(edLevel = case_when(
    is.na(edLevel.x) ~ edLevel.y,
    TRUE ~ edLevel.x
  )) %>% ## fill in edLevel values from schoolinfo for school-specific data
  select(-edLevel.x, -edLevel.y) ## drop join-created columns

### check values of surveylookup entries
##link = link to google file or path to local file
# respondents : one of unique(surveylookup$respondents) --c("educator", "parent" ,  "student"  ,"raters" )
# type: one of c("survey", "rating")
# final: binary (1/0) whether the file is a final file or an intermediate file (intermediate files are not loaded in the current code)

## The following function runs code to check for valid values.
#' Title
#'            NO ARGUMENTS -- you just need the surveylookup_wNew1 table in your environment
#' @return -- message that identifies invalid values
#' @export
#'
#' @examples
runLookupCheck <- function() {
  
  respondentVec <- c("educator", "parent" ,  "student"  ,"raters" )
  typeVec <- c("survey", "rating")
  finalVec <- c(0,1)
  cdeSchoolNumberVec <- c(unique(schoolsTable$cdeSchoolNumber), NA)
  edLevelVec <- c("elementary", "middle", "high", "K-8", NA)
  
  respondentTest <- surveylookup_wNew1$respondents[!surveylookup_wNew1$respondents %in% respondentVec]
  typeTest <- surveylookup_wNew1$type[!surveylookup_wNew1$type %in% typeVec]
  finalTest <- surveylookup_wNew1$final[!surveylookup_wNew1$final %in% finalVec]
  cdeSchoolNumberTest <- surveylookup_wNew1$cdeSchoolNumber[!surveylookup_wNew1$cdeSchoolNumber %in% cdeSchoolNumberVec]
  edLevelTest <- surveylookup_wNew1$edLevel[!surveylookup_wNew1$edLevel %in% edLevelVec]
  
  ## get all the test names for checking
  allTestNames <- c("respondentTest", "typeTest", "finalTest", "cdeSchoolNumberTest", "edLevelTest")
  ## combine all tests for quick check
  allTest <- c(respondentTest, typeTest, finalTest, cdeSchoolNumberTest, edLevelTest)
  
  
#' Title
#'
#' @param colTestString string - name of column Test variable as a string
#'
#' @return message regarding validity of values in column
#' @export
#'
#' @examples
  reportValueTest <- function(colTestString) {
    # colTestString = "typeTest"
    # colTestString = "edLevelTest"
    colName <- str_remove(colTestString, "Test")
    colTest <- get(colTestString)
    
    if(length(colTest) == 0){
      cat(glue::glue("Column `{colName}` has valid values.\n"))
      cat("\n")
    } else if(length(colTest) > 0) {
      warning(glue::glue("|***Column `{colName}` has the following invalid value: `{colTest}`   "))
  
    }
    
  } ## end function
 
  
  ### check for valid values
  if(sum(length(allTest)) == 0) {
    colNames <- str_remove(allTestNames, "Test")
    print(glue::glue("Values for `{colNames}` are all valid."))
  } else if(sum(length(allTest)) > 0) {
    
    ## find the values that are incorrect with reportValueTest
    walk(allTestNames, reportValueTest) 
    warning("Errors found: You can fix these using the editData addin.")
  }
  return(allTest)
}

## Check lookup for valid values
allTest <- runLookupCheck()

## If there are errors, fix invalid values with edit data (the previous code listed invalid values in a warning message)
if(sum(length(allTest)) > 0) {
  require(editData)
  surveylookup_wNew1 <- editData(surveylookup_wNew1)
}

## If there were errors re-Check lookup after using editData
if(sum(length(allTest)) > 0) {
  allTest <- runLookupCheck()
}



### finalize new entries to the lookup table 
## !!!! IMPORTANT !!!! this will remove duplicate surveys  Previously saved data associated with duplicate surveys will be overwritten in this table and in the surveyResponses and ratingResponses tables when the extraction process for surveys and ratings is run below.
surveylookup_wNew <- surveylookup_wNew1 %>% 
  create_surveyID() 



warning("Make sure you check surveylookup_wNew before saving...") 
View(surveylookup_wNew) ## you should see the surveys you added with ID numbers

saveRDS(surveylookup_wNew, "data/surveylookup.rds")


surveylookup <- readRDS("data/surveylookup.rds")
### With the new data added to the surveylookup table, you can run either the survey read function or the rating read functions. The rating functions are below the survey read function. 




################### SURVEY read function ##############################

### set some test values for the function. You can run these if you want to test the function
# fileID <- surveylookup$link[1] ## example to test
# schoolNum <- surveylookup$cdeSchoolNumber[1]
# 
# fileID <- surveylookup$link[55]
# schoolNum <- surveylookup$cdeSchoolNumber[55]
# 
# fileID <- surveylookup$link[56]
# schoolNumber <- surveylookup$cdeSchoolNumber[56]



## Set variables to id certain questions. 
warning("review new questions to ensure that the regex below will catch the school Q's and the demo Q's")  ## You'll need to know a little about regular expressions (regex)  - https://www.regular-expressions.info/tutorial.html

### Regex to recognize demographic and school questions. This is used in the readSurveyData function
demoQ_text <- tolower(c("What grade|Which Pathway|I am.*"))
schoolQ_text <-  "i go to school at.*|i attend.*|what is your school.*|please select your school.*|what school does your child attend.*|please select your school.*"


### load functions

## function to strip out surveyMonkey junk columns and rows
#' Title
#'
#' @param rawData -- raw data from surveyMonkey
#'
#' @return
#' @export
#'
#' @examples
preProcessSurveyMonkey <- function(rawData) {
  ##specify column names that are unnecessary (surveymonkey cols)
  dropCols <- c("Collector ID"
                #,"Start Date"
                ,"End Date"
                ,"IP Address"
                ,"Email Address"                                                                                                                                 
                ,"First Name"                                                                                                                                         
                ,"Last Name"
                ,"Custom Data 1"  
  )
  
  
  surveyMonkeyID <- rawData[[2, "Collector ID"]]
  
  ## stamdardize surveymonkey format
  surveyDataRaw <- rawData %>% 
    rename(respondentID = `Respondent ID`) %>% 
    select(-all_of(dropCols)) 
  
  # extract response type var that is nest in surveymonkey table -- this assumes that the survey data
  #start in column 10
  openQs <- rawData[1,10:length(names(rawData))] %>% 
    pivot_longer(cols = names(rawData)[10:length(names(rawData))], names_to = "qText", values_to = "responseType") %>% 
    mutate(responseType = str_extract(tolower(responseType), "open")) %>% 
    filter(!is.na(responseType))
  
  surveyDataRaw <- surveyDataRaw[-1,] ## dropping the responseType row.
  
  return(list("openQs"=openQs, "surveyDataRaw"=surveyDataRaw))
}
#' Title readSurveyData
#'
#'NOTES: Open-ended question are identified by having more than 15 different responses (Kind of a weak criteria, but it's a starting point)
#'Issue 1: if there selected response items with more than 15 options it will be identified as a open response
#'Issue 2: if there are fewer 15 respondents, no item will show up as an open response
#'
#' @param fileID the url of the googlesheet or the name of the excel file (with .xlsx) to be read
#' @param schoolNumber - character -- cde school number
#' @param .demoQ_text - string with regex of to match lowercase demo questions. e.g. tolower(c("What grade|Which Pathway|I am"))
#' @param .schoolQ_text - string with regex for matching the school qs of all surveys
#' @param surveyMonkey - boolean that specifies whether the file is an excel file that came from survey monkey
#'
#' @return List with a standardized data table 'surveyResponses' and a question lookup 'sureveyQlookup'
#' @export
#'
#' @examples test <- readSurveyData()
readSurveyData <- function(fileID,
                           schoolNumber = NA,
                           surveyMonkey,
                           .demoQ_text = demoQ_text,
                           .schoolQ_text = schoolQ_text) {
  # counter <- 3
  # fileID= surveyProcessInfo$fileID[counter]
  # schoolNumber=surveyProcessInfo$schoolNumber[counter]
  # surveyMonkey=surveyProcessInfo$surveyMonkey[counter]
  # .demoQ_text = demoQ_text
  # .schoolQ_text = schoolQ_text

  
  ##clean up extra characters that may have been generated by encoding the file name string
  fileID <- iconv(fileID, "", "ASCII", sub = " ")
  
  
  ## ensure the school number doesn't have spaces
  schoolNumber <- str_trim(schoolNumber, side = "both")
  
  surveylookup_filtered <- surveylookup %>% filter(link == fileID)
  surveyid <- surveylookup_filtered$surveyID[1]
  # Conditional to read xlsx and google differently -------------------------
  if (str_detect(fileID, ".xlsx$")) {
    #### READ EXCEL SHEET ###########
    dataFolder <- "data_raw/canonCity/surveyData/surveymonkey/"
    datapath <- paste0(dataFolder, fileID)
    fileName <- fileID
    #read data
    surveyDataRaw1 <- readxl::read_excel(datapath) %>%
      rename(timestamp = `Start Date`) #change name of timestamp col
    
    rawDataPre <- preProcessSurveyMonkey(surveyDataRaw1)
    openQs <- rawDataPre$openQs
    surveyDataRaw <- rawDataPre$surveyDataRaw
    
    
  } else {
    #### READ GOOGLE SHEET ###########
    ## read in google sheets
    ## get info about sheet
    sheetInfo <- drive_get(fileID)
    datapath <- fileID #setting so that this can be used
    fileName <- sheetInfo$name
    
    seconds <- sample(x = 1:5, size = 1, replace=TRUE) ## set a 1-5 second time out to trick google into 
    # not shutting down our access.
    print(glue("First sleep: Sleeping {seconds} seconds to make it difficult for Google to determine we are using a machine."))
    Sys.sleep(seconds)
    
    ## check how many sheets are in the file
    sheetNames <- sheet_names(sheetInfo)
    if (length(sheetNames) > 1) {
      warning(
        "Your survey data sheet has multiple sheets. These are the names of the sheets:\n",
        paste(sheetNames, collapse = " | ")
      )
    }
    seconds <- sample(x = 1:5, size = 1, replace=TRUE) ## set a 1-5 second time out to trick google into 
    # not shutting down our access.
    print(glue("Second sleep: Sleeping {seconds} seconds to make it difficult for Google to determine we are using a machine."))
    Sys.sleep(seconds)
    
    if (surveyMonkey == T) {
      surveyDataRaw1 <- read_sheet(sheetInfo) %>%
        rename(timestamp = `Start Date`)
      
      rawDataPre <- preProcessSurveyMonkey(surveyDataRaw1)
      openQs <- rawDataPre$openQs
      surveyDataRaw <- rawDataPre$surveyDataRaw
      
      
    } else {
      #read survey data from google sheet not using surveyMonkey
      surveyDataRaw <- read_sheet(sheetInfo) %>%
        rename(timestamp = Timestamp)   #change name of timestamp col
      
      
      openQs <- data.frame(matrix(ncol = 2, nrow = 0)) %>%
        setNames(c("qText", "responseType")) %>%
        mutate(qText = as.character(qText),
               responseType = as.character(responseType))
    }
  }
  # End of excel / google conditional
  
  # #read survey data
  # surveyDataRaw <- read_sheet(sheetInfo)
  #find date survey administered
  surveyDate <- surveyDataRaw[[1, 'timestamp']] %>%
    as.Date.POSIXct()
  ## determine current endyear
  if (lubridate::week(surveyDate) < 33) {
    ##week 33 is mid-August
    surveyendyear <- lubridate::year(surveyDate)
  } else {
    surveyendyear <- lubridate::year(surveyDate + 1)
  }
  
  
  
  # 1. create a surveyQlookup table
  qText <- names(surveyDataRaw)
  #add info for joining
  surveyQlookup <- data.frame("qText" = qText) %>%
    rowid_to_column("qNum") %>%
    mutate(
      qName = paste0("q", qNum),
      surveyID = surveyid,
      fileAddress = datapath,
      fileName = fileName,
      endYear = surveyendyear
    ) %>%
    left_join(openQs, by = "qText") # adding in questionType for those that have that information
  #need to add surveyID, demo, scale
  
  ## 2. create long data set
  surveyDataLong <- surveyDataRaw %>%
    mutate(across(everything(), .fns = as.character)) %>% #convert all data to character so that they can be stored in one column
    {
      if (!"respondentID" %in% names(surveyDataRaw))
        rowid_to_column(., "respondentID")
      else
        .
    } %>% #if respondentID hasn't been set, use the row number.Otherwise pass the dataset unchanged
    pivot_longer(cols = all_of(qText[qText != "respondentID"]),
                 names_to = "qText",
                 values_to = "response") %>%  ## note that we are dropping "respondentID" from qText because it is included when pulling from surveymonkey
    mutate(respondentID = as.numeric(respondentID)) %>%
    mutate(
      fileName = fileName,
      fileAddress = datapath,
      endYear = surveyendyear,
      surveyID = surveyid
    )
  
  #3a. Set item type by checking for open-responses
  ## find distinct responses ...but drop timestamp
  if (nrow(openQs) == 0 &
      n_distinct(surveyDataLong$respondentID) < 16) {
    #check to see if you have enough survey respondents
    warning(
      "Not enough respondents to determine if question is open-ended.  You need to manually check ",
      datapath,
      "for open-ended questions and adjust surveyQlookup."
    )
    openResponse <-
      "No open ended questions" ## this would have to match an open ended question text to be counted.
  } else {
    responses <- surveyDataLong %>%
      distinct(response, .keep_all = TRUE) %>%
      filter(!qText %in% c("timestamp"))
    #find out which questions have a lot of different responses...these are almost certainly open ended
    openResponse <- count(responses, qText) %>%
      arrange(n) %>%
      filter(n > 15) %>% ### assumes that selected response items will have fewer than 15 entries
      pull(qText)
  }
  
  #3b. Finalize surveyQlookup: set open-ended response flag in surveyQlookup
  
  surveyQlookup <- surveyQlookup %>%
    mutate(
      responseType = case_when(
        is.na(responseType) & qText %in% openResponse ~ "open",
        is.na(responseType) & qText == "timestamp" ~ "auto",
        is.na(responseType) ~ "selected",
        TRUE ~ responseType
      ),
      demoQ = case_when(str_detect(tolower(qText), pattern = demoQ_text) ~ 1)
    )
  
  ##4. Finalize long data  ### add school number to long format
  
  
  ### add school to responses
  ## find non-school specific surveys
  if (is.na(schoolNumber)) {
    #find the question asking about school attend
    schoolQ <- surveyDataLong %>%
      left_join(select(surveyQlookup,-fileName,-endYear),
                by = c("fileAddress", "qText")) %>% ## add in responseType for each response
      filter(responseType == "selected",
             str_detect(tolower(qText), .schoolQ_text)) %>%
      # str_detect(tolower(response), "canon city e.*|.*exploratory.*|ces|canon city m.*|canon city h.*|harrison|mckinley|lincoln|washington")) %>%
      # distinct(response, qText, .keep_all = TRUE) %>%
      distinct(qText) %>%
      pull(qText)
    #set school for each respondent
    if (length(schoolQ) == 1) {
      #if there is a school question...
      #create a joining table with schoolnumber and respondentID
      joinSchools <- surveyDataLong %>%
        filter(qText == schoolQ,!is.na(response)) %>% #just get the response to the school Q
        distinct(respondentID, response) %>% ## get table with respondentID and selected school
        mutate(response = tolower(response)) %>% 
        # mutate(
        #   cdeSchoolNumber = case_when(
        #     str_detect(tolower(response), "canon city e.*|.*exploratory.*|ces") ~ "7950",
        #     str_detect(tolower(response), "canon city m.*|.*ccms.*") ~ "1262",
        #     str_detect(tolower(response), "canon city h.*|.*cchs.*") ~ "1266",
        #     str_detect(tolower(response), ".*harrison.*") ~ "3802",
        #     str_detect(tolower(response), ".*mckinley.*") ~ "5704",
        #     str_detect(tolower(response), ".*lincoln.*") ~ "5166",
        #     str_detect(tolower(response), ".*washington.*") ~ "9248"
        #   )
        # ) %>%  # create cdeSchoolNumber from responses
      regex_left_join(select(schoolsTable, cdeSchoolNumber, schoolRegex), by = c("response" = "schoolRegex")) %>% 
        select(-response, -schoolRegex)
      ## add school by joining on respondentID
      surveyData <- surveyDataLong %>%
        left_join(joinSchools, by = "respondentID")
      
    } else  if (length(schoolQ) == 0) {
      # no school question identified
      #check to see if this is the highschool survey (one school)
      currSurvey <- filter(surveylookup, link == fileID)
      if (currSurvey$respondents == "student" &
          currSurvey$edLevel == "high") {
        surveyData <- surveyDataLong %>%
          mutate(cdeSchoolNumber = "1266")
        warning(
          "No 'choose your school' questions identified. I think it's a high school survey. CDE school number 1266 (Canon City HS) was added for ",
          datapath,
          ". Double check this data to make sure this is correct."
        )
      } else {
        surveyData <- surveyDataLong %>%
          mutate(cdeSchoolNumber = NA)
        warning(
          "No 'choose your school' questions identified.  CDE school number not added for ",
          datapath,
          ". You'll need to add school number to these data."
        )
      }
      
    } else  if (length(schoolQ) > 1) {
      surveyData <- surveyDataLong %>%
        mutate(cdeSchoolNumber = NA)
      warning(
        "Multiple 'choose your school' questions identified.  CDE school number not added for ",
        datapath,
        ". You may need to modify this function to better determine 'choose your school' questions."
      )
    }
    
  } else {
    surveyData <- surveyDataLong %>%
      mutate(cdeSchoolNumber = schoolNumber)
  }
  
  return(list("surveyData" = surveyData, "surveyQlookup" = surveyQlookup))
  
} 
#test <- readSurveyData(fileID,schoolNum)

 #end expression loading functions
############# LOOP SURVEY read function ###########################
#### 1A -- FOR DATA SAVED TO GOOGLE SHEETS USE THIS .......................

## Set year to extract
endYear_vec <-  2024 # specify if you want to load only one year
### or ##
endYear_vec <-  NULL  # specify NA if you want to load all

surveyProcessInfo <- readRDS("data/surveylookup.rds") %>% 
  filter(type == "survey") %>% 
  {if(length(endYear_vec)>0) filter(.,endYear  %in% endYear_vec) else . } %>% ## get a specific year if thats what we want
  select(fileID = link, schoolNumber = cdeSchoolNumber, surveyMonkey) 
 




# fileIDs <- surveylookup$link
# 
# #### 1B -- FOR THE DATA STORED IN EXCEL SHEETS FROM THE PAST USE THIS .......................
# ## read an entire set of survey data for excel
# dataFolder_set <- "data/canonCity/surveyData/surveymonkey/"
# #get file names
# fileIDs <- list.files(dataFolder_set)


###### 2. AFTER RUNNING 1A or 1B ...........................
###### LOOP across file IDs
surveyProcessed <- pmap(surveyProcessInfo, readSurveyData)


#extract the table and bind rows
## map()-ing across all elements of the list to pull surveyData out...then bind_rows()
surveyResponses_to_add <- map(.x=1:length(surveyProcessed), .f = ~surveyProcessed[[.x]]$surveyData) %>% 
  bind_rows()
## use map() extract the surveyQlookup table
surveyQlookup_to_add <- map(.x = 1:length(surveyProcessed), .f = ~surveyProcessed[[.x]]$surveyQlookup) %>% 
  bind_rows()

#look over the surveyResponses and the surveyQlookup tables
View(surveyResponses_to_add)
View(surveyQlookup_to_add)

############# insert read SURVEY data into main data files #################


###### 3.  SAVE file after running 2. ....................
## get the surveyResposes and surveyQlookup data that already exists
surveyResponses_archived <- readRDS("data/surveyResponses.rds")
surveyQlookup_archived <- readRDS("data/surveyQlookup.rds") 

## Get all the surveyIDs of the surveys you are going to add to the file
filterTable <- surveyResponses_to_add %>% 
  distinct(surveyID)
#remove remove these responses from surveyResponses if they have already been loaded and  
## then add in the version we just processed
surveyResponses <- surveyResponses_archived %>% 
  anti_join(filterTable) %>% 
  bind_rows(surveyResponses_to_add)

## remove questions from surveyQlookup if they were loaded previously and then add in 
## the version we just processed
surveyQlookup <- surveyQlookup_archived %>% 
  anti_join(filterTable) %>% 
  bind_rows(surveyQlookup_to_add)

saveRDS(surveyResponses, "data/surveyResponses.rds")
saveRDS(surveyQlookup, "data/surveyQlookup.rds")

surveyResponses <- readRDS("data/surveyResponses.rds")
surveyQlookup <- readRDS("data/surveyQlookup.rds")

####################################################################.
############ add DEMOS/SurveyOptions for surveyResponses  ##########
####################################################################.
#### create options table
warning("Skip past option table if options have already been set")

questionOptions <- surveyResponses %>% 
  left_join(surveyQlookup) %>% 
  filter(responseType == "selected") %>% 
  distinct(qText, qName, response) %>% 
  filter(!is.na(response)) %>% 
  add_count(qText, qName) %>% 
  arrange(qText, qName, n)

# write.csv(questionOptions, "data/optionsQlookup_raw.csv")

distinctOptions <- questionOptions %>% 
  distinct(response)
## read in Old options table and add on any new options
optionsOldwithNew <- read.csv("data/optionsQlookup.csv") %>% 
  full_join(distinctOptions)



write.csv(optionsOldwithNew, "data/optionsQlookup.csv", row.names = F)
#### !!!!!!!!  MANUAL step when questions have a new set of options !!!!!!!! ##
#### manually set optionOrder of options in spreadsheet if they are new
#### Opening up the "data/optionsQlookup.csv" sheet in google or excel is probably the easiest.

## Open the data you just saved to review any options that may have been added. 
## You will need to manually set optionOrder (see Notes in csv) and scale (see notes in CSV) for each new item.
utils::browseURL("data/optionsQlookup.csv")

## This table assumes that scale items 1 & 2 (e.g. Strongly Disagree, Disagree) 
## have a negative valance., 3 (undecided or don't know) has a neutral valance
## and 4 & 5 (agree, strongly agree) have a positive valance. This will be used to 
## compute "Percent Favorable" responses
optionValenceFill <- tibble::tribble(
  ~optionOrder, ~optionValence, ~scale,
  1L,             0L,     1L,
  2L,             0L,     1L,
  3L,             NA,     1L,
  4L,             1L,     1L,
  5L,             1L,     1L
)


optionsQlookup <- read.csv("data/optionsQlookup.csv") %>% 
  select(-optionValence, -notes.on.option.order) %>% 
  left_join(optionValenceFill) %>% 
  right_join(questionOptions, by = "response") %>% 
  select(-n)

View(optionsQlookup)


checked <- menu(c("I haven't checked","I have checked"),  title = "Have you checked the optionsQlookup file?")

#Resume here after confirming you checked the file

if(checked == 2){
  saveRDS(optionsQlookup, "data/optionsQlookup.rds")
  print("optionsQlookup.rds has been saved")
  } else stop("optionsQlook.rds not saved!!!  You need to select `I have checked` to save the file")




#### set up Demos for joining - start here #############
## this is from the app...all of the survey responses
### create dataset with joined tables for plotting
surveyResponses_filter <- surveylookup %>% 
  ungroup() %>% 
  select(-cdeSchoolNumber, -schoolName, -schoolNameShort) %>% #dropping school from surveylookup because some surveys cover multiple schools...join school info to the cdeschoolnumber in survey responses 
  filter(type == "survey") %>% 
  inner_join(surveyResponses) %>% #, by = c("link"= "fileAddress",  "endYear")
  left_join(select(schoolsTable, -edLevel), by = c("cdeSchoolNumber")) %>% ## add school names to each response
  inner_join(surveyQlookup) %>% #, by = c("link" = "fileAddress", "qText", "endYear", "fileAddress")
  left_join(optionsQlookup, by = c("response", "qText", "qName"))



surveyResponses_filter <- surveylookup %>% 
  ungroup() %>% 
  select(-cdeSchoolNumber, -schoolName, -schoolNameShort) %>% #dropping school from surveylookup because some surveys cover multiple schools...join school info to the cdeschoolnumber in survey responses 
  filter(type == "survey") %>% 
  inner_join(surveyResponses) %>% #, by = c("link"= "fileAddress",  "endYear")
  left_join(select(schoolsTable, -edLevel), by = c("cdeSchoolNumber")) %>% ## add school names to each response
  inner_join(surveyQlookup) %>% #, by = c("link" = "fileAddress", "qText", "endYear", "fileAddress")
  left_join(optionsQlookup, by = c("response", "qText", "qName"))


# test <- surveyResponses_filter %>% 
#   filter(edLevel == "middle", is.na(cdeSchoolNumber)) %>% 
#   count(endYear, cdeSchoolNumber)

 ## Create and save demo's lookup table ----------------------- 
##demos table with labels for controls... save this file for use in the app
demosTable <- surveyResponses_filter %>% 
  filter(demoQ == 1) %>% 
  distinct( qText, response) %>% 
  arrange( qText) %>% 
  mutate(displayResponse = case_when(
    response == "A Girl" ~ "Female",
    response == "A Boy" ~ "Male",
    response == "Support Staff member" ~ "Support Staff Member", 
    TRUE ~ response
  ),
  responseOrder= case_when(
    displayResponse == "Female" ~ 1,
    displayResponse == "Male" ~ 2,
    displayResponse == "I prefer not to answer this question" ~ 3,
    TRUE ~ as.numeric(str_extract(response, "\\d+"))),
  displayName = case_when(
    qText == "I am" ~ "Gender",
    qText == "I am a..." ~ "Role",
    str_detect(tolower(qText), "grade")~ "Grade",
    str_detect(tolower(qText), "pathway") ~ "Pathway"
  )
  ) %>% 
  arrange(qText, responseOrder, displayResponse)

saveRDS(demosTable, "data/demosTable.rds")
demosTable <- readRDS("data/demosTable.rds")


#### End Surveys --------------------------------------

##############################################################################.
##############################################################################.
#     RATING SHEETS  #########################################################
##############################################################################.
##############################################################################.
## There are two functions that process the rating forms. The first function is 
# used to loop through the individual sheets of each rating form.  The second
# function loops through each schools' rating form and to create the final data
# structure.  Again, this data structure is in long format to maximize flexibility
# in group_bys and plotting. 

# Process rating sheet data -----------------------------------------------


#RATING sheets
### Rating form data handling
## get list of surveys and rating forms NOTE:  New rating sheets will need to be 
## added in the same way they were for the surveys (see above)
surveylookup <- readRDS("data/surveylookup.rds") ## we filter this for ratings below


#read rating template data ##########
#### Rating form data handling... google connection failing so we are using csv and excel

## extract data from one sheet


#' Title extractRatingSheet
#'NOTE: format of rating sheets must conform to existing template: 
#'1. 2 rows at top are dropped
#'2. Third row contains headers
#'3. -First Name- column must be populated or the row will be dropped
#'4. Must have columns named "First Name", "Last Name" and "School"
#'5. Summary Rows and Summary Columns and Summary worksheet will all be dropped.
#'6. Title of the file should have the endyear that it was conducted.
#'7. Google will kick you out if you are reloading all of the rating sheets. If 
#'   loading more than a couple of sheets, set the seconds variable (below), to
#'   60 seconds.
#'
#' @param datapath - string of googlesheet ID or url (could easily make this the excel spreadsheet file path to be loaded). 
#' @param sheet_selected - string. the excel worksheet to load
#' @param SchoolNumberFromLookup -- string -- the school number specified in surveylookup table
#'
#' @return - a table of the ratings on a worksheet in long format
#' @export
#'
#' @examples  extractRatingSheet(datapath = "data/canonCity/ratingData/2022_Harrison_ratings.xlsx", sheet_selected= "Climate")
#' 
#' datapath <- "https://docs.google.com/spreadsheets/d/1UXfsN_n7QO8z0vhiYwnNrfI1_uh57aFQyMKOmd1uK8U/edit#gid=1136102995"
#' sheet_selected <- "Improvement of Instruction"
extractRatingSheet <- function(datapath, sheet_selected= sheetsList[[1]], schoolNumberFromLookup = schoolNumber) {

  
  # datapath = ratingLoopTable$dataPath[1]
  # sheet_selected = "Postive Student Behavior"
  # schoolNumberFromLookup = ratingLoopTable$schoolNumber[1]
  # 
  
  ratingData <- range_read(datapath, skip = 2, sheet = sheet_selected)
  print(glue::glue("Extracting sheet for school number: {schoolNumberFromLookup}"))
  
  seconds <- sample(x = 10:50, size = 1, replace=TRUE) ## set a 10-80 second time out to trick google into 
  # not shutting down our access.
  print(glue("Sleeping {seconds} seconds to make it difficult for Google to determine we are using a machine and to avoid overrunning quotas."))
  Sys.sleep(seconds)
  print("Done with sleeping.")
  ## create named vector of columns -- names contain the actual question text
  colNames <- names(ratingData)
  # tempNames <- paste0(sheet_selected, 1:length(colNames))
  # names(tempNames) <- colNames
  
  
  #Check to make sure that sheets have identification column names.
  idCols <- c("First Name", "Last Name", "School")
  matchedCols <- colNames[colNames %in% idCols]
  ##send message to fix column name if no match
  if(length(idCols) != length(matchedCols)){
    noMatch <- setdiff(idCols, matchedCols)
    unMatchedCols <- colNames[!colNames %in% intersect(idCols, matchedCols) ]
    warning(paste0("Your column name for '", noMatch, "' on the sheet named '", sheet_selected, "' does not match any of your column names. You should change the name of the column that has data for '", noMatch, "' to '",noMatch,"'\n","Spreadsheet: ", datapath,"\n\n******  These are the unmatched column names  ******* \n", paste(unMatchedCols, collapse= " |\n") ))
  }
  # fill in school name if missing
  
  # schoolCol <- tempNames[names(tempNames) == "School"]
  schoolName <- ratingData %>% # get the name of the school
    distinct(School) %>% 
    na.omit() %>% 
    mutate(School = str_trim(School)) %>% 
    pull(School)
  print(glue::glue("School Name from sheet: {schoolName}"))

  ## check to see if the school name is unique
  if(length(schoolName) !=1) {
    schoolName <- schoolName[1] #if there's more than one, take the first one and send a warning message
    warning("You should have only one school in the vector, but there are more than one.  Check to make sure the correct school was added.\nSchool Name that was added is '", schoolName,"'.\nSheetName: ", sheet_selected, "\nDataPath and Google drive info: \n", datapath )
  }
  ## clean out unneccessary rows
 ratingData2 <-  ratingData %>% 
    filter(!is.na(`First Name`),#drop anything without the first column (firstName) populated
           str_detect(tolower(`First Name`), "mean|mode|deviation", negate = TRUE)) %>% ## drop rows with summary statistics
    mutate(School = case_when(
             is.na(School) | str_detect(School, "School")~ schoolName, #replacing missing or incorrect values
             TRUE ~ School),
           construct = sheet_selected) %>% 

 mutate(across(.cols = where(is.list), .fns = ~ replace(.[[1]], is.null(.), NA_real_) ),
        across(.cols = where(is.list), .fns = ~unlist(.) )) %>% 
## Some missing values are read from Google as NULL rather than NA - which causes the column to be read in as a list rather than a vector-- here converting NULLS to NAs and then unlist()ing them-- Note: Replacing the NULL with NA_real_ causes the column to unlist itself...I'm checking for lists and unlist()ing as a precaution.
    relocate(construct, 1)
  ## pivot longer...
  longCols <- colNames[!colNames %in% c(idCols, "construct")]
  ratingDataLong <- ratingData2 %>% 
    mutate(across(all_of(longCols), .fns = as.integer)) %>% 
    pivot_longer(cols = all_of(longCols), names_to = "ratingTopic", values_to = "rating") %>% 
    mutate(cdeSchoolNumber = schoolNumberFromLookup)
  
  
}


# test <- extractRatingSheet(datapath  = ratingLoopTable$dataPath[[9]], sheet_selected= "Climate")


### nest sheet extraction function in a function to extract all sheets from a file

#' Title getDataFromAllSheets
#'
#' @param dataPath string -- the google URL or sheetID
#' @param endyear numeric -- the endyear
#' @param dimensions character vector -- a vector of all the dimension names (sheet names) that will have data (this is so we can drop summary worksheets)
#' @param final boolean  -- 1 = this is a final rating, 0 = this is an initial rating
#' @param schoolNumber --
#' @param surveyID -- surveyId from surveylookup
#'
#' @return -- data from all rating sheets from a spreadsheet rating template
#' @export
#'
#' @examples getDataFromAllSheets(dataPath = "data/canonCity/ratingData/", dataFile = "2022_Harrison_ratings.xlsx", endyear = 2022)
getDataFromAllSheets <-
  function(dataPath = ratingLoopTable$dataPath[1],
           endyear = ratingLoopTable$endyear[1],
           dimensions = ratingDimensions,
           final = ratingLoopTable$final[1],
           schoolNumber = ratingLoopTable$schoolNumber[1],
           surveyID = ratingLoopTable$surveyID[1]) {
    # dataPath = ratingLoopTable$dataPath[1]
    # endyear = ratingLoopTable$endyear[1]
    # dimensions = ratingDimensions
    # final = ratingLoopTable$final[1]
    # schoolNumber = ratingLoopTable$schoolNumber[1]
    # surveyID = ratingLoopTable$surveyID[1]
    
    # dataPath = "https://docs.google.com/spreadsheets/d/1UXfsN_n7QO8z0vhiYwnNrfI1_uh57aFQyMKOmd1uK8U/edit#gid=1136102995"
    # endyear = 2023
    # dimensions = ratingDimensions
    # final = 1
    # schoolNumber = "3802"
    # surveyID = 125
    
    ## id the sheet names in the spreadsheet/workbook
    #read sheet names
    sheetInfo <- drive_get(dataPath)
    sheetName <- sheetInfo$name[1]
    sheetsList_all <- sheet_names(sheetInfo)
    ## drop summary and appendix sheets
    sheetsList <-
      sheetsList_all[tolower(sheetsList_all) %in% tolower(dimensions)]
    print(paste("Processing sheet name : ", sheetName))
    
    ## clean up sheet names and catch odd occurrences
    if (length(sheetsList) == length(dimensions)) {
      print("Rating dimensions match expected dimensions")
    } else {
      warning(
        paste(
          "Expected number of rating sheets not found.\nRating sheets expected: \n",
          paste(dimensions, collapse = " | "),
          "\nRating sheets found: \n",
          paste(sheetsList, collapse = " | "),
          "\nDropped sheets: ",
          paste(sheetsList_all[!tolower(sheetsList_all) %in% tolower(dimensions)], collapse = " | ")
        )
      )
    }
    
    
    ## extract each sheet of the file and combine into a long table using extractRatingSheet()
    map(sheetsList, extractRatingSheet, datapath = sheetInfo, schoolNumberFromLookup = schoolNumber) %>%
      bind_rows() %>%
      mutate(
        endYear = endyear,
        finalizedRating = final,
        cdeSchoolNumber = schoolNumber,
        surveyID = surveyID
      )
    
  }


# test <- getDataFromAllSheets(dataPath = ratingLoopTable$dataPath[2],
#                              endyear = ratingLoopTable$endyear[2],
#                              dimensions = ratingDimensions,
#                              final = ratingLoopTable$final[2],
#                              schoolNumber = ratingLoopTable$schoolNumber[2],
#                              surveyID = ratingLoopTable$surveyID[2]) 

### creating vector of sheet names (rating dimensions)  to look for (an argument in the function)
## I've included these warnings to call attention to manual checking that should be going on 
# when loading the data. 
warning("Check this list of rating dimensions/constructs to make sure it's the same for the current data.")

ratingDimensions <- c("Climate","Health","Innovative Instruction", "Equity of Opportunity","Learning Experiences","Assessment Practices", "Improvement of Instruction", "Postive Student Behavior", "Resource Acquisition" )
#Throwing warning to check to make sure rating dimensions are current. 
print(ratingDimensions)
warning("You need to check to make sure that the rating dimension above have not changed this year. ", paste(ratingDimensions, collapse = " | "))

#create a table with function parameters to loop through to run data extraction the rating sheets
## setting a binary -- Should the code below rerun (and overwrite) all rating data (TRUE) or just 
## the newly loaded data (FALSE)
reloadAllRatings <- TRUE
reloadAllRatings <- FALSE

# this is setting the endyear to the current endyear
if(lubridate::week(Sys.Date())<33){ ##week 33 is mid-August
  surveyendyear <- lubridate::year(Sys.Date())} else {surveyendyear <- lubridate::year(Sys.Date())+1}

ratingendyear <- surveyendyear
## or -- if running a back year...specify as below
# ratingendyear <- 2023:2024
# ratingendyear <- 2022
# ratingendyear <- 2023
# ratingendyear <- 2024

if (reloadAllRatings == TRUE){
  ratingLoopTable <- surveylookup %>% 
    filter(type == "rating", final == 1) %>% 
    select(dataPath = link, endyear = endYear, final, schoolNumber = cdeSchoolNumber, surveyID)
  warning("You will be loading all rating sheets listed in surveylookup, and will OVERWRITE the ratings Table in the conditional before saving.")
} else if(reloadAllRatings == FALSE){

  ## reset this filter if not reloading all ratings
  ratingLoopTable <- surveylookup %>% 
      filter(type == "rating", final == 1, endYear %in% ratingendyear) %>%
      select(dataPath = link, endyear = endYear, final, schoolNumber = cdeSchoolNumber, surveyID)
  View(ratingLoopTable)
  warning("You need to stop and check ratingLoopTable to make sure you are loading the correct subset of the rating forms.")
}


# 

# test <- getDataFromAllSheets(dataPath = ratingLoopTable$dataPath[[1]], endyear = ratingLoopTable$endyear[[1]], dimensions = ratingDimensions, final = ratingLoopTable$final[[1]], schoolNumber = ratingLoopTable$schoolNumber[[1]])

###############################################.
####### LOOP through extract process ##########
################################################

## loop through all the rating datafiles
ratingResponses_to_add <- pmap(ratingLoopTable, getDataFromAllSheets) %>%  #getDataFromAllSheets() uses the extractRatingSheet() function insede to get each sheet
  bind_rows() %>% 
  mutate(ratingTopic = str_trim(ratingTopic, side = "both")) # clear off trailing/leading whitespace
## create table of rating "questions"

ratingResponses_archived <- readRDS("data/ratingResponses.rds") %>% 
  mutate(ratingTopic = str_trim(ratingTopic, side = "both")) 

#find all rating sheets that have been previously saved that are also in the new data
filterTable_rating <- ratingResponses_to_add %>% 
  distinct(cdeSchoolNumber, endYear, finalizedRating, surveyID)

# ratingDimensions <- c("Climate","Health","Innovative Instruction", "Equity of Opportunity","Learning Experiences","Assessment Practices", "Improvement of Instruction", "Postive Student Behavior", "Resource Acquisition" )
## get school table to replace School with the schoolNameShort field
schoolsTableJoiner <- schoolsTable %>% 
  select(cdeSchoolNumber, schoolNameShort)

#Conditional to handle final formatting and saving of ratingResponses. 
if(reloadAllRatings == FALSE){ ## make sure you don't create duplicate ratings and finalize
  ratingResponses <- ratingResponses_archived %>% 
    anti_join(filterTable_rating) %>%
    bind_rows(ratingResponses_to_add) %>%
    mutate(
      construct = factor(construct, levels = ratingDimensions)
    ) %>% 
    left_join(schoolsTableJoiner) %>% 
    select(-School) %>% 
    rename(School = schoolNameShort)
} else if(reloadAllRatings == TRUE){ # !!!! overwrite entire set of ratings !!!!
  ratingResponses <- ratingResponses_to_add %>% 
    mutate(
      construct = factor(construct, levels = ratingDimensions)
    ) %>% 
    left_join(schoolsTableJoiner) %>% 
    select(-School) %>% 
    rename(School = schoolNameShort)
}

## Take a look at the data file you created.  Check to see that year(s) you added 
## look correct. 
View(ratingResponses)

### save ratings
{ 
saveRatings <- menu(choices = c("I'm ready to save!", "Don't save."), 
                    title = "Save the updated ratingResponses file?\nYou can type `0` or ESC to exit.")
 ## return here after responding to prompt.
if(saveRatings == 1){
  saveRDS(ratingResponses, "data/ratingResponses.rds")
  print("Rating responses saved as data/ratingResponses.rds")
} else if(saveRatings == 2) print("Updated ratingResponses NOT saved.")
}

stop("You have completed survey and rating data processing.")


# # End Data Processing... --------------------------------------------------
# 
# 
# ##  code below is for exploring the data  -- no need to run if you are just cleaning 
# ## data for the app.
# 
# # Summarizing functions from Shiny app ------------------------------------
# ##code that was created in the process of creating the application.
# source("app.R", local = TRUE)
# source("utilities.R",local = T)
# #rating summary for the entire district across all constructs
# test <- process_ratingData(level = "subconstruct", schoolNum = "3802", selectedconstruct="Assessment Practices")
# test <- process_ratingData(level = "construct", schoolNum = "3802", selectedconstruct=NA)
# test <- process_ratingData(level = "overall", schoolNum = "3802", selectedconstruct=NA)
# 
# 
# 
# 
# ### survey summaries-- 
# 
# ### create dataset with joined tables for plotting
# surveyResponses_filter <- surveylookup %>% 
#   ungroup() %>% 
#   select(-cdeSchoolNumber, -schoolName, -schoolNameShort) %>% #dropping school from surveylookup because some surveys cover multiple schools...join school info to the cdeschoolnumber in survey responses 
#   filter(type == "survey") %>% 
#   
#   inner_join(surveyResponses) %>% #, by = c("link"= "fileAddress",  "endYear")
#   left_join(select(schoolsTable, -edLevel), by = c("cdeSchoolNumber")) %>% ## add school names to each response
#   inner_join(surveyQlookup) %>% #, by = c("link" = "fileAddress", "qText", "endYear", "fileAddress")
#   left_join(optionsQlookup, by = c("response", "qText", "qName"))
# 
# surveyRespondents <- "parent" # any of these three c("parent","student", "educator")
# surveyRespondents <- "student"# any of these three c("parent","student", "educator")
# 
# ##set grouping variables
# xAxis <- 'endYear' ## can use endYear or schoolNameShort __ this is often flipped in ggplot so it turns into the y-axis
# 
# grouper <- c(xAxis, "respondents")
# # or 
# grouper <- c(xAxis, "edLevel")
# 
# ## for schools on the x axis
# xAxis <- 'schoolNameShort' 
# 
# grouper <- c("endYear", "respondents", xAxis)
# 
# ## for questions on the x axis
# xAxis <- 'qName'
# tagalongs <- c("qNum",  "qText")
# grouper <- c("endYear", 'schoolNameShort', "edLevel",  xAxis, tagalongs)
# 
# 
# # this will run the combinations above
# test <-  process_surveyData(data = surveyResponses_filter, .grouper = grouper, .surveyRespondents = surveyRespondents)
# 
# 
# 



#' 
#' 
#' ##### Summarizing rating data #####
#' # ratingResponses <- readRDS("data/ratingResponses.rds")%>% 
#' #   mutate(School = case_when(  # fixing a mispelling
#' #     School == "Washingotn" ~ "Washington", 
#' #     TRUE ~ School
#' #   ))
#' 
#' constructs <- ratingResponses %>% 
#'   distinct(endYear, construct, ratingTopic)
#' #### there are columns that had numbers but not heading...all start with ...  Filter these.
#' 
#' schools <- ratingResponses %>% 
#'   distinct(School)
#' 
#' schoolsTable <-  tibble::tribble(
#'   ~schoolName, ~cdeSchoolNumber, ~schoolNameShort,     ~edLevel,
#'   "Canon City High School",            1266L,           "CCHS",       "high",
#'   "Canon City Middle School",            1262L,           "CCMS",     "middle",
#'   "Canon Exploratory School",            7950L,            "CES", "elementary",
#'   "Harrison School",            3802L,       "Harrison",        "K-8",
#'   "Lincoln School of Science and Technology",            5166L,        "Lincoln", "elementary",
#'   "McKinley Elementary School",            5704L,       "McKinley", "elementary",
#'   "Washington Elementary School",            9248L,     "Washington", "elementary"
#' ) %>% 
#'   mutate(cdeSchoolNumber = as.character(cdeSchoolNumber)) %>% 
#'   arrange(schoolName)
#' 
#' ratingResponses <- readRDS("data/ratingResponses.rds") %>% 
#'   mutate(School = case_when(  # fixing a mispelling
#'     School == "Washingotn" ~ "Washington", 
#'     TRUE ~ School
#'   )) %>% 
#'   filter(str_detect(ratingTopic, "^\\.\\.\\.", negate = TRUE), #dropping all ratingTopics that were not labeled -- they all start with "..."
#'          finalizedRating == 1) %>% 
#'   left_join(schoolsTable, by = c("School" = "schoolNameShort"))
#'   
#' school <- "3802"
#' schoolgrouper <- c("cdeSchoolNumber", "schoolName", "School")
#' constructs <- c("construct")
#' constructs <- c("construct", "ratingTopic", "topicShort")
#' 
#' 
#' 
#' 
#' # # Examples of data handling and plotting for ratings. 
#' # 
#' # 
#' # 
#' # 
#' # overallRatingDist <-ratings %>% 
#' #   mutate(topicShort = str_sub(ratingTopic, start = 1, end = 25)) %>% 
#' #   group_by(across(all_of(c("endYear")))) %>% 
#' #   summarize(n = sum(!is.na(rating)),
#' #             total = sum(rating, na.rm = TRUE),
#' #             mean = total/n, 
#' #             School = "District") 
#' # overallRating <- ratings %>% 
#' #   mutate(topicShort = str_sub(ratingTopic, start = 1, end = 25)) %>% 
#' #   group_by(across(all_of(c("endYear", schoolgrouper)))) %>% 
#' #   summarize(n = sum(!is.na(rating)),
#' #             total = sum(rating, na.rm = TRUE),
#' #             mean = total/n) %>% 
#' #   bind_rows(overallRatingDist)
#' # 
#' # ggplot(overallRating, aes(x = mean, y = School, fill=factor(ifelse(School=="District","District reference","School")))) +
#' #   geom_col(position = position_dodge2(width =.9, preserve = "single") )+
#' #   coord_flip()+
#' #   facet_grid(cols = vars(endYear)) +
#' #   scale_fill_manual(name = "School", values = c(  "grey50", "steelblue"), guide = guide_legend(reverse = TRUE))+
#' #   coord_cartesian(xlim = c(1,4)) +
#' #   theme_minimal()
#' # 
#' # 
#' # ####### PLOT school-level data -- constructs
#' # 
#' # school <- "3802"
#' # schoolgrouper <- c("cdeSchoolNumber", "schoolName", "School")
#' # baseGroup <- c("endYear")
#' # constructs <- c("construct")
#' # # constructs <- c("construct", "ratingTopic", "topicShort")
#' # 
#' # grouper <- c(schoolgrouper, baseGroup, constructs)
#' # 
#' # m1 <- ratings %>% 
#' #   mutate(topicShort = str_sub(ratingTopic, start = 1, end = 25)) %>% 
#' #   group_by(across(all_of(grouper))) %>% 
#' #   summarize(n = sum(!is.na(rating)),
#' #             total = sum(rating, na.rm = TRUE),
#' #             mean = total/n) %>% 
#' #   filter(cdeSchoolNumber == school)
#' # 
#' # schoolNameShort <- m1$School[[1]]
#' # 
#' # m <- ratings %>% 
#' #   mutate(topicShort = str_sub(ratingTopic, start = 1, end = 25)) %>% 
#' #   group_by(across(all_of(grouper[!grouper %in% schoolgrouper]))) %>% 
#' #   summarize(n = sum(!is.na(rating)),
#' #             total = sum(rating, na.rm = TRUE),
#' #             mean = total/n,
#' #             School = "District") %>% 
#' #   bind_rows(m1) %>% 
#' #   mutate(School = factor(School, levels = c("District",schoolNameShort)))
#' # 
#' # 
#' # 
#' # 
#' # ## set colors to school and district
#' # colorVect <- c("grey50", "steelblue")
#' # names(colorVect) <- c("District", "Harrison")
#' # 
#' # ggplot(m, aes(x = mean, y = construct, fill = School))+
#' #   geom_col(position = position_dodge2(width =.9, preserve = "single") )+
#' #   
#' #   coord_flip()+
#' #   facet_grid(cols = vars(endYear)) +
#' #   scale_fill_manual(values = colorVect, guide = guide_legend(reverse = TRUE))+
#' #   coord_cartesian(xlim = c(1,4)) +
#' #   theme_minimal()
#' # 
#' #   
#' # 
#' # ########## PLOT rating sub-construct ..........................
#' # 
#' # school <- "3802"
#' # schoolgrouper <- c("cdeSchoolNumber", "schoolName", "School")
#' # baseGroup <- c("endYear")
#' # #constructs <- c("construct")
#' #  constructs <- c("construct", "ratingTopic", "topicShort")
#' # 
#' # grouper <- c(schoolgrouper, baseGroup, constructs)
#' # 
#' # m1 <- ratings %>% 
#' #   mutate(topicShort = str_sub(ratingTopic, start = 1, end = 25)) %>% 
#' #   group_by(across(all_of(grouper))) %>% 
#' #   summarize(n = sum(!is.na(rating)),
#' #             total = sum(rating, na.rm = TRUE),
#' #             mean = total/n) %>% 
#' #   filter(cdeSchoolNumber == school)
#' # 
#' # schoolNameShort <- m1$School[[1]]
#' # selectedconstruct <- m1$construct[[1]]
#' # 
#' # m <- ratings %>% 
#' #   mutate(topicShort = str_sub(ratingTopic, start = 1, end = 25)) %>% 
#' #   group_by(across(all_of(grouper[!grouper %in% schoolgrouper]))) %>% 
#' #   summarize(n = sum(!is.na(rating)),
#' #             total = sum(rating, na.rm = TRUE),
#' #             mean = total/n,
#' #             School = "District") %>% 
#' #   bind_rows(m1) %>% 
#' #   mutate(School = factor(School, levels = c("District",schoolNameShort)))
#' # 
#' # m <- filter(m, construct == selectedconstruct)
#' 
#' # Examples of data handing for ratings ------------------------------------
#' 
#' 
#' #### data handling function for ratings #####################
#' ########## PLOT rating sub-construct ..........................
#' ## These are the draft versions that are now in the app.
#' grouper <- c("endYear", constructs)
#' 
#' 
#' m <- ratingResponses %>% 
#'   filter(str_detect(tolower(`First Name`), "review team")) %>% ## get the agreed upon subconstruct ratings
#'   group_by(endYear, construct, School) %>% 
#'   summarize(sumRating =sum(rating, na.rm=TRUE),
#'             possiblePoints = n()*4,
#'             percent = sumRating/possiblePoints)
#' 
#' 
#' #### SEE APP for most recent version of this function ------------------
#' #' Title process_ratingData
#' #'
#' #' @param level string - one of c("subconstruct", "construct", "overall" )
#' #' @param schoolNum  string -- cde school number
#' #' @param selectedconstruct -sting -- name of selected construct  -- leave blank except for when level = "subconstruct"
#' #'
#' #' @return  list -- plotting data, name of school and name of construct
#' #' @export
#' #'
#' #' @examples process_ratingData(level = "construct", schoolNum = "3802", selectedconstruct="Assessment Practices")
#' process_ratingData <- function(level = "construct",  schoolNum = "0000", selectedconstruct=NA) {
#'   if(level == "subconstruct" & is.na(selectedconstruct) ) {stop("You must specify a construct when summarizing subconstructs.")}
#'   ## testing vars
#'   # schoolNum <- "3802"
#'   # selectedconstruct <- "Assessment Practices"
#'   
#'   schoolgrouper <- c("cdeSchoolNumber", "schoolName", "School")
#'   baseGroup <- c("endYear")
#'   
#'   subconstructRatings <- ratingResponses %>% 
#'     filter(str_detect(tolower(`First Name`), "review team"))
#'   
#'   
#'   ## rollup to school level -- constructs weighted evenly (not rolling from subconstruct)
#'   ## rollup to district -- average school construct ratings rather than district construct ratings -- reduces impact of outlier schools
#' #' Title
#' #'
#' #' @param grouper 
#' #'
#' #' @return summary table of rolled up school-level construct summaries
#' #' @export
#' #'
#' #' @examples  schoolOverall <- overallRatings(grouper = c(schoolgrouper, baseGroup))
#' #'  districtRatings <- overallRatings(grouper=c(baseGroup, "construct"))
#' #'  districtOverall <- overallRatings(grouper = baseGroup)
#'   overallRatings <- function(grouper){
#'     schoolRatings %>% ## averaging percents to weight constructs evenly
#'       group_by(across(all_of(grouper))) %>% 
#'       summarise(sumPercent = sum(percent, na.rm = TRUE),
#'                 n = n(),
#'                 percent = sumPercent/n)
#'   }
#'   
#'   ## create plotting data
#'   
#' if(level == "subconstruct" ){ ## we want subconstruct scores for a construct at a school
#'   m1 <- subconstructRatings %>% 
#'     filter(cdeSchoolNumber == schoolNum, 
#'            construct == selectedconstruct) 
#'   
#'   m2 <- subconstructRatings %>% ### creating district reference data
#'     filter(construct == selectedconstruct) %>% 
#'     group_by(endYear, construct, ratingTopic) %>% 
#'     summarize(sumRating = sum(rating, na.rm = TRUE),
#'               totalPossible = sum(!is.na(rating)), 
#'               rating = sumRating/totalPossible) %>% 
#'     mutate(cdeSchoolNumber = "0000",
#'            School = "District") %>% 
#'     select(-sumRating, -totalPossible)
#'   
#'   m1 <- select(m1, all_of(names(m2)))
#'   # names(m1) = "endYear"         "construct"       "ratingTopic"     "rating"          "cdeSchoolNumber" "School"
#' 
#'     
#' } else {
#'   #set grouper for school and for district --
#'   ### Rollup subconstruct scores to construct. weighting as in the spreadsheet 
#'   #--subconstructs weighted evenly to construct rollup.
#'   schoolRatings <-     subconstructRatings %>% 
#'     group_by(across(all_of(c(schoolgrouper, baseGroup, "construct")))) %>% 
#'     summarize(sumRating =sum(rating, na.rm=TRUE),
#'               possiblePoints = n()*4, #each entry has the possibility of 4 points
#'               percent = sumRating/possiblePoints) %>% 
#'     mutate(rating = round(percent*4, 1))
#'    
#'   ##????? check George's method for computing school average -- apply to district
#'   if(level == "construct" ){ #if we want constructs for a school
#'     m1 <- schoolRatings %>%
#'       ungroup() %>% 
#'       filter(cdeSchoolNumber == schoolNum) %>% 
#'       select(-sumRating, -possiblePoints, -schoolName) 
#'     ## create district comparison -- district ratings by construct
#'     m2 <- overallRatings(grouper=c(baseGroup, "construct")) %>% 
#'       mutate(cdeSchoolNumber = "0000",
#'              School = "District",
#'              rating = round(percent*4, 1))  %>% 
#'       select(-sumPercent, -n)
#'     # names(m1) =  "cdeSchoolNumber" "School"    "rating"      "endYear"         "construct"       "percent"  
#'     
#'   } else if(level == "overall" ){ #if we want and overall rating
#'     m1 <- overallRatings(grouper = c(schoolgrouper, baseGroup)) %>% 
#'       ungroup() %>% 
#'       select(-schoolName) 
#'   
#'     m2 <- overallRatings(grouper = baseGroup) %>% 
#'       mutate(cdeSchoolNumber = "0000",
#'              School = "District",
#'              rating = round(percent*4, 1)) 
#'     # names(m1) = "cdeSchoolNumber" "School"   "rating"       "endYear"         "sumPercent"      "n"               "percent" 
#'   }
#' }
#'   m <- bind_rows(m1, m2) %>% 
#'     {if(schoolNum != "0000") mutate(.,School = factor(School, levels = c("District",sort(unique(m1$School))))) else .}
#'  
#'   if(schoolNum == "0000"){
#'     schoolNameShort <- "District"
#'   } else {
#'     schoolNameShort <- m1$School[[1]]
#'     # selectedconstruct <- m1$construct[[1]]
#'   }
#'  
#'   if(!is.na(selectedconstruct)){
#'     m <- m %>% 
#'       mutate(topicShort = str_sub(ratingTopic, start = 1, end = 30)) 
#'   }
#'   return(list("m"=m, "schoolNameShort"=schoolNameShort, "selectedconstruct" = selectedconstruct))
#'   
#' }
#' process_ratingData(level = "subconstruct", schoolNum = "3802", selectedconstruct=NA)
#' 
#' 
#' ### end data handling functions for ratings ----------------------------------
#' 
#' 
#' 
#' 
#' # More data handling/plotting that is now in the app.  -----------------------------
#' 
#' 
#' 
#' ## set colors to school and district
#' colorVect <- c("grey50", "steelblue")
#' names(colorVect) <- c("District", schoolNameShort)
#' 
#' ggplot(m, aes(x = mean, y = topicShort, fill = School))+
#'   geom_col(position = position_dodge2(width =.9, preserve = "single") )+
#'   
#'   coord_flip()+
#'   facet_grid(cols = vars(endYear)) +
#'   scale_fill_manual(values = colorVect, guide = guide_legend(reverse = TRUE))+
#'   coord_cartesian(xlim = c(1,4)) +
#'   theme_minimal()
#' 
#'                
#' ########### SCRATCH #############   TABLE processing
#' 
#' ##cut for function....
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' #### FIX surveyQlookup ##############
#' 
#' 
#' # 
#' # questionOptions <- surveyResponses %>% 
#' #   left_join(surveyQlookup) %>% 
#' #   filter(responseType == "selected") %>% 
#' #   distinct(qText, qName, response) %>% 
#' #   filter(!is.na(response)) %>% 
#' #   add_count(qText, qName) %>% 
#' #   arrange(qText, qName, n)
#' #   
#' # write.csv(questionOptions, "data/optionsQlookup_raw.csv")
#' # 
#' # distinctOptions <- questionOptions %>% 
#' #   distinct(response)
#' # 
#' # write.csv(distinctOptions, "data/optionsQlookup_raw.csv")
#' # #### mess around with options in spreadsheet
#' # ### save as optionsQlookup.csv
#' # optionValenceFill <- tibble::tribble(
#' #                        ~optionOrder, ~optionValence, ~scale,
#' #                                  1L,             0L,     1L,
#' #                                  2L,             0L,     1L,
#' #                                  3L,             NA,     1L,
#' #                                  4L,             1L,     1L,
#' #                                  5L,             1L,     1L
#' #                        )
#' # 
#' # 
#' # optionsQlookup <- read.csv("data/optionsQlookup.csv") %>% 
#' #   select(-optionValence, -X, -notes.on.option.order) %>% 
#' #   left_join(optionValenceFill) %>% 
#' #   right_join(questionOptions, by = "response") %>% 
#' #   select(-n)
#' # 
#' # saveRDS(optionsQlookup, "data/optionsQlookup.rds")
#' 
#' 
#' 
#' # SURVEY plotting  --------------------------------------------------------
#' 
#' 
#' #read data for survey responses
#' optionsQlookup <- readRDS("data/optionsQlookup.rds")
#' surveyResponses <- readRDS("data/surveyResponses.rds")
#' surveyQlookup <- readRDS("data/surveyQlookup.rds")
#' surveylookup <- readRDS("data/surveylookup.rds")
#' 
#' surveyRespondents <- "student"
#' surveyRespondents <- "educator"
#' surveyRespondents <- "parent"
#' 
#' 
#' surveyRespondents_all <- c("educator", "parent", "student")
#' 
#' ### create dataset with joined tables for plotting
#' surveyResponses_filter <- surveylookup %>% 
#'   ungroup() %>% 
#'   select(-cdeSchoolNumber, -schoolName, -schoolNameShort) %>% #dropping school from surveylookup because some surveys cover multiple schools...join school info to the cdeschoolnumber in survey responses 
#'   filter(type == "survey") %>% 
#'   inner_join(surveyResponses) %>% #, by = c("link"= "fileAddress",  "endYear")
#'   left_join(select(schoolsTable, -edLevel), by = c("cdeSchoolNumber")) %>% ## add school names to each response
#'   inner_join(surveyQlookup) %>% #, by = c("link" = "fileAddress", "qText", "endYear", "fileAddress")
#'   left_join(optionsQlookup, by = c("response", "qText", "qName"))
#' 
#' 
#' 
#' #longitudinal plotting
#' 
#' 
#' ### Example of processing and plotting surveys with Demos ------------------
#' ##for a particular survey
#' # choose a survey -- need to get the distinct group of surveys by filtering on respondents, type and edlevel
#' responder <- "student" ## c( "educator", "parent" ,  "student")  NOTE: If you select parent, there will be no demos and this draft plotting code will 
#' surveyType <- "survey"
#' ed_level <- "middle"
#' ed_level <- NA
#' 
#' chosenSurvey <- surveylookup %>% 
#'   filter(respondents == responder, type == surveyType) %>% 
#'   {if(!is.na(ed_level)) filter(.,  edLevel == ed_level) else .} %>% 
#'   pull(surveyID)
#' 
#' 
#' # create a file of demos for each respondent to this survey
#' demos <- surveyResponses_filter %>% 
#'   filter(surveyID %in% chosenSurvey) %>% 
#'   filter(demoQ == 1) %>% 
#'   select(surveyID, respondentID, qText, response) %>% 
#'   left_join(demosTable) %>% 
#'   pivot_wider(id_cols = c(surveyID, respondentID), names_from = displayName, values_from = displayResponse) 
#' 
#' #add on survey demos to each person's responses
#' surveyResp_1 <- surveyResponses_filter %>% 
#'   filter(surveyID %in% chosenSurvey, 
#'          is.na(demoQ)) %>% #drop demo q rows since we are adding t hem as columns
#'   left_join(demos)
#' 
#' # Identify the demo cols of this dataset
#' demoCols <- names(demos)[names(demos) %in% unique(demosTable$displayName)]
#' ### use these demoCols to create disaggregation radiobuttons to add to the grouping
#' 
#' #longitudinal plotting-------------------
#' 
#' #' Title process_surveyData
#' #'
#' #' @param data  survey Dataframe -- 
#' #' @param .grouper character vector of column names to group by
#' #' @param .surveyRespondents string one of c("student", "educator", "parent")
#' #'
#' #' @return a summary dataframe of survey data
#' #' @export
#' #'
#' #' @examples   m1 <-  process_surveyData(data = surveyResponses_filter, .grouper = grouper, .surveyRespondents = surveyRespondents)
#' process_surveyData <- function(data, .grouper, .surveyRespondents) {
#'   data %>% 
#'     filter(respondents %in% .surveyRespondents, responseType =="selected", scale == 1) %>% 
#'     group_by(across(all_of(.grouper))) %>% 
#'     summarize(total = sum(!is.na(response)),
#'               nResponses = sum(optionValence, na.rm = TRUE),
#'               percentPos = nResponses/total,
#'               n = n_distinct(respondentID)) 
#' }
#' 
#' 
#' 
#' demo <- demoCols[1] ## this won't work with surveys that don't have any demo questions e.g., Parent survey
#' demo <- NA
#' xAxis <- 'endYear'
#' schools <- c("schoolName","cdeSchoolNumber")
#' grouper <- c(xAxis, demo, schools)
#' grouper <- grouper[!is.na(grouper)]
#' 
#' 
#' m <- surveyResp_1 %>% 
#'   process_surveyData(.grouper = grouper, .surveyRespondents = responder) %>% 
#'   mutate(EndYear= as.character(endYear),
#'          tool_tip = paste0("Percent Favorable: ", round(percentPos*100), "%\nn = ", n,"\nYear: ", endYear)) %>% 
#'   {if(!is.na(demo)) filter(., !is.na(.data[[demo]])) else .} %>% 
#'   {if("cdeSchoolNumber" %in% grouper)filter(., !is.na(cdeSchoolNumber)) else .}
#' 
#' p <-  ggplot(m, aes(x= EndYear, y = percentPos))+
#'   geom_line(aes(group = schoolName))+
#'   geom_point_interactive(size = 2, aes(tooltip = tool_tip) )+
#'   
#'   labs(x = "End Year",
#'        y = "Percent Favorable")+
#'   scale_y_continuous(labels = scales::label_percent(), limits = c(.30,1))+
#'   theme_minimal()+
#'   theme(strip.text.y = element_text(color = "grey20", size = 10, hjust = 0, margin = margin(t = 0, r = 0, b = 0, l = 2, unit = "cm"))) #,axis.text.y = element_text(color = "grey20", size = 12)
#' 
#' p <-  ggplot(m, aes(x= EndYear, y = percentPos, color = .data[[demo]] , fill = .data[[demo]]))+
#'   geom_line(aes(group = .data[[demo]]))+
#'   geom_point_interactive(size = 2, aes(tooltip = tool_tip) )+
#'   
#'   labs(x = "End Year",
#'        y = "Percent Favorable")+
#'   scale_y_continuous(labels = scales::label_percent(), limits = c(.30,1))+
#'   theme_minimal()+
#'   theme(strip.text.y = element_text(color = "grey20", size = 10, hjust = 0, margin = margin(t = 0, r = 0, b = 0, l = 2, unit = "cm"))) #,axis.text.y = element_text(color = "grey20", size = 12)
#' 
#' if("cdeSchoolNumber" %in% grouper){
#'   p <- p + facet_wrap(facets = vars(schoolName),
#'                       strip.position = "top",
#'                       ncol = 1)
#' }
#' 
#' p <- girafe_formatting(p, select = "single", width = 12, height = 10)
#' p
#' 
#' 
#' 
#' ###Survey plotting overall
#' xAxis <- 'endYear'
#' grouper <- c(xAxis, "
#'              ") # 'edLevel'
#' 
#' 
#' m1 <- surveyResponses_filter %>% 
#'   filter(respondents %in% surveyRespondents, responseType =="selected", scale == 1) %>% 
#'   group_by(across(all_of(grouper))) %>% 
#'   summarize(total = sum(!is.na(response)),
#'             n = sum(optionValence, na.rm = TRUE),
#'             percentPos = n/total) 
#' 
#' m <- m1
#' 
#' ggplot(m, aes(y = percentPos, x = factor(.data[[xAxis]]), fill = factor(endYear))) +
#'   geom_col(position = "dodge") +
#'   facet_wrap(facets = vars(respondents),
#'              ncol = 3,
#'              scales = "free_y",
#'              strip.position = "top",)+
#'   # theme(strip.text.x = element_text(angle = 0, hjust = 0))+
#'   scale_y_continuous(labels = scales::label_percent())
#' 
#' 
#' ## survey plotting school
#' xAxis <- 'schoolNameShort'
#' 
#' grouper <- c("endYear", "respondents", xAxis)
#' 
#' 
#' m1 <- surveyResponses_filter %>% 
#'   filter(respondents %in% surveyRespondents, responseType =="selected", scale == 1) %>% 
#'   group_by(across(all_of(grouper))) %>% 
#'   summarize(total = sum(!is.na(response)),
#'             n = sum(optionValence, na.rm = TRUE),
#'             percentPos = n/total) 
#' 
#' m <- m1
#' 
#' ggplot(m, aes(y = percentPos, x = .data[[xAxis]], fill = factor(endYear))) +
#'   geom_col(position = "dodge") +
#'   facet_wrap(facets = vars(respondents),
#'              ncol = 1,
#'              scales = "free_y",
#'              strip.position = "top",)+
#'   # theme(strip.text.x = element_text(angle = 0, hjust = 0))+
#'   scale_y_continuous(labels = scales::label_percent())+
#'   coord_flip()
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' #plotting questionlevel for school
#' xAxis <- 'qName'
#' tagalongs <- c("qNum",  "qText")
#' grouper <- c("endYear", 'schoolNameShort', xAxis, tagalongs)
#' schoolName_short <- "CCHS"
#' surveyRespondents <- "student"
#' 
#' m1 <- surveyResponses_filter %>% 
#'   filter(respondents %in% surveyRespondents, responseType =="selected", scale == 1) %>% 
#'   group_by(across(all_of(grouper))) %>% 
#'   summarize(total = sum(!is.na(response)),
#'             n = sum(optionValence, na.rm = TRUE),
#'             percentPos = n/total) 
#' ## filter for one school and wrap question text
#' m2 <- m1 %>% 
#'   filter(schoolNameShort==schoolName_short) %>%
#'   mutate(qText = str_wrap(qText, width = 80))
#' 
#' ##set factor levels for questions so they are in order
#' factorLevels <- m2 %>% 
#'   ungroup() %>% 
#' distinct( qNum, qName, qText) %>% 
#'   arrange(qNum)
#' 
#' m <- m2 %>% 
#'   mutate(qName = factor(qName, levels = unique(factorLevels$qName)),
#'          qText = factor(qText, levels = unique(factorLevels$qText))) 
#'   
#' ggplot(m, aes(x = .data[[xAxis]], y = percentPos, fill = factor(endYear))) +
#'   geom_col(position = "dodge") +
#'   facet_wrap(facets = vars(qText),
#'              ncol = 1, 
#'              scales = "free_y",
#'              strip.position = "top",)+
#'   theme(strip.text.x = element_text(angle = 0, hjust = 0))+
#'   scale_y_continuous(labels = scales::label_percent()) +
#'   coord_flip()
#'   
#' 
#' ## by responscols = ## by response
#' 
#' grouper <- c("endYear", 'schoolNameShort',"qNum", "qName", "qText", "response")
#' endyear <- 2020
#' selectedschool <- "CCHS"
#' 
#' m1 <- surveyResponses_filter %>% 
#'   filter(respondents %in% surveyRespondents, responseType =="selected", scale == 1) %>% 
#'   group_by(across(all_of(grouper[grouper != "response"]))) %>% 
#'   mutate(total = sum(!is.na(response)) )%>% 
#'   group_by(across(all_of(grouper))) %>% 
#'   summarize(n = sum(!is.na(response)),
#'             total = first(total),
#'             optionOrder = first(optionOrder),
#' 
#'             percentResponse = n/total) %>% 
#'   filter(schoolNameShort == selectedschool) %>% 
#'   arrange(endYear, qName, optionOrder)
#' 
#' factorLevels <- m1 %>% 
#'   ungroup() %>% 
#'   filter(endYear ==endyear) %>% 
#'   distinct( qNum, qName, qText, response) %>% 
#'   arrange(qNum)
#' 
#' factorLevelsResp <- m1 %>% 
#'   ungroup() %>% 
#'   filter(endYear == endyear) %>% 
#'   distinct(response, optionOrder) %>% 
#'   arrange(optionOrder) %>% 
#'   mutate(optionColor = case_when(
#'     optionOrder == 1 ~ "goldenrod3",
#'     optionOrder == 2 ~ "goldenrod1",
#'     optionOrder == 3 ~ "azure2",
#'     optionOrder == 4 ~ "dodgerblue",
#'     optionOrder == 5 ~ "dodgerblue4"
#'     ),
#'     optionCategory = case_when(
#'       optionOrder == 1 ~ "Most Negative",
#'       optionOrder == 2 ~ "Moderately Negative",
#'       optionOrder == 3 ~ "Neutral",
#'       optionOrder == 4 ~ "Moderatly Positive",
#'       optionOrder == 5 ~ "Most Positive"
#'     )
#'   )
#' 
#' 
#' responseTable <- m1 %>% 
#'   mutate(cellDisplay = paste0(response, "\n", round(percentResponse*100), "%\nn = ", n)) %>% 
#'   pivot_wider(id_cols = c(endYear, qNum, qText, qName, schoolNameShort), names_from = optionOrder, values_from = cellDisplay) %>% 
#'   select(endYear, schoolNameShort, qNum, qName, qText, as.character(1:5))
#' 
#' 
#' 
#' m <- m1 %>% 
#'   filter(endYear == endyear) %>% 
#'   left_join(select(factorLevelsResp, response, optionCategory)) %>% 
#'   mutate(qName = factor(qName, levels = unique(factorLevels$qName)),
#'          qText = factor(qText, levels = unique(factorLevels$qText))
#'          ,
#'          response = factor(response, levels = unique(factorLevelsResp$response)),
#'          optionCategory = factor(optionCategory, levels = unique(factorLevelsResp$optionCategory)),
#'          tooltip = paste0(qText, "\nResponse: ", response, "\nPercent: ", round(percentResponse*100), "%\nn = ", n)
#'          ) 
#' 
#' # colorVect <- factorLevelsResp$optionColor %>%
#' #   setNames(factorLevelsResp$response)
#' colorVect <- unique(factorLevelsResp$optionColor) %>% 
#'   setNames(unique(factorLevelsResp$optionCategory))
#' 
#' # q10 and q16 are ordering the bars in reverse order of the factor
#' # m <- filter(m, qName %in% c("q10", "q16", "q6"))
#'  
#' 
#' p <- ggplot(filter(m, endYear == endyear), aes(y=qName, x = percentResponse, tooltip = tooltip, fill = forcats::fct_rev(optionCategory))) +
#'   geom_col_interactive(position = "stack")+ #position_fill(reverse = T)
#'   facet_wrap(facets = vars(qText),
#'              ncol = 1, 
#'              scales = "free_y",
#'              strip.position = "top",)+
#'   theme_minimal()+
#'   theme(strip.text.x = element_text(angle = 0, hjust = 0))+
#'   scale_fill_manual(values = colorVect,  guide = guide_legend(reverse=T))+
#'   scale_x_continuous(labels = scales::label_percent())
#' 
#' girafe(code = print(p))
#' 
#' 
#' 
#' ######## OLD CODE --- delete when finalizing --------------------------
#' # 
#' # 
#' # ### question variables
#' # dataCols <- names(surveyRaw)
#' # notQuestions <- 1:9 #vector with the indices of non-q cols
#' # questions <- dataCols[-notQuestions]
#' # 
#' # 
#' # #set surveyID - surveyMonkey has this in "collector ID" and the first row has questionType (so get the 2nd row)
#' # if(str_detect(tolower(dataFolder), ".*surveymonkey*")){
#' #   surveyID <- surveyRaw$`Collector ID`[2]
#' # } else if (str_detect(tolower(dataFolder), ".*google*")){
#' #   surveyID <- surveyName
#' # }
#' # 
#' # #extract question type if available 
#' # 
#' # if(str_detect(tolower(dataFolder), ".*surveymonkey*")){
#' #   questionType <- surveyRaw[1, ] %>% 
#' #     select(all_of(questions)) %>% 
#' #     pivot_longer(cols = all_of(questions), names_to = "question", values_to = "qtype") %>% 
#' #     mutate(surveyID = as.character(surveyID))
#' # } else if (str_detect(tolower(dataFolder), ".*google*")){
#' #   questionType <-  as.data.frame(questions) %>% 
#' #     setNames("question") %>% 
#' #     mutate(qtype = NA)
#' # }
#' # 
#' # surveyQlookup <- questionType %>% 
#' #   rowid_to_column("qOrder") %>% ### do this in a groupby to get to produce numbers from one
#' #   mutate(surveyID = as.character(surveyID),
#' #          reverseCode = NA,
#' #          qid= "figure out surveyId and then create qid",
#' #          surveyName = surveyName,
#' #          endYear = endyear,
#' #          school = school)
#' # 
#' # #Create table with questions from survey, question info and survey info
#' # 
#' # 
#' # questionType <- surveyRaw[1, ] %>% 
#' #   select(all_of(questions)) %>% 
#' #   pivot_longer(cols = questions[-1], names_to = "question", values_to = "qtype") %>% 
#' #   mutate(surveyName = surveyName,
#' #          endYear = endyear,
#' #          school = school) 
#' # 
#' # 
#' # 
#' 
#' 
