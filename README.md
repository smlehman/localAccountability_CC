## Canon City Schools accountability code
### Local accountability data processing and reporting

The primary scripts that are used to extract, process, summarize and display data are surveyDataHandling.R, utilities.R and app.R.

The surveyDataHandling script handles most of the extraction and processing of the summary and rating data.  There are three general blocks of code that will need to be run every time there is new data. 
 -- Create surveylookup table
 -- Extract survey data
 -- Extract rating data

Interactive menus have been added as well as more extensive error detection to better guide the script user through the process. 
 #### Create surveylookup table
 This is the most manual part of the entire process.  You will need to create a table that contains the initial values defining what type of data each link to data contains.  
 You will use a spreadsheet template to do this. (available at https://docs.google.com/spreadsheets/d/1C0Q6tCJxbOisMxdaoUSGi3AiTSjrKw9ZR88V_kMFG4s/edit#gid=0) 

It is very important that the values entered here are correct as they drive much of the data extraction process. The valid values of the fields are specified in the script, and there is also error handling code that helps to identify whether fields have valid entries (e.g., for edLevel 'high' is acceptable but "high school" is not. 

#### Extract survey responses
There are several things that one should attend to when extracting surveys.  
  * You should check the regex (regular expressions) that identify demographic questions and the school question.  It's important that the regex string identifying these will catch the demo and school questions.
  * You should check the surveyResponses file to ensure that schools are being correctly identified -- The names for schools vary across surveys, so we have to use regex (again) to identify which school has been selected.  There shouldn't be NA in the cdeSchoolNumber column unless the response column is an NA. You can easily find NA in a column by sorting it. The NA's fall to the bottom of the sorted data. 

 If you have new options for selected response items that haven't been used before, you will need to add these in the optionsQlookup.  The code will identify all options that occur in the data and then will open a csv for you to identify whether an item is a scale (as in a likert scale) item (1 = T and 0 = F).
 For scale items you must identify option order (use the notes in the csv for guidance).  Some items that are not scale, like grade, will have an order as well. 

 Extracting surveys typically happens pretty fast.  Most of the time is in reading from Google, but we don't have to stop the process too long between reads as googe is more tolerant of reading an entire spreadsheet. 

#### Extract rating responses
In contrast the survey extraction, ratings extraction is super slow - expect it to take between 45 minutes to 2 hours per year you process.* 
Important things to attend to: 
  * Check to make sure that the construct/dimension tab names exactly match the text that the warning message lists.  If they don't, the extraction process will error out (not good when it takes so long). It might be smart to lock the format of the rating form.  This year (2024) someone corrected the spelling of `Postive Student Behavior` to `Positive Student Behavior` and it caused the entire process to error out. If this happens the function will tell you what sheet names it found, what sheet names it is looking for, and what sheet names don't match.

  *  You do have the option to reload all ratings from prior year, but know that this will take a very long time and google sometimes will kick you out saying you've overrun your quota.
  *  Take a look at the ratingResponses table (the script will open it for you) before you save. 


*TLDR: Google gets very angry when we read individual sheets from files using a machine, so we have to trick it into thinking we are not a machine by making the script sleep between our access.  The sleep time here is substantial.  Without randomly changing our sleep times we have to wait over 60 seconds between each read.  By randomizing sleep times, we can get ~30 per read, but that still means it takes about an hour to run code that would take R less than a second to run under normal conditions. 

