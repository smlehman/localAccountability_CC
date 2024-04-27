## Canon City Schools accountability code
### Local accountability data processing and reporting

The primary scripts that are used to extract, process, summarize and display data are surveyDataHandling.R, utilities.R and app.R.

The surveyDataHandling script handles most of the extraction and processing of the summary and rating data.  There are three general blocks of code that will need to be run every time there is new data. 
 -- Create surveylookup table
 -- Extract survey data
 -- Extract rating data

 #### Create surveylookup table
 This is the most manual part of the entire process.  You will need to create a table that contains the initial values defining what type of data each link to data contains.  
 You will use a spreadsheet template to do this (available at 
