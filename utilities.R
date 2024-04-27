
 
#### utilities file for analytics app ##############


###############-----libraries and data ---------
## app.R ## 
# devtools::install_github("paulc91/shinyauthr")
#devtools::install_github("DivadNojnarg/shinydashboardPlus")
#install.packages(c("ggrepel" ,"ggthemes","RColorBrewer","plotly" ,"shinyjs" ,"shiny" ,"shinydashboard" ,"shinydashboardPlus"  ,"tidyverse" ,"DT"   ,"shinycssloaders","shinyWidgets"  ,"stringr","lazyeval" ,"ggiraph","randomNames","htmlwidgets" ,"lubridate" ), repos='http://cran.rstudio.com/')  #code for installing multiple packages 

###---add shinyjqui for showing/hiding, 
# # library(shinyjqui) ##  !!!!!conflict with ggiraph... interactive graphs show double tooltip boxes
# packages <- c('ggrepel', 'ggthemes', 'RColorBrewer', 'shinyjs', 'shiny', 'shinydashboard', 'shinydashboardPlus', 'DT', 'shinycssloaders', 'shinyWidgets', 'lazyeval', 'ggiraph', 'randomNames', 'htmlwidgets', 'lubridate', 'fst', 'emojifont', 'tidyverse', 'fuzzyjoin')



library(shinyjs) ## usse to add javascript functionality (show/hide, change inputs, reset,etc.)https://deanattali.com/shinyjs/
library(shiny) 
library(shinydashboard) 
library(bslib)
library(thematic)
library(data.table) ##super fast data handling
# library(dtplyr)
library(tidyverse) 
library(lubridate) 
# library(DT)   #library(RColorBrewer)
library(reactable)
library(shinyWidgets)  # widget formatting  https://dreamrs.github.io/shinyWidgets/  --see examples run shinyWidgetsGallery()
library(ggiraph)
library(glue) 
library(fst)

# library (patchwork)
# library (sodium) ## needed for hashing and reading passwords

# load.fontawesome()

### find directories --for debugging shinyproxy and kubernetes
# print("List of files in working directory")
# print(list.files(recursive = TRUE))

schoolsTable <-  tibble::tribble(
                                 ~schoolName, ~cdeSchoolNumber, ~schoolNameShort,     ~edLevel,     ~schoolRegex,
                    "Canon City High School",            1266L,           "CCHS",       "high", ".*canon city h.*|.*cchs.*",
                  "Canon City Middle School",            1262L,           "CCMS",     "middle", ".*canon city m.*|.*ccms.*",
                  "Canon Exploratory School",            7950L,            "CES", "elementary", "canon city e.*|.*exploratory.*|ces", 
                           "Harrison School",            3802L,       "Harrison",        "K-8", ".*harrison.*|harr",
  "Lincoln School of Science and Technology",            5166L,        "Lincoln", "elementary", ".*lincoln.*|.*lsst.*",
                "McKinley Elementary School",            5704L,       "McKinley", "elementary", ".*mckinley.*|.*mcki.*",
              "Washington Elementary School",            9248L,     "Washington", "elementary", ".*washington.*|.*wash.*",
  "Mount View Core Knowledge Charter School",            6752L,     "Mount View",       "K-8",  ".*mount view.*|.*mountain view.*|>*core.*|.*mvck.*"
  ) %>% 
  mutate(cdeSchoolNumber = as.character(cdeSchoolNumber)) %>% 
  arrange(schoolName)

surveyResponses <- readRDS("data/surveyResponses.rds") %>% ungroup()
surveyQlookup <- readRDS("data/surveyQlookup.rds")%>% ungroup()
optionsQlookup <- readRDS("data/optionsQlookup.rds")%>% ungroup()
surveylookup <- readRDS("data/surveylookup.rds")%>% ungroup()
demosTable <- readRDS("data/demosTable.rds") %>%  ungroup()

surveyYears <- surveylookup %>% 
  select(endYear) %>% 
  arrange(desc(endYear)) %>% 
  distinct() %>% 
  pull(endYear)

#find current EndYear
if(lubridate::week(Sys.Date())<33){ ##week 33 is mid-August
  surveyendyear <- lubridate::year(Sys.Date())} else {surveyendyear <- lubridate::year(Sys.Date())+1}


ratingResponses <- readRDS("data/ratingResponses.rds") %>% 
  mutate(School = case_when(  # fixing a mispelling
    School == "Washingotn" ~ "Washington", 
    TRUE ~ School
  )) %>% 
  filter(str_detect(ratingTopic, "^\\.\\.\\.", negate = TRUE), #dropping all ratingTopics that were not labeled -- they all start with "..."
         finalizedRating == 1) %>% 
  left_join(schoolsTable)


#  custom colors ---------------------------------------------------------

customColors <- c("#4e79a7", "#f28e2b", "#76b7b2","#e15759",  "#59a14f", "#edc948", "#b07aa1", "#ff9da7", "#9c755f", "#bab0ac")
  

school_disagg_list <<- c("allSchools", "Area", "edLevel", "SchoolCategory",  "Title1Flag", "AlternativeEducationCampus")#all is the field that specifies active schools--it is added in the data processing function surveyGroupedData(). All schools in list are "active"  , "SPF"
names(school_disagg_list) <- c("None", "Articulation Area","Ed Level", "School Type", "Title I", "Alt Ed Centers") #, "SPF"

schoollist <<- schoolsTable %>% 
  select(cdeSchoolNumber, schoolName) %>% 
  add_row(cdeSchoolNumber="0000", schoolName="District") %>% 
  arrange(schoolName)



#########------------------create variables for global environment -----------------
#create named list for the select box. -- the value passed by the select box will be the the CDE school code.

#school_selectList <- schoollist$VeryAbbreviatedName
school_selectList <<-  unique(schoollist$cdeSchoolNumber)  #using the school number rather than the school name
names(school_selectList) <-  unique(schoollist$schoolName) # don't use distinct because school_list is vector, not a dataframe


# girafe formatting function ----------------------------------------------


####Function: girafe_formatting:  apply typcial girafe settings to interactive plots with css formatting. 
#Arguments:
#1. p = plot
#2. width = width of plot
#3. height = height of plot
#4. selected_css = css for selected elements
#5. hover_css = css for hover interactivity


girafe_formatting <- function(p = p, width = 8, height = 8, selected_css = "stroke:black;r:6pt;", hover_css = "stroke:black;cursor:pointer;", select = "multiple") { #fill:#999966
  
  tooltip_css <- "background-color:gray;color:white;font-family:Sans-Serif;padding:10px;border-radius:5px 5px 5px 5px;" #border-radius:10px 20px 10px 20px;

  
  p <- girafe(code = print(p), width_svg = width, height_svg = height)#
  
  p <-girafe_options(p,
                     opts_selection(type = select, css = selected_css, only_shiny = TRUE), #width_svg = 12, height_svg = 8,
                     opts_hover_inv(css = "opacity:0.95;"),
                     opts_hover(css = hover_css),
                     opts_tooltip(opacity = .90, css = tooltip_css, zindex = 9999),# setting zindex to 9999 so that modals do not float above the tooltips.
                     opts_sizing(rescale = TRUE, width = 1),
                     opts_toolbar(position = "topright", saveaspng = TRUE),
                     opts_zoom(min=1, max = 4)
                     #stroke:black;
  ) # fill:#FF3333;
}

# colorSetter function ----------------------------------------------------

#creating some color vectors that can be used outside of the function
#call in one of the color vectors -- passed in as a parameter  
colors5prof <- c("#C55859", "#ff7f0e", "#E7CD6A", "#2ca02c", "#1f77b4" ) 
colors4prof <- c("#C55859", "#ff7f0e",  "#2ca02c", "#1f77b4" )
colors3prof <- c( "#ff7f0e",  "#2ca02c", "#1f77b4" )
colors3prof_neg <- c( "#C55859", "#ff7f0e", "#1f77b4" )
#tableau - blue, orange, red, seafoam, green, yellow, purple, pink, brown, gray
colorsTableau <- c("#4e79a7", "#f28e2b", "#76b7b2","#e15759",  "#59a14f", "#edc948", "#b07aa1", "#ff9da7", "#9c755f", "#bab0ac")
# names(colorsTableau) <- c("blue", "orange", "seafoam","red",  "green", "yellow", "purple", "pink", "brown", "gray")
#orange, green
colors2 <- c("#ff7f0e",  "#2ca02c")
colors2wneutral <- c("#ff7f0e", "#c2c2c2", "#2ca02c") #oragnge, grey, green
######colorSetter function to lock colors of to particular variable values
#returns - ggplot expression specifying fill or color
#columnName - the column name that needs a color or fill
#colorVector - one of the color vectors colors5prof,colors4prof, colorsTableau, colors2  -- defaults to colorsTableau
#aesthetic - an aesthetic to color -- usually color or fill

colorSetter <- function( columnName, colorVector = "colorsTableau", aesthetic = "color"){
  ##  CMAS: red, orange, yellow, green, blue#set colors in order of proficiency--contains both cmas and SAT levels
  ##  CMAS: red, orange, yellow, green, blue
  print(colorVector)
  print("is vector?")
  print(is.vector(colorVector))
  
  #call in one of the color vectors -- passed in as a parameter  ---now defined outside of function
  # colors5prof <- c("#C55859", "#ff7f0e", "#E7CD6A", "#2ca02c", "#1f77b4" ) 
  # colors4prof <- c("#C55859", "#ff7f0e",  "#2ca02c", "#1f77b4" )
  # #tableau - blue, orange, green, red, purple
  # colorsTableau <- c("#1F77B4", "#FF7F0E", "#2CA02C", "#D62728", "#D62728")
  # #orange, green
  # colors2 <- c("#ff7f0e",  "#2ca02c")
  
  #find number of levels that need a color assigned
  if(is.factor(columnName)){ 
    levelsNbr <- length(levels(columnName))
  } else {
    levelsNbr <- length(unique(columnName))  
    
  }
  print("number of levels")
  print(levelsNbr)
  #take colors parameter, use it's value to call the the vector of the same name and assign the vector to color
  
  #### !!!!!!!!!!!!!!! currently not working !!!!!!!! only works with default tableauColors
  if(length(colorVector)<2){ #if colorvector is just a name...
    colorPal <- eval(as.name(colorVector)) 
    #colorPal <- eval(colorVector)
    print(eval(colorVector))
    print(colorVector)
  } else {
    colorPal <- colorVector
    
  }
  
  #call the vector and shorten the vector to match the length of the unique values in the plotted field
  dotColors <- colorPal[1:levelsNbr] 
  print("dotColors")
  print(dotColors)
  
  #assign names (values from the plotted field) to the color vector -- if a factor, use levels -- otherwise just get unique values
  if(is.factor(columnName)){ 
    names(dotColors) <- levels(columnName)
  } else {
    names(dotColors) <- unique(columnName)
  }
  
  print("names of dotColors")
  print(names(dotColors))
  ###determine if this is for setting color or fill and create set accordingly
  if(aesthetic=="color"){
    colorScale <- scale_color_manual(values = dotColors,   guide = guide_legend(reverse = FALSE))#  reversing order of factor guide = guide_legend(reverse = TRUE),name = paste("Projected",m$stateTest[1]  ,"Proficiency"),
  } else if(aesthetic =="fill"){
    colorScale <- scale_fill_manual(values = dotColors,   guide = guide_legend(reverse = FALSE))#  reversing order of factor guide = guide_legend(reverse = TRUE),name = paste("Projected",m$stateTest[1]  ,"Proficiency"),
  }
  
  
  return(colorScale)
} 


# PLOT UI FUNCTION --------------------------------------------------------
plotUI_layout <- function(plot, ui) {
  div(
  style = "position: relative; height: auto;",
  plot, #e.g., pathCompleterUIplot(ns("completerStatus")),
  div(
    style = "position: absolute; left: 0.5em; bottom: 0.5em;",
    dropdown(
      # inputId = "dropID",
      ui, # e.g., pathCompleterUI(ns("completerStatus")),
      size = "xs",
      icon = icon("gear", class = "opt"),
      up = TRUE
    )
  ) #end div absolute
)#end div
}

# FLUID DESIGN FUNCTION ---------------------------------------------------

fluid_design <- function(id, w, x, y, z) {
  fluidRow(
    div(
      id = id,
      column(
        width = 6,
        uiOutput(w),
        uiOutput(y)
      ),
      column(
        width = 6,
        uiOutput(x),
        uiOutput(z)
      )
    )
  )
}





