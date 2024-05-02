# library(shiny)#other library calls in utilities.r
library(tippy)  
library(viridis)
#library(fuzzyjoin)   
# library(glue)      

# %%%%%%%%%%%%%%%%%%%%%            
# SOURCING ###########    
# %%%%%%%%%%%%%%%%%%%%%       
source("utilities.R", local = TRUE)#other source/module files calls in utilities.r  

#function to put in utilities.......

 
#' Title process_ratingData
#'
#' @param level string - one of c("subconstruct", "construct", "overall" )
#' @param schoolNum  string -- cde school number
#' @param selectedconstruct -sting -- name of selected construct  -- leave blank except for when level = "subconstruct"
#' @param years - integer -- vector of years that will be displayed e.g., 2023:2021
#'
#' @return  list -- plotting data, name of school and name of construct
#' @export
#'
#' @examples test <- process_ratingData(level = "subconstruct", schoolNum = "3802", selectedconstruct="Assessment Practices")
#' test <- process_ratingData(level = "construct", schoolNum = "3802", selectedconstruct=NA)
#' test <- process_ratingData(level = "overall", schoolNum = "3802", selectedconstruct=NA)
process_ratingData <- function(level = "construct",  schoolNum = "0000", selectedconstruct=NA, years) {
  if(level == "subconstruct" & is.na(selectedconstruct) ) {stop("You must specify a construct when summarizing subconstructs.")}
  ## testing vars
  # schoolNum <- "3802"
  # selectedconstruct <- "Assessment Practices"
  
  schoolgrouper <- c("cdeSchoolNumber", "schoolName", "School")
  baseGroup <- c("endYear")
  
  subconstructRatings <- ratingResponses %>% 
    filter(endYear %in% years) %>% 
    filter(str_detect(tolower(`First Name`), "review team")) ## extracting only the negotiated rating for each subconstruct
  
  ## NESTED FUNCTION
  ## rollup to school level -- constructs weighted evenly (not rolling from subconstruct)
  ## rollup to district -- average school construct ratings rather than district construct ratings -- reduces impact of outlier schools
  #' Title
  #'
  #' @param grouper 
  #'
  #' @return summary table of rolled up school-level construct summaries
  #' @export
  #'
  #' @examples  schoolOverall <- overallRatings(grouper = c(schoolgrouper, baseGroup))
  #'  districtRatings <- overallRatings(grouper=c(baseGroup, "construct"))
  #'  districtOverall <- overallRatings(grouper = baseGroup)
  overallRatings <- function(grouper){
    schoolRatings %>% ## averaging percents to weight constructs evenly
      group_by(across(all_of(grouper))) %>% 
      summarise(sumPercent = sum(percent, na.rm = TRUE),
                n = sum(!is.na(percent)),
                percent = round(sumPercent/n, 2))
  }
  
  ## create plotting data
  
  if(level == "subconstruct" ){ ## we want subconstruct scores for a construct at a school
    m1 <- subconstructRatings %>% 
      filter(cdeSchoolNumber == schoolNum, 
             construct == selectedconstruct) 
    
    m2 <- subconstructRatings %>% ### creating district reference data
      filter(construct == selectedconstruct) %>% 
      group_by(endYear, construct, ratingTopic) %>% 
      summarize(sumRating = sum(rating, na.rm = TRUE),
                totalPossible = sum(!is.na(rating)*4), 
                rating = round(sumRating/totalPossible,1)) %>% 
      mutate(cdeSchoolNumber = "0000",
             School = "District") %>% 
      select(-sumRating, -totalPossible)
    
    m1 <- select(m1, all_of(names(m2)))
    # names(m1) = "endYear"         "construct"       "ratingTopic"     "rating"          "cdeSchoolNumber" "School"  + "topicShort"
    
    
  } else {
    #set grouper for school and for district --
    ### Rollup subconstruct scores to construct. weighting as in the spreadsheet 
    #--subconstructs weighted evenly to construct rollup.
    schoolRatings <-     subconstructRatings %>% 
      group_by(across(all_of(c(schoolgrouper, baseGroup, "construct")))) %>% 
      summarize(sumRating =sum(rating, na.rm=TRUE),
                possiblePoints = sum(!is.na(rating))*4, #each entry has the possibility of 4 points
                percent = round(sumRating/possiblePoints, 2)) %>% 
      mutate(rating = round(percent*4, 1))
    
    ##????? check George's method for computing school average -- apply to district
    if(level == "construct" ){ #if we want constructs for a school
      m1 <- schoolRatings %>%
        ungroup() %>% 
        filter(cdeSchoolNumber == schoolNum) %>% 
        select(-sumRating, -possiblePoints, -schoolName) 
      ## create district comparison -- district ratings by construct
      m2 <- overallRatings(grouper=c(baseGroup, "construct")) %>% 
        mutate(cdeSchoolNumber = "0000",
               School = "District",
               rating = round(percent*4, 1))  %>% 
        select(-sumPercent, -n)
      # names(m1) =  "cdeSchoolNumber" "School"    "rating"      "endYear"         "construct"       "percent"  
      
    } else if(level == "overall" ){ #if we want and overall rating
      m1 <- overallRatings(grouper = c(schoolgrouper, baseGroup)) %>% 
        ungroup() %>% 
        select(-schoolName) %>% 
        mutate(rating = round(percent*4, 1))
      
      m2 <- overallRatings(grouper = baseGroup) %>% 
        mutate(cdeSchoolNumber = "0000",
               School = "District",
               rating = round(percent*4, 1)) 
      # names(m1) = "cdeSchoolNumber" "School"   "rating"       "endYear"         "sumPercent"      "n"               "percent" 
    }
  }
  m <- bind_rows(m1, m2) %>% 
    mutate(School = factor(School, levels = c("District",sort(unique(m1$School))))) 
  
  if(schoolNum == "0000"){
    schoolNameShort <- "District"
  } else {
    schoolNameShort <- m1$School[1]
    # selectedconstruct <- m1$construct[[1]]
  }
  
  if(!is.na(selectedconstruct)){
    m <- m %>% 
      mutate(topicShort = paste0(str_sub(ratingTopic, start = 1, end = 25),"...",nchar(ratingTopic)) )
  }
  return(list("m"=m, "schoolNameShort"=schoolNameShort, "selectedconstruct" = selectedconstruct))
  
}
# test <- process_ratingData(level = "subconstruct", schoolNum = "3802", selectedconstruct="Assessment Practices")
# process_ratingData(level = "construct", schoolNum = "3802", selectedconstruct=NA)
# test <- process_ratingData(level = "overall", schoolNum = "3802", selectedconstruct=NA)

#' Title process_surveyData
#'
#' @param data  survey Dataframe -- 
#' @param .grouper character vector of column names to group by
#' @param .surveyRespondents string one of c("student", "educator", "parent")
#'
#' @return a summary dataframe of survey data
#' @export
#'
#' @examples   m1 <-  process_surveyData(data = surveyResponses_filter, .grouper = grouper, .surveyRespondents = surveyRespondents)
process_surveyData <- function(data, .grouper, .surveyRespondents) {
 m <-  data %>% 
    filter(respondents %in% .surveyRespondents, responseType =="selected", scale == 1) %>% 
    group_by(across(all_of(.grouper))) %>% 
   {if('schoolNameShort' %in% .grouper) filter(., !is.na(cdeSchoolNumber)) else .} %>% #if school wasn't specified, drop when grouping by school
    summarize(total = sum(!is.na(response)),
              nResponses = sum(optionValence, na.rm = TRUE),
              percentPos = nResponses/total,
              n = n_distinct(respondentID),
              sd = sd(percentPos, na.rm = TRUE))
 m
}

# source("module_surveyDisplay.R", local = TRUE)        
# 
#  
source("ui/uiDashboard.R", local = TRUE)   
# source("ui/uiCulture.R", local = T)     

# thematic_shiny()    
thematic_off()    
# %%%%%%%%%%%%%%%%%%%%%       
# UI #################     
# %%%%%%%%%%%%%%%%%%%%%       
ui <- tagList( 
  # useShinydashboard() ,   
  shinyjs::useShinyjs(),  
  tags$head(tags$style(HTML('
                        .modal-xl {
                        width: 95%;
                        }
                      '))),
  tags$head( tags$style(type="text/css", "text {font-family: sans-serif}")) ,
  
  navbarPage( 

    id="mainNavBar",     
    title =  tagList(               
      # fluidRow(   
      #   column(width = 6,    
      HTML('<div style="margin-top: -10px;">
                    <div class="header-title"><img src="logo_white.png" style="width: 21px; height: 21px;"> school insights</div>
                
                 </div>')    
       
    ),      
    ## end title  
    header = tagList(absolutePanel(top = "0.5em", right = "3em" , tags$style(type='text/css', ".selectize-input { padding: 2px; min-height: 0; font-size: 12px;} .selectize-dropdown { line-height: 9px; font-size: 12px; padding: 3px;}"),
                                   # uiOutput("yearSelect") 
                                   ### selectInput for years
                                   tagList( div(
                                     tags$style(type='text/css', ".selectize-input { padding: 2px; min-height: 0;} .selectize-dropdown { line-height: 9px; }"),
                                     
                                     pickerInput(
                                       inputId = "yearSelect",
                                       label = tags$sub(tags$span(style="color:white; font-size:10px; line-height: 4px", HTML(glue::glue("Choose years to display")))), #{tags$sub('Use delete key to remove year')}, NULL
                                       choices = surveyYears,#"Select your School" = "",
                                       options = pickerOptions(maxOptions = 4, 
                                                               maxOptionsText = "Please select no more than 4 years.",
                                                               container = "body"),
                                       selected = surveyYears[1:4] , 
                                       multiple = T,
                                       # selectize = TRUE,
                                       width = "175px"
                                       # , 
                                       # class = "dropdown" #needed if inserting in shinydashboard header
                                     ) 
                                   ))
                                   ),
                     div(
                       style = "position: absolute; right: 0.5em; top: 5em;",
                       dropdown(
                         # inputId = "dropID",
                         radioButtons(inputId = "surveyColor_btn",
                                      label = "Color scheme",
                                      choices = c("Default"="default", "High Contrast: Yellow"="cividis", 'High Contrast: Green'="viridis", "High Contrast: Red"= "plasma")),
                         size = "xs",
                         label = "Color",
                         right = TRUE,
                         icon = icon("gear", class = "opt"),
                         up = FALSE,
                       )
                     ) #end div absolute
                     ),
    windowTitle = "Cañon City School Insights",    
    position = "static-top", 
    selected = "Ratings",  
    collapsible = TRUE,       
    # footer = #list(  
    #   # div("You are logged in as: ", permissionsUI_message(id = "perm"), style = "font-size:30%")
    #   # ,
    #   # tagList("TabID: ", textOutput("navText")),
    #   # HTML("</div></div>")
    #   # ) #edn footer list     
    #   ,
    #    
    theme =  bs_theme(bootswatch = "flatly"),
    # theme =  bs_theme(bootswatch = "cerulean"),
    #theme =  bs_theme(bootswatch = "minty"),   
    # theme =  bs_theme(bootswatch = "darkly"),   
    
    #      
    # # %%%%%%%%%%%%%%%%%%%%%   
    # # MAIN DASHBOARD #########  
    # # %%%%%%%%%%%%%%%%%%%%% 
    #  
    # uiDashboard
    # tabPanel("",
    #          icon = icon("fas fa-home"),
    #          title = "Home",
    #          value = "Home",
    #          
    #          #------------------------------------------------------------------------------------#
    #          # dashboard UI ####
    #          #------------------------------------------------------------------------------------#              
    #          div(
    #            p("Add summary measures here..."),
    #            p("add trends..."),
    #            p("Add disaggregation by gender, race, grade"),
    #            p("Surveys:Add Percent Postive of questions for the district as it is available for the school...")
    #            
    #            
    #          )
    # ),#end tab1
    # # %%%%%%%%%%%%%%%%%%%%%   
    # # Ratings UI ############  
    # # %%%%%%%%%%%%%%%%%%%%%     
    tabPanel("",
             # icon = icon("fas fa-home"),
             title = "Ratings",
             value = "Ratings",
             
             #------------------------------------------------------------------------------------#
             # rating UI 
             #------------------------------------------------------------------------------------#     
             tabsetPanel(
               tabPanel(title = "Ratings across schools",
                        div(style = "position: absolute; right: 8em; top: 5em;",
                            actionButton(inputId = "plotinfo_r", label = NULL, icon = icon("info-circle")), #tagList(icon("info-circle"))
                            tippy_this("plotinfo_r", "<p style='text-align:left;'>The plot below displays the percent of points earned on the instructional review.</p>
                                            <p style='text-align:left;color:yellow'>Select a school to see detailed rating information.</p>",
                                       arrow = TRUE,
                                       placement = "bottom")
                            ),#end div
                        div(
                          # "Rating plots",
                         
                          girafeOutput("overallRatingPlot", width = "100%", height = "500px"),
                          girafeOutput("ratingPlot")
                          # ,
                          # ggiraphOutput("subconstructPlot")
                          
                        )
                        
                        ),# end first rating tab
               tabPanel(title = "Ratings across years",
                        fluidRow(
                          column(width = 3,
                                 uiOutput("schoolSelect_r")
                          ), #end col
                          column(width = 9, 
                                 
                                 girafeOutput("ratinglong", width = "100%", height = "600px")
                          ) #end col
                        ) #end row1
               )# end 2nd rating tab
             )# end rating tabset
            

    ), #end rating panel 
    # # %%%%%%%%%%%%%%%%%%%%%   
    # # Surveys ############  
    # # %%%%%%%%%%%%%%%%%%%%%     
    tabPanel("",
             # icon = icon("fas fa-home"),
             title = "Surveys",
             value = "Surveys",
             
             

             tabsetPanel(
               tabPanel(title = "Survey Overview",
                        div(style = "position: absolute; right: 8em; top: 5em;",
                            actionButton(inputId = "fav_info", label = NULL, icon = icon("info-circle")), #tagList(icon("info-circle"))
                            tippy_this("fav_info", "<p style='text-align:left;'>These plots show the percent of favorable responses to the survey items indicated. </p> 
                                             <p style='text-align:left;'>For example if two people selected 'Disagree', one person selected 'Agree' and a fourth person selected 'Strongly Agree', the item would have a 50% favorability rating.</p>",
                                       arrow = TRUE,
                                       placement = "bottom")
                        ),
                        fluidRow( 
                          # div(style = "height: 100%; Width: 100%;",
                          #   div(style = "float:left; height: 100%; Width:60%;",
                          column(width = 9, 
                                 girafeOutput("surveyDistrict", height = "300px")
                          ), # end col
                          column( width = 2, offset=1, 
                                  # ), #end div
                                  # div( style = "float:right; height: 100%; Width:40%;",
                                  checkboxGroupInput(inputId = "chk_surveyRespondent",
                                                     label = "Select respondents to plot",
                                                     choices = c("student","parent","educator"),
                                                     choiceNames = c("Student","Parent","Educator"),
                                                     selected = c("student","parent","educator"),
                                                     inline = FALSE),
                                    # ) #end div
                                  # ), end div
                          ) # end col
                        ), #end row
                        fluidRow(
                          column(width = 10,
                                 p("")
                                 ),# end col
                          column(width = 2,
                                
                                 actionButton(inputId = "plotinfo_s", label = NULL, icon = icon("info-circle")), #tagList(icon("info-circle"))
                                 tippy_this("plotinfo_s", "<p style='text-align:left;'>The plot below displays the district average percent favorable as diamonds on the bar representing the school or item percent favorable.</p>
                                            <p style='text-align:left;color:yellow'>Select a survey from a particular school to see detailed information.</p>",
                                            arrow = TRUE,
                                            placement = "bottom")
                                 ) #end col
                        ), #end row
                        girafeOutput("surveySchools")),
               tabPanel(title= "Surveys across years",
                        
                        fluidRow(
                          column(width = 3,
                                 uiOutput("schoolSelect_s"),
                                 radioButtons(inputId = "surveyType_btn",
                                              label = "Select Survey",
                                              choices = c("Student" = "student", "Family" = "parent", "Educator" = "educator")),
                                 uiOutput("demo_btn")
                          ), #end col
                          column(width = 9, 
                                 
                          girafeOutput("surveylong", width = "100%", height = "600px")
                          ) #end col
                        ) #end row1

             ) #end tabpanel
       
    )
    ) # end tabpanel
  ) # end navbarPage
)# end tagList   

server <- function(input, output, session) {              
  
  # Uncomment to activate interactive theme creation 
  # bs_themer()
  ## debugging navigation issues 
  themeSet <-       theme(plot.title = element_text(size=20, color ="grey20"),
                          axis.text.x = element_text(color = "grey20", size = 16),
                          axis.text.y = element_text(color = "grey20", size = 16),
                          axis.title.x = element_text(color = "grey20", size = 18),
                          axis.title.y = element_text(color = "grey20", size = 18),
                          strip.text = element_text(color = "grey20", size = 15)
                          )
  
  

  ### adding in masterSelect UI -- Call into UI with uiOutput("masterSelect")     
  # source("server/server_masterSelectUI.R", local = TRUE)   

# Generate years UI -------------------------------------------------------

  
  ## set default value of years to display
  session$userData$stateUIVals$selectedYears <- surveyYears[1:4]
  ## update stored value of selectedYears so that it displays across all tabs
  observeEvent(input$yearSelect,{
    
    session$userData$stateUIVals$selectedYears <- input$yearSelect
    
    cat("Updating years to display in session. Default yearSelect =", input$yearSelect, ".\n")
  })
  
  
  
  output$yearSelect   <- renderUI({
    # print(school_selectList)
    
    # #######uncomment this to run with pathways.
    
    cat("In yearSelect renderUI.  input$mainNavBar=",input$mainNavBar, "\n")
    print(session$userData$stateUIVals$selectedYears)
    
    
    
    tagList( div(
      tags$style(type='text/css', ".selectize-input { padding: 2px; min-height: 0;} .selectize-dropdown { line-height: 9px; }"),
      
      selectInput(
        inputId = "yearSelect",
        label = "Select years to display", #h3("Select your School"), NULL
        choices = surveyYears,#"Select your School" = "",
        selected = session$userData$stateUIVals$selectedYears , 
        multiple = T,
        selectize = TRUE,
        width = "200px"
        # , 
        # class = "dropdown" #needed if inserting in shinydashboard header
      ) 
    ))
    
    
  })
  
  
  
  
  ### RATING Plots ----------------------------------------------------
schoolsList <- reactive({
  schools <- schoolsTable %>% 
    arrange(schoolName)
  schoolList <- unique(schools$cdeSchoolNumber)
  names(schoolList) <- unique(schools$schoolName)
  
  schoolList
})

# Rating overall plot -----------------------------------------------------

  
  output$overallRatingPlot <- renderGirafe({
    colorScale <- input$surveyColor_btn
    validate(need(length(input$yearSelect)>0, "You must select at least one year."))
    
    mList <- process_ratingData(level = "overall", schoolNum = "0000", selectedconstruct=NA, years = input$yearSelect)
    print(mList$m)
    
    m <- mList$m %>% 
      mutate(tooltip = paste0(School, "\nPercent: ",percent*100, "%\nMean Rating: ", rating))
    
    
    schoolNames <- unique(as.character(m$School))
    schools <- schoolNames[schoolNames != "District"]
    print(schools)
    ## set colors to school and district
    if(colorScale == "default"){
      colors <- c("grey50", "steelblue")
    } else {  #c("Default"="default", "High Contrast: Yellow"="cividis", 'High Contrast: Green'="viridis", "High Contrast: Red"= "plasma"))
      colors <- do.call(colorScale, list(2)) %>% 
        rev() #reverses order of color vector so darker color is second
    }
    
    customColors <- c(colors[1], rep(colors[2], length(schools))) ## all schools should be steelblue...replicating this x the number of schools
    names(customColors) <- c("District", schools)
    print(customColors)
    
    p <-  ggplot(m, aes(y = percent, x = School, data_id = cdeSchoolNumber, tooltip = tooltip,  fill=School)) +
    
      geom_col_interactive(position = position_dodge2(width =.9, preserve = "single") , alpha = .75)+
      geom_hline_interactive(yintercept =.75, size =3, color = "darkgrey", tooltip = "Minimum Acceptable Rating")+
      annotate(geom = "text", y = .75, x = Inf, label = "Minimum Acceptable Rating", vjust = 0, hjust = 0 ,angle = -90)+
      coord_flip()+
      labs(title = "Cañon City Instructional Review: Overall ratings",
           y = "Percent of points earned")+
      facet_grid(cols = vars(endYear)) +
      scale_fill_manual_interactive(values = customColors, guide = "none")+
       # scale_fill_manual(name = "School", values = c(  "grey50", "steelblue"), guide = guide_legend(reverse = TRUE))+
      # coord_cartesian(xlim = c(1,4)) +
      theme_minimal()+
      scale_y_continuous(labels = scales::label_percent(), limits = c(0,1))+ 
      themeSet+
      theme(axis.text.x = element_text(color = "grey20", size = 13),
            panel.spacing = unit(15, "pt"),
            panel.background = element_rect(fill = NA, color = "grey90"))
    

     
    girafe_formatting(p, select = "single", width = 12, height = 6)
    # girafe(ggobj = p, width_svg = 12, height_svg = 6,
    #        options = list(opts_sizing(rescale = TRUE))) 
  })
  
  
  
  
  ratingData <- reactive({
    school1 <- input$overallRatingPlot_selected
    
    validate(need(length(input$yearSelect)>0, "You must select at least one year."))
    validate(need(school1 != "0000", "Please select a school."))
    process_ratingData(level = "construct", schoolNum = school1, selectedconstruct=NA, years = input$yearSelect)
    
    
  })

# Rating plot - schools ---------------------------------------------------

  output$ratingPlot <- renderGirafe({
    colorScale <- input$surveyColor_btn
    m <- ratingData()$m %>% 
      mutate(tooltip = paste0(construct, "\n", School, "\n",percent*100,"%", "\nRating Mean:", rating))
    
    schoolNameShort <- ratingData()$schoolNameShort
    # print(schoolNameShort)
    
    ## set colors to school and district
    customColors <- c("grey50", "steelblue")
    names(customColors) <- c("District", schoolNameShort)
    
    p <-  ggplot(m, aes(y = percent, x = construct, fill = School, data_id= construct, tooltip = tooltip))+
      geom_hline_interactive(yintercept =.75, size =3, color = "grey", tooltip = "Minimum Acceptable Rating")+
      geom_col_interactive(position = position_dodge2(width =.9, preserve = "single"), alpha = .75 )+
      labs(title = paste0("Construct ratings for ", schoolNameShort),
           x = "Construct",
           y = "Percent of points earned")+
      coord_flip()+
      facet_grid(cols = vars(endYear)) +
      # scale_fill_manual(values = colorVect, guide = guide_legend(reverse = TRUE))+
      # coord_cartesian(xlim = c(1,4)) +
      theme_minimal()+
      scale_y_continuous(labels = scales::label_percent(), limits = c(0,1))+ 
      themeSet +
      theme(axis.text.x = element_text(color = "grey20", size = 12),
            panel.spacing = unit(15, "pt"),
            panel.background = element_rect(fill = NA, color = "grey90"))
    
    if(colorScale == "default"){
      p <- p +  scale_fill_manual_interactive(values = customColors, guide = guide_legend(reverse = TRUE))+
        scale_color_manual_interactive(values = customColors, guide = guide_legend(reverse = TRUE))
    } else {  #c("Default"="default", "High Contrast: Yellow"="cividis", 'High Contrast: Green'="viridis", "High Contrast: Red"= "plasma"))
      p <- p + scale_fill_viridis(discrete = TRUE, option = colorScale, direction = -1)+
        scale_color_viridis(discrete = TRUE, option = colorScale, direction = -1)
    } 
    
    girafe_formatting(p, select = "single", width = 12, height = 6)
  })
  
# Rating plot - subconstructs ---------------------------------------------

  subConstructData <- reactive({
    school1 <- input$overallRatingPlot_selected
    validate(need(school1 != "0000", "Please select a school."))
    
    construct <- input$ratingPlot_selected
    
    validate(need(length(input$yearSelect)>0, "You must select at least one year."))
    validate(need(!is.na(construct), "Please select a construct"))
    print("the selected construct")
    print(construct)
    
    process_ratingData(level = "subconstruct", schoolNum = school1, selectedconstruct=construct, years = input$yearSelect)
    
  })
  
  output$subconstructPlot <- renderGirafe({
    colorScale <- input$surveyColor_btn
    data <- subConstructData()
    m <- data$m %>% 
      mutate(tooltip = paste0(ratingTopic, "\n",School," Rating: ",rating)) 
      # mutate(rating = factor(rating, levels = 1:4, labels = c("1 =\nIneffective", "2 =\nSomewhat Effective", "3 =\nEffective","4 =\nHighly Effective")))
    
    # print("rating subconstruct data")
    # print(m)
    
    selectedconstruct <- data$selectedconstruct
    schoolNameShort <- data$schoolNameShort
    
    
    ## set colors to school and district
    customColors <- c("grey50", "steelblue")
    names(customColors) <- c("District", schoolNameShort)
    
    # labels <- distinct(m, ratingTopic, topicShort)
    # axesLabels <- labels$ratingTopic
    # names(axesLabels) <- labels$topicShort
    
    p <- ggplot(m, aes(x = rating, y = topicShort, fill = School, tooltip = tooltip))+
      geom_vline_interactive(xintercept = 3, size =3, color = "grey", tooltip = "Minimum Acceptable Rating")+
    
      geom_col_interactive(position = position_dodge2(width =.9, preserve = "single"), alpha = .75 )+
      labs(title = paste0("Detail rating means for the ", str_to_title(selectedconstruct), " construct"),
           subtitle = schoolNameShort,
           x = "Sub-Topic",
           y = 'Rating')+
      # scale_x_discrete(labels = axesLabels)+
      coord_flip()+
      facet_grid(cols = vars(endYear)) +
      # scale_fill_manual(values = colorVect, guide = guide_legend(reverse = TRUE))+
      coord_cartesian(xlim = c(1,4)) +
      theme_minimal()+ 
      themeSet # theme(plot.title = element_text(size=20, color ="grey20"))
      # theme(plot.title = element_text(size=20, color ="grey20"))
    
    if(colorScale == "default"){
      p <- p +  scale_fill_manual_interactive(values = customColors, guide = guide_legend(reverse = TRUE))+
        scale_color_manual_interactive(values = customColors, guide = guide_legend(reverse = TRUE))
    } else {  #c("Default"="default", "High Contrast: Yellow"="cividis", 'High Contrast: Green'="viridis", "High Contrast: Red"= "plasma"))
      p <- p + scale_fill_viridis(discrete = TRUE, option = colorScale, direction = -1)+
        scale_color_viridis(discrete = TRUE, option = colorScale, direction = -1)
    } 
    
    girafe_formatting(p, select = "single", width = 12, height = 6)
    
  })
  
  ### modal for ratings
  observeEvent( input$ratingPlot_selected, {
    construct <- input$ratingPlot_selected
    
    showModal(modalDialog(
      #title = paste0( "Longitudinal summary of ", selectedStudent,"'s Engagement"),
      easyClose = TRUE,
      size = "xl",
      div(ggiraphOutput("subconstructPlot")) ,#set a scroll bar on the div , style = "font-size: 65%; width: 95%; overflow-x: scroll"
      HTML("<p style='text-align:left;color:gray'>Hover over plot elements for additional information.<br>Ratings:<br>1 = Ineffective<br>2 = Somewhat Effective<br>3 = Effective<br>4 = Highly Effective</p>")
      # ,
      # uiOutput(session$ns("kidProcessingLevel_radio"))
      
    ))
  })
  
  

# Rating Long. data prep. --------------------------------------------------

  output$schoolSelect_r <- renderUI({
    schoolList <- schoolsList()
    
    selectInput(inputId = "schoolSelect_rating",
                label = "Select a school",
                choices = schoolList)
    
  })
  
output$ratinglong <- renderGirafe({
  school1 <- input$schoolSelect_rating
  colorScale <- input$surveyColor_btn

  validate(need(length(input$yearSelect)>0, "You must select at least one year."))
  validate(need(school1 != "0000", "Please select a school."))
  dataList <- process_ratingData(level = "construct", schoolNum = school1, selectedconstruct=NA, years = input$yearSelect) # this produces a list

  m <- dataList$m %>% 
    mutate(tool_tip = paste0(construct, "\n", endYear, "\n",percent*100,"%", "\nRating Mean: ", rating),
           EndYear = as.character(endYear)) %>% 
    filter(cdeSchoolNumber == school1)
  
  schoolNameShort <- dataList$schoolNameShort
  # print(schoolNameShort)
  
  
  p <-  ggplot(m, aes(y = percent, x = EndYear, color = construct, fill = construct, data_id= construct, tooltip = tool_tip))+
    # geom_hline_interactive(yintercept =.75, size =3, color = "grey", tooltip = "Minimum Acceptable Rating")+
    geom_point_interactive(size = 7, aes(tooltip = tool_tip) , alpha = .3)+
    geom_line_interactive(aes(group = construct, tooltip = construct ), size = 2, alpha = .8)+
    geom_point_interactive(size = 4, aes(tooltip = tool_tip) , alpha = .8)+
    labs(title = paste0("Construct ratings for ", schoolNameShort),
         y = "Percent of rating points earned")+
    # coord_cartesian(xlim = c(1,4)) +
    theme_minimal()+
    scale_y_continuous(labels = scales::label_percent())+ #, limits = c(0,1) 
    themeSet 
  
  if(colorScale == "default"){
    p <- p +  scale_fill_manual_interactive(values = colorsTableau, guide = guide_legend(reverse = TRUE))+
      scale_color_manual_interactive(values = colorsTableau, guide = guide_legend(reverse = TRUE))
  } else {  #c("Default"="default", "High Contrast: Yellow"="cividis", 'High Contrast: Green'="viridis", "High Contrast: Red"= "plasma"))
    p <- p + scale_fill_viridis(discrete = TRUE, option = colorScale, direction = -1)+
      scale_color_viridis(discrete = TRUE, option = colorScale, direction = -1)
  } 
  
  girafe_formatting(p, select = "single", width = 12, height = 6)
  
  
  
})

# SURVEYS -----------------------------------------------------------------


  ## Data prep ---------------------
  surveyData_prep <- reactive({
    
    print("In surveyResponse filter")
    validate(need(length(input$yearSelect)>0, "You must select at least one year."))
    years <- input$yearSelect
    # years <- 2021:2024
    ### create dataset with joined tables for plotting
    surveyResponses_filter <- surveylookup %>% 
      filter(endYear %in% years) %>% 
      ungroup() %>% 
      select(-cdeSchoolNumber, -schoolName, -schoolNameShort) %>% #dropping school from surveylookup because some surveys cover multiple schools...join school info to the cdeschoolnumber in survey responses 
      filter(type == "survey") %>% 

      inner_join(surveyResponses) %>% #, by = c("link"= "fileAddress",  "endYear")
      left_join(select(schoolsTable, -edLevel), by = c("cdeSchoolNumber")) %>% ## add school names to each response
      inner_join(surveyQlookup) %>% #, by = c("link" = "fileAddress", "qText", "endYear", "fileAddress")
      left_join(optionsQlookup, by = c("response", "qText", "qName"))
  
    # return(list("surveyRespondents" = surveyRespondents, "surveyResponses_filter" = surveyResponses_filter))
    
  })
  

surveyDistrictSummary <- reactive({
  surveyRespondents <- input$chk_surveyRespondent
  
  surveyResponses_filter <- surveyData_prep() 
  
  ###Survey plotting overall
  # surveyRespondents <- c("parent","student", "educator")
  xAxis <- 'endYear'
  grouper <- c(xAxis, "respondents") # 'edLevel'
  
  process_surveyData(data = surveyResponses_filter, .grouper = grouper, .surveyRespondents = surveyRespondents)
})  
  

############ survey plotting ################
#### district plots -------------
  
  output$surveyDistrict <- renderGirafe({
   colorScale <- input$surveyColor_btn
    xAxis <- 'endYear'
  
    m <- surveyDistrictSummary() %>% 
      mutate(tooltip = paste0("District ",endYear,  "\nPercent Favorable: ", round(percentPos*100), "%\nn = ", n),
             endYear = as.character(endYear))
    
    p <- ggplot(m, aes(y = percentPos, x = factor(.data[[xAxis]]), tooltip = tooltip, fill = factor(endYear))) +
      geom_col_interactive(position = "dodge", alpha = .8) +
      coord_flip()+
      facet_wrap(facets = vars(respondents),
                 nrow = 3,
                 strip.position = "top")+
      
      # theme(strip.text.x = element_text(angle = 0, hjust = 0))+
      labs(title = "Percent of favorable responses on surveys: District-wide", 
           fill = "EndYear",
           x = "End Year",
           y = "Percent Favorable")+
      theme_minimal()+
      ylim (0,1) + 
      scale_y_continuous(labels = scales::label_percent(), limits = c(0,1))+
      themeSet
    
      if(colorScale == "default"){
        p <- p +  scale_fill_manual(values = colorsTableau, guide = guide_legend(reverse = TRUE))
        } else {  #c("Default"="default", "High Contrast: Yellow"="cividis", 'High Contrast: Green'="viridis", "High Contrast: Red"= "plasma"))
        p <- p + scale_fill_viridis(discrete = TRUE, option = colorScale, direction = -1)
      } 
      # scale_fill_manual(values = colorVect, guide = "none")+ 
   
      
    
    girafe_formatting(p, select = "single", width = 12, height = 6)
    
  })
  
## School plots ------------------
  output$surveySchools <- renderGirafe({
    surveyResponses_filter <- surveyData_prep() #$surveyResponses_filter
    # surveyRespondents <- c("student","parent","educator") ## testing...
    surveyRespondents <- input$chk_surveyRespondent
    districtSummary <- surveyDistrictSummary()
    colorScale <- input$surveyColor_btn
    
    # m_district <- districtSummary %>% 
    #   ungroup() %>% 
    #   filter(endYear  == max(endYear))
    # 
    # maxEndYear <- max(m_district$endYear, na.rm = T)
     print("district data for schoolplot")
     print(districtSummary)
     m_district <- districtSummary %>% 
       select(endYear, respondents, percentPos_dist = percentPos) %>% 
       mutate(tooltip_dist = glue("{endYear} District mean: {round(percentPos_dist*100)}%"))
    
    xAxis <- 'schoolNameShort'
    
    grouper <- c("endYear", "respondents", xAxis)
    
    m1 <-  process_surveyData(data = surveyResponses_filter, .grouper = grouper, .surveyRespondents = surveyRespondents)
    
    m <- m1 %>% 
      mutate(tooltip = paste0(schoolNameShort, " " ,endYear , "\nPercent Favorable: ", round(percentPos*100), "%\nn = ", n)) %>% 
      left_join(m_district, by = c("endYear", "respondents"))
    
    print(m)
  
    p <- ggplot(m, aes(y = percentPos, x = .data[[xAxis]], tooltip = tooltip, fill = factor(endYear), data_id =interaction(schoolNameShort, respondents)  )) +
      geom_col_interactive(position = position_dodge(width = .95), alpha = .8) +
      geom_point_interactive(data = m, aes(x = .data[[xAxis]], y =percentPos_dist,  tooltip = tooltip_dist),position = position_dodge(width = .95), size =4, shape = 23, color = "white")+
      geom_point_interactive(data = m, aes(x = .data[[xAxis]], y =percentPos_dist, color = factor(endYear), tooltip = tooltip_dist),position = position_dodge(width = .95), size =3, shape = 23)+
      facet_wrap(facets = vars(respondents),
                 ncol = 3,
                 # scales = "free_y",
                 strip.position = "top")+ 
      # theme(strip.text.x = element_text(angle = 0, hjust = 0))+
      labs(title = "Percent of favorable ratings on surveys by school",
           x ="",
           y = "Percent Favorable",
           fill = "End Year",
           caption = "Respondents not specifying a school were removed from school-level results.")+
      theme_minimal()+
      scale_y_continuous(labels = scales::label_percent(), limits = c(0,1))+
      coord_flip() +
      themeSet +
      theme(plot.title = element_text(size=25, color ="grey20"))
    
    if(colorScale == "default"){
      p <- p +  scale_fill_manual(values = colorsTableau, guide = guide_legend(reverse = TRUE))+
        scale_color_manual(values = colorsTableau, guide = "none")
    } else {  #c("Default"="default", "High Contrast: Yellow"="cividis", 'High Contrast: Green'="viridis", "High Contrast: Red"= "plasma"))
      p <- p + scale_fill_viridis(discrete = TRUE, option = colorScale, direction = -1)+
        scale_color_viridis(discrete = TRUE, option = colorScale, direction = -1)+
        guides(color="none")
    } 
    
    girafe_formatting(p, select = "single", width = 16, height = 8)
  })
  

### click data to pass to item-level plots
  clickData <- reactive({
    id <- input$surveySchools_selected
    print(id)
    validate(need(!is.na(id), "Select a specific school's survey to see details"))
    #create a text string from the columns of the selected row number
    id <- unlist(strsplit(id, "\\.") )
    
    id
  })
  
  ### item -level survey plots -------------------------
  output$itemPercentPos <- renderGirafe({#renderGirafe
    colorScale <- input$surveyColor_btn
    clickData <- clickData()
    surveyResponses_filter <- surveyData_prep() #$surveyResponses_filter
    xAxis <- 'qName'
    tagalongs <- c("qNum",  "qText")
    grouper <- c("endYear", 'schoolNameShort', "edLevel",  xAxis, tagalongs) ## adding edLevel here so that we can get it when we have student surveys. Woun't affect school level calculations
    # clickData <- unlist(strsplit("Lincoln.student", "\\.") )#"Lincoln.educator"
    schoolName_short <- clickData[1]
    print(schoolName_short)
    surveyRespondents <- clickData[2]
    print(surveyRespondents)
    validate(need(!is.na(schoolName_short), "Select a school/survey to see specific response details."))
    
    # create data for school
    m1 <-  process_surveyData(data = surveyResponses_filter, .grouper = grouper, .surveyRespondents = surveyRespondents)
    m2 <- m1 %>% 
      filter(schoolNameShort==schoolName_short) %>%
      mutate(qText = str_wrap(qText, width = 120)) %>% 
      mutate(tooltip = paste0(qText ,  "\n",schoolName_short, " " ,endYear, " Percent Favorable: ", round(percentPos*100), "%\nn = ", n))
    # c(1 = Ineffective; 2 = Somewhat Effective; 3 = Effective; 4 = Highly Effective)
    ## create data for district
    ## if we are looking at student survey, there are three versions...get the correct set of qs so other edlevel means don't display
    if(surveyRespondents == "student"){
      schoolEdLevel <- m1 %>% 
        filter(schoolNameShort==schoolName_short) %>% 
        pull(edLevel)
      # print("edLevel in item plot")
      # print(schoolEdLevel[1])
      district_grouper <- grouper[!grouper %in% c("schoolNameShort")]
    } else {
      district_grouper <- grouper[!grouper %in% c("schoolNameShort", "edLevel")] # don't include edLevel for non-student surveys since we want the entire district percentpos, not just the edlevel percentPos
    }
    
    ## generate district level summary and set up to join to school level summary
    m_district1 <-  process_surveyData(data = surveyResponses_filter, .grouper = district_grouper, .surveyRespondents = surveyRespondents) #run the function same as below, without the schoolGrouping
    m_district <- m_district1 %>% 
      select(endYear,  qName, qText, percentPos_dist = percentPos) %>% #respondents,
      mutate(tooltip_dist = glue("{endYear} District mean: {round(percentPos_dist*100)}%\n{qText}"))
    
    ##set factor levels for questions so they are in order
    factorLevels <- m2 %>% 
      ungroup() %>% 
      distinct( qNum, qName, qText) %>% 
      arrange(qNum)
    
    set_factorLevel <- function(data){
      data %>% 
        mutate(qName = factor(qName, levels = unique(factorLevels$qName)),
               qText = factor(qText, levels = unique(factorLevels$qText))) 
    }
    ## set join district and school data and set factors
    m <- m2 %>% 
      left_join(m_district) %>% 
      set_factorLevel() 
    
    p <- ggplot(m, aes(x = .data[[xAxis]], y = percentPos, fill = factor(endYear), tooltip = tooltip)) +
      geom_col_interactive(position = position_dodge(width = .95), alpha = .8) +
      geom_point_interactive(data = m, aes(x = .data[[xAxis]], y =percentPos_dist,  tooltip = tooltip_dist),position = position_dodge(width = .95), size =4, shape = 23, color = "white")+
      geom_point_interactive(data = m, aes(x = .data[[xAxis]], y =percentPos_dist, color = factor(endYear), tooltip = tooltip_dist),position = position_dodge(width = .95), size =3, shape = 23)+
      facet_wrap(facets = vars(qText),
                 ncol = 1, 
                 scales = "free_y",
                 strip.position = "top")+
      labs(title = paste0(schoolName_short,": Percent of favorable responses on individual questions of ",str_to_title(surveyRespondents), ' survey'),
           y = "Percent Favorable",
           x = "",
           fill = "End Year")+
      scale_y_continuous(labels = scales::label_percent(), limits = c(0,1)) +
      coord_flip()+
      theme_minimal()+
      themeSet +
      theme(strip.text.x = element_text(color = "grey20", size = 10, hjust = 0, margin = margin(t = 0, r = 0, b = 0, l = 2, unit = "cm")),
            axis.text.y = element_text(color = "grey20", size = 12))

    if(colorScale == "default"){
      p <- p +  scale_fill_manual(values = colorsTableau, guide = guide_legend(reverse = TRUE))+
        scale_color_manual(values = colorsTableau, guide = "none")
    } else {  #c("Default"="default", "High Contrast: Yellow"="cividis", 'High Contrast: Green'="viridis", "High Contrast: Red"= "plasma"))
      p <- p + scale_fill_viridis(discrete = TRUE, option = colorScale, direction = -1)+
        scale_color_viridis(discrete = TRUE, option = colorScale, direction = -1)+
        guides(color="none")
    } 
    
    p <- girafe_formatting(p, select = "single", width = 12, height = 10)
    p
    
  })
  
  ## fire up modal
  observeEvent( clickData(), {
    id <- clickData()

    outputFunction <-  girafeOutput #plotOutput
    outputObject <- "itemPercentPos"
    messageText <- "Percent favorable summary of items. (District mean displayed as a diamond)"

    showModal(modalDialog(
      #title = paste0( "Longitudinal summary of ", selectedStudent,"'s Engagement"),
      easyClose = TRUE,
      size = "xl",
      div(outputFunction(outputObject,  height = "100%", width = "100%")) ,#set a scroll bar on the div  , style = "font-size: 65%; width: 95%; overflow-x: scroll"
      helpText(messageText)
      # ,
      # uiOutput(session$ns("kidProcessingLevel_radio"))

    ))
  })
  
  
  ###Survey longitudinal data######
  longData <- reactive({
    schoolNum <- input$schoolSelect_survey
    validate(need(!is.na(schoolNum), "Waiting for UI to populate."))
    surveyResponses_filter <- surveyData_prep()
    responder <- input$surveyType_btn
    surveyType <- "survey" # set to survey since this is only surveys.
    # if(responder == "student"){
    #   ed_level <- input$surveyEdLevel_btn
    #   validate(need(!is.na(ed_level), "Waiting for UI to generate"))
    # } else { ed_level <- NA } 
    # 
    
    chosenSurvey <- surveylookup %>% 
      filter(respondents == responder, type == surveyType) %>% 
      # {if(!is.na(ed_level)) filter(.,  edLevel == ed_level) else .} %>% 
      pull(surveyID)
    
    # create a file of demos for each respondent to this survey
    demos <- surveyResponses_filter %>% 
      filter(demoQ == 1, surveyID %in% chosenSurvey) %>% 
      filter(cdeSchoolNumber == schoolNum) %>% 
      select(surveyID, respondentID, qText, response) %>% 
      left_join(demosTable) %>% 
      pivot_wider(id_cols = c(surveyID, respondentID), names_from = displayName, values_from = displayResponse) 
    
    # Identify the demo cols of this dataset
    demoCols1 <- names(demos)[names(demos) %in% unique(demosTable$displayName)]
    demoCols <- c(demoCols1, "Overall")
    print('DemoCols')
    print(demoCols)
    ### use these demoCols to create disaggregation radiobuttons to add to the grouping
    return(list("chosenSurvey" = chosenSurvey, "demoCols" = demoCols, "demos" = demos))
  })
  
## Survey long. UI --------------------
  
   output$schoolSelect_s <- renderUI({
     schoolList <- schoolsList()
     
     selectInput(inputId = "schoolSelect_survey",
                 label = "Select a school",
                 choices = schoolList)
     
   })
   
  output$demo_btn <- renderUI({
    validate(need(!is.na(longData()), "Waiting for longitudinal survey preprocessing to finish."))
    demoList <- longData()$demoCols
    
    
    radioButtons(inputId = "surveyDemo_btn",
                 label = "Select a demographic",
                 choices = demoList)
  })
  
  ### Plot long. surveys ------------------
  output$surveylong <- renderGirafe({
  
    colorScale <- input$surveyColor_btn
    responder <- input$surveyType_btn
    # responder <- "educator" ## c( "educator", "parent" ,  "student")
    demo <- input$surveyDemo_btn
    # ed_level <- input$surveyEdLevel_btn
    surveyResponses_filter <- surveyData_prep()
    data <- longData()
    chosenSurvey <- data$chosenSurvey
    demos <- data$demos
   demoCols <- data$demoCols
    validate(need(demo %in% demoCols, "Waiting for updated demo input"))
    schoolNum <- input$schoolSelect_survey
    ##for a particular survey
    # choose a survey -- need to get the distinct group of surveys by filtering on respondents, type and edlevel

    

    
    #add on survey demos to each person's responses
    surveyResp_1 <- surveyResponses_filter %>% 
      filter(surveyID %in% chosenSurvey, is.na(demoQ)) %>% #drop demo q rows since we are adding t hem as columns
      left_join(demos) %>% 
      mutate(Overall = "All respondents") # adding Overall column with same value for everyone to use as a non-grouper variable
    

    
    # demo <- demoCols[1]
    # demo <- NA
    xAxis <- 'endYear'
    schools <- c("schoolName","cdeSchoolNumber")
    grouper <- c(xAxis, demo, schools)
    grouper <- grouper[!is.na(grouper)]
    ### compute percentpos for demos
    validate(need(!is.na(demo), "Waiting for dynamic ui to populate."))
    m <- surveyResp_1 %>% 
      filter(cdeSchoolNumber == schoolNum) %>% 
      process_surveyData(.grouper = grouper, .surveyRespondents = responder) %>% 
      mutate(EndYear= as.character(endYear),
             tool_tip = paste0("Percent Favorable: ", round(percentPos*100), "%\nn = ", n,"\nYear: ", endYear, "\n",.data[[demo]])) %>% 
      {if(!is.na(demo)) filter(., !is.na(.data[[demo]])) else .} %>% 
      {if("cdeSchoolNumber" %in% grouper)filter(., !is.na(cdeSchoolNumber)) else .}
   
     ### compute percentpos for overall
    noDemo <- "Overall"
    grouper2 <- c(grouper[grouper != demo], "Overall") # dropping the demo, adding overall to the grouper
    m1 <- surveyResp_1 %>% 
      filter(cdeSchoolNumber == schoolNum) %>% 
      process_surveyData(.grouper = grouper2, .surveyRespondents = responder) %>% 
      mutate(EndYear= as.character(endYear),
             tool_tip = paste0("Percent Favorable: ", round(percentPos*100), "%\nn = ", n,"\nYear: ", endYear, "\n",.data[[noDemo]])) 

    
    if(is.na(demo)) demo <- "Overall"
    
    p <-  ggplot(m, aes(x= EndYear, y = percentPos))+

      # plotting the overall lines
      geom_point_interactive(data = m1, aes(tooltip = tool_tip), size = 7, alpha = .1)+
      geom_line_interactive(data = m1, aes( group = Overall, tooltip = Overall), size = 2, alpha = .3, linetype = "twodash")+
      geom_point_interactive(data = m1, aes(tooltip = tool_tip), size = 4, alpha = .5)+
      ## plotting the demo lines
      geom_point_interactive(size = 7, aes(tooltip = tool_tip, color = .data[[demo]] , fill = .data[[demo]]) , alpha = .3)+
      geom_line_interactive(aes(group = .data[[demo]], color = .data[[demo]] , fill = .data[[demo]], tooltip = .data[[demo]]), size = 2, alpha = .8)+
      geom_point_interactive(size = 4, aes(tooltip = tool_tip, color = .data[[demo]] , fill = .data[[demo]]) , alpha = .8)+
      
 
      
      labs(x = "End Year",
           y = "Percent of favorable responses",
           title = glue("Percent of favorable responses to the {str_to_title(responder)} survey"),
           subtitle  = glue("{str_to_title(m$schoolName[1])} grouped by {demo}"))+
      scale_y_continuous(labels = scales::label_percent())+ #, limits = c(.30,1)
      theme_minimal()+
      themeSet +
      theme(legend.position="bottom")
    
    if(colorScale == "default"){
      p <- p +  scale_fill_manual(values = colorsTableau, guide = guide_legend(reverse = TRUE))+
        scale_color_manual(values = colorsTableau, guide = guide_legend(reverse = TRUE))
    } else {  #c("Default"="default", "High Contrast: Yellow"="cividis", 'High Contrast: Green'="viridis", "High Contrast: Red"= "plasma"))
      p <- p + scale_fill_viridis(discrete = TRUE, option = colorScale, direction = -1)+
        scale_color_viridis(discrete = TRUE, option = colorScale, direction = -1)
    } 
    
    # if("cdeSchoolNumber" %in% grouper){
    #   p <- p + facet_wrap(facets = vars(schoolName),
    #                       strip.position = "top",
    #                       ncol = 1)+
    #     theme(strip.text.y = element_text(color = "grey20", size = 10, hjust = 0, margin = margin(t = 0, r = 0, b = 0, l = 2, unit = "cm"))) #,axis.text.y = element_text(color = "grey20", size = 12)
    # }
    
    p <- girafe_formatting(p, select = "single", width = 12, height = 10)
    p
    
    
    
  })
  
  ###### module calls #############        
  
  
  # surveyDisplay(id = "mainPage", school = reactive(input$masterSelect), sessionUserInfo = reactive(session$userData$sessionVals$userInfo))   
  
  
  
}

shinyApp(ui, server)