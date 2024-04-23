### source this ui directly into the server of your primary app.
observeEvent(input$masterSelect,{
  
  session$userData$stateUIVals$selectedSchool <- input$masterSelect
   
  cat("Updating schoolnumber in session. Default masterSelect =", input$masterSelect, ".\n")
})


output$masterSelect   <- renderUI({
  # print(school_selectList)
  
  # #######uncomment this to run with pathways.
  
  cat("In masterSelect renderUI.  input$mainNavBar=",input$mainNavBar, "\n")
  print(input$mainNavBar)
 
  
  # # cdonditional to remove schools that don't have pathways courses
  # if(input$mainNavBar %in% c("pathways")){
  #   validate(need(exists("student_levelcredits"), "Waiting for pathway data to load"))
  # 
  #   schoolsInPathways <- unique(student_levelcredits$schoolNumber)
  # 
  #   school_selectList <- school_selectList[school_selectList %in% schoolsInPathways]
  # }
  # 
  school_selectList <- school_selectList[names(school_selectList) != "District"] #pulling District out of middle
  school_selectList <- c("District" = "0000", school_selectList)

  
  tagList( div(
    tags$style(type='text/css', ".selectize-input { padding: 2px; min-height: 0;} .selectize-dropdown { line-height: 9px; }"),
    
    selectInput(
      inputId = "masterSelect",
      label = NULL, #h3("Select your School"),
      choices = school_selectList,#"Select your School" = "",
      selected = session$userData$stateUIVals$selectedSchool , 
      multiple = F,
      selectize = TRUE,
      width = "200px"
      # , 
      # class = "dropdown" #needed if inserting in shinydashboard header
    ) 
  ))
})


