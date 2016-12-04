# TO DO:
# rename states/territories to full names - check? (Marisa)
# rename variables we are using - on that, but need some help (Marisa)
# c("POPU_LSA", "TOTSTAFF", "TOTINCM", "BKVOL", "EBOOK", "AUDIO_DL", "VIDEO_DL", "VISITS", "REGBOR", "TOTCIR", "MICROF")

###
# TO DO FOR SUMMARY: 
# 1. results update automatically but not for YEAR
# rich.shinyapps.io/regression/ 
# 2. resort data frame by library name in alphabetical order
###

library(shiny)
library(DT)
library(broom)
library(dplyr)
library(RColorBrewer)

StateNameList <- data.frame(abb = c(state.abb, "AS", "DC", "GU", "MP", "PR", "VI"), 
                            name = c(state.name, "American Samoa", "District of Columbia", 
                                   "Guam", "Northern Mariana Islands", "Puerto Rico", "U.S. Virgin Islands"))

RegressionVariableList <- data.frame(abb=c("POPU_LSA", "TOTSTAFF", "TOTINCM", "BKVOL", "EBOOK", "AUDIO_DL", "VIDEO_DL", "VISITS", "REGBOR", "TOTCIR"),
                                     name=c("Legal Service Area Population","Total Paid Employees","Total Operating Income","Print Materials","Electronic Books","Downloadable Audio Titles","Downloadable Video Titles","Total Library Visits","Total Card Holders","Total Circulation Transactions"))




# SERVER START

shinyServer(function(input, output, session) {
  
  load("libraryData.rda")

  year <- reactive({ input$year })
  
  year <- reactive({ input$year })
  pupld <-  pupld_2014
  reactive({
    if (year == 2013) {
      pupld <-  pupld_2013
    } else if (year == 2012) {
      pupld <-  pupld_2012
    }
    
  })
  pusum <- pusum_2014
  reactive({
    if (year == 2013) {
      pusum <- pusum_2013
    } else if (year == 2012) {
      pusum <- pusum_2012
    }
    
  })
  
  observe({
    input$state
    # update list of library based on the state
    updateSelectInput(session, "lib", "LIBRARY", choices = c(paste(pupld[pupld$STABR==input$state, 'LIBNAME'])) )
  })
  # upload datasets
  
  # SUMMARY OUTPUT REACTIVE HEADERS
  output$state <- renderText({ 
    paste(StateNameList[StateNameList$abb == input$state, c('name')])
  })
  output$lib <- renderText({ 
    input$lib
  })
  
  # extract summarized information of the interested state
  stateFact <- reactive(pusum[pusum$STABR == input$state, ])
  # extract detailed information of all the libraries in the interested state
  stateLibs <- reactive(pupld[pupld$STABR== input$state, ])
  # extract detailed information of the chosen libarary 
  libFact <- reactive(pupld[pupld$STABR== input$state & pupld$LIBNAME == input$lib, ])
  
  
  
  output$pieState_material <- renderPlot({
    
    stateFact <- stateFact()
    
    number <- stateFact[, c("EBOOK", "VIDEO_PH", "VIDEO_DL", "AUDIO_PH", "AUDIO_DL")]
    data.pie <- data.frame(Categories = c("E-book", 
                                          "Video (physical)", "Video (downloadable)",
                                          "Audio (physical)", "Audio (downloadable)"),
                           Quantity = as.numeric(number))
    
    percent <- function(x, digits = 2, format = "f", ...) {
      paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
    }
    
    piecols <- brewer.pal(length(data.pie$Quantity), "Set3")
    pie(x = data.pie$Quantity, labels = percent(data.pie$Quantity/sum(data.pie$Quantity)), 
        radius = 1,  cex = 0.8, pt.cex=0.75, col = piecols)
    legend("center", legend = c(paste(data.pie$Categories)), cex = 0.8, pt.cex=0.75,fill = piecols)
  })
  
  output$pieState_expense <- renderPlot({
    stateFact <- stateFact()
    number <- stateFact[, c("STAFFEXP", "PRMATEXP", "ELMATEXP", "OTHMATEX", "OTHOPEXP")]
    data.pie <- data.frame(Categories = c("Staff Expenditures", 
                                          "Print Materials",
                                          "Electronic Materials",
                                          "All Other Materials",
                                          "Other Expenses"),
                           Quantity = as.numeric(number))
    
    percent <- function(x, digits = 2, format = "f", ...) {
      paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
    }
    
    piecols <- brewer.pal(length(data.pie$Quantity), "Set1")
    pie(x = data.pie$Quantity, labels = percent(data.pie$Quantity/sum(data.pie$Quantity)), 
        radius = 1,  cex = 0.8, col = piecols)
    legend("center", legend = c(paste(data.pie$Categories)), cex = 0.8, pt.cex=0.75,fill = piecols)
  })
  
  
  output$tableState <- renderTable({
    stateFact <- stateFact()
    stateLibs <- stateLibs()
    
    Item <- c("Number of Libraries", "Population of the Legal Service Area",
              "Opertating Revenue from State Government", "Percentage of Urban Libraries",
              "Cardholders")
    
    percent <- function(x, digits = 2, format = "f", ...) {
      paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
    }
    
    Value <- c(as.integer(nrow(stateLibs)), 
               as.integer(stateFact$POPU_LSA), 
               as.integer(stateFact$STGVT), 
               percent(mean(stateLibs$LOCALE %in% c(11,12,13))),
               as.integer(stateFact$REGBOR))
    data.frame(Item, Value)
  })
  
  output$tableLib <- renderTable({
    libFact <- libFact()
    
    Item <- c("Name of Library", "Address of Library",
              "Zip Code", 
              "Population of the Legal Service Area",
              "Annual Library Visits",
              "Cardholders", "Number of Internet Computers",
              "Geographic Location"
    )
    Value <- c(paste(libFact$LIBNAME), paste0(libFact$ADDRESS, ", ", libFact$CITY), 
               libFact$ZIP, libFact$POPU_LSA,
               libFact$VISITS, libFact$REGBOR,
               libFact$GPTERMS, 
               if (libFact$LOCALE %in% c(11,12,13)){
                 "City"
               } else if (libFact$LOCALE %in% c(21,22,23)){
                 "Suburb"
               } else if (libFact$LOCALE %in% c(31,32,33)){
                 "Town"
               } else {
                 "Rural"
               })
    data.frame(Item, Value)
    
  })
  
  output$pieLib_material <- renderPlot({
    
    libFact <- libFact()
    
    number <- libFact[, c("EBOOK", "VIDEO_PH", "VIDEO_DL", "AUDIO_PH", "AUDIO_DL")]
    data.pie <- data.frame(Categories = c("E-book", 
                                          "Video (physical)", "Video (downloadable)",
                                          "Audio (physical)", "Audio (downloadable)"),
                           Quantity = as.numeric(number))
    
    percent <- function(x, digits = 2, format = "f", ...) {
      paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
    }
    
    piecols <- brewer.pal(length(data.pie$Quantity), "Set3")
    pie(x = data.pie$Quantity, labels = percent(data.pie$Quantity/sum(data.pie$Quantity)), 
        radius = 1,  cex = 0.8, col = piecols)
    legend("center", legend = c(paste(data.pie$Categories)), cex = 0.8,pt.cex=0.75,fill = piecols)
  })
  
  output$pieLib_expense <- renderPlot({
    
    libFact <- libFact()
    
    number <- libFact[, c("STAFFEXP", "PRMATEXP", "ELMATEXP", "OTHMATEX", "OTHOPEXP")]
    data.pie <- data.frame(Categories =  c("Staff Expenditures", 
                                           "Print Materials",
                                           "Electronic Materials",
                                           "All Other Materials",
                                           "Other Expenses"),
                           Quantity = as.numeric(number))
    
    percent <- function(x, digits = 2, format = "f", ...) {
      paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
    }
    
    piecols <- brewer.pal(length(data.pie$Quantity), "Set1")
    pie(x = data.pie$Quantity, labels = percent(data.pie$Quantity/sum(data.pie$Quantity)), 
        radius = 1,  cex = 0.8, col = piecols)
    legend("center", legend = c(paste(data.pie$Categories)), cex = 0.8,fill = piecols)
  })
  
  
  
  
  # REGRESSION INPUT
  # NEED TO FIGURE OUT HOW TO ASK FOR FULL NAME OF VARIABLE, BUT HAVE INPUT
  # FOR SERVER CODE BE THE NAMES OF COLUMNS IN PUPLD
  
  # Predictor Variables
  #output$predictor_vars = renderUI({
  #  selectInput('predictor_vars', h5('Predictor Variables'), choices = RegressionVariableList$name)
  #})
  
  # output$predictor_vars <- RegressionVariableList$abb[RegressionVariableList$names==output$predictor_vars]
  
  # Outcome Variable
  #output$outcome_var = renderUI({
  #  selectInput('outcome_var', h5('Outcome Variable'), choices = RegressionVariableList$name)
  #})
  
  # output$outcome_var <- RegressionVariableList$abb[RegressionVariableList$names==output$outcome_var]
  
  # Scatterplot(s). NEED TO FIGURE OUT HOW TO MAKE ONE FOR EACH PREDICTOR
  # VARIABLE WITH OUTCOME VARIABLE
  
#  output$scatterplot <- renderPlot({
    
#    plot(pupld[,output$predictor_vars], pupld[,output$outcome_var],
#         xlab = output$predictor_vars, ylab = output$outcome_var,  main = output$predictor_vars "vs." output$outcome_var, pch = 16, 
#         col = "black", cex = 1) 
    
#  })
  
  
  # REGRESSION OUTPUT TABLE EXAMPLE
  
  model = lm(PITUSR ~ VISITS+ REGBOR+ factor(MICROF)+GPTERMS+HRS_OPEN, data = pupld )
  output$regsummary <- renderDataTable(tidy(model))

  
})
  # (BIVARIATE) REGRESSION EXAMPLE
# linear_model <- lm(output$outcome_var ~ output$predictor_vars, data=pupld)
# output$model_summary <- renderDataTable(tidy(linear_model))
  
  # For bivariate model
# output$linear_model <- renderPrint({
# summary(linear_model())
# })
  