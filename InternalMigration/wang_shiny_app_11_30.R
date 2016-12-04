# set libraries 
library(shiny)
library(shinythemes)
library(ggplot2)
library(dplyr)
library(maptools)
library(rgdal)
library(scales)
library(ggmap)
library(Cairo)
library(GGally)
library(survival)
# input data 
#sa <- read.csv("/Users/rebeccawang/Google Drive/Year 3/Sem1/stat computing/mig SA shiny app/ipumsi_00008.csv", stringsAsFactors = FALSE)
#samhs <- read.csv("/Users/rebeccawang/Google Drive/Year 2/Sem2/EHA/Assignments/Final Paper/Data/analysis_persyr.csv", stringsAsFactors = FALSE)

dat <- data.frame(cond = rep(c("A", "B"), each=200),
                  yvar = c(rnorm(200),rnorm(200, mean=.8)),
                  xvar1 = c(rnorm(100),rnorm(300, mean=.5)),
                  xvar2 = c(rnorm(200),rnorm(200, mean=.1)),
                  xvar3 = rep(c("T", "W", "P", "E"), each=100)
                  )
dataset <- diamonds

# survival anslysi code from: 
# https://www.r-statistics.com/2013/07/creating-good-looking-survival-curves-the-ggsurv-function/


################################################################################
# CODE THE USER INTERFACE
################################################################################
ui <- navbarPage(theme = shinytheme("superhero"), 
                 title="Internal Migration in South Africa",

#-------------------------------------------------------------------------------
# 1. ABOUT THIS APP
#-------------------------------------------------------------------------------
tabPanel("About",
   h1("Population Mobility in South Africa"),
   br(),
   br(),
   br(),
   h1("Event History Analysis"),
   h5("Migration as a repeat event")
), #close about panel 
#-------------------------------------------------------------------------------
# 1. EXPLORE DATA 
#-------------------------------------------------------------------------------
tabPanel("1. Explore data", 
   fluidRow(h4("Make sample restrictions or just leave at default for no restrictions"),
           column(width=4, sliderInput("in.restrict.age", "Age",
                                        min=1, max=99, value=5, step=1)), 
           column(width=4, radioButtons("in.restrict.sex", "Sex", c("Male", "Female"))), 
           column(width=4, selectInput("in.restrict.ed", "Education", c("No selection", "None", "HS", "Technical", "Other HigherEd")))
    ), #close fluidrow
   br(),
   br(),
   fluidRow(
      sidebarLayout(
      sidebarPanel(
         h4("Select two variables to compare"),
         selectInput("in.compare.var1", "Variable 1", c("yvar", "xvar1", "xvar2", "xvar3")),
         br(),
         selectInput("in.compare.var2", "Variable 2", c("yvar", "xvar1", "xvar2", "xvar3")) #close fluidrow
      ), #close sidebarPanel  
      mainPanel(
        h2("Examine the bivariate relationship"),
        plotOutput("plot1.1.1", click = "plot1.1.1_click", 
                                hover = "plot1.1.1_hover",
                                brush = "plot1.1.1_brush"), 
        verbatimTextOutput("plot1.1.1_info")
      ) #close mainPanel 
      )#close sidebarlayout 
   )#close fluidRow
), #close 1. explore data tab  

#-------------------------------------------------------------------------------
# 2. VISUALIZE RESULTS
#-------------------------------------------------------------------------------
tabPanel("2. Visualize results",
   tabsetPanel(
     
      tabPanel("Table", tableOutput("table2.1")),
      
      tabPanel("Plot", 
         plotOutput("plot2.1"),
   
         hr(),
   
         fluidRow(
            column(3,
               h4("Data Exploration"),
               sliderInput('sampleSize', 'Sample Size', 
                        min=1, max=nrow(dataset), value=min(1000, nrow(dataset)), 
                        step=500, round=0),
               br(),
               checkboxInput('jitter', 'Jitter'),
               checkboxInput('smooth', 'Smooth')
            ),
     
           column(4, offset = 1,
              selectInput('x', 'X', names(dataset)),
              selectInput('y', 'Y', names(dataset), names(dataset)[[2]]),
              selectInput('color', 'Color', c('None', names(dataset)))
           ),
     
           column(4,
              selectInput('facet_row', 'Facet Row', c(None='.', names(dataset))),
              selectInput('facet_col', 'Facet Column', c(None='.', names(dataset)))
           )
         )#close fluidRow
      )#closelast tabpanel 
   )#closetabsetPanel
), #close panel 2. visualize results

#-------------------------------------------------------------------------------
# 3. VIEW CENSUS DATA
#-------------------------------------------------------------------------------
tabPanel("3. Census", 
    fluidPage(
        column(5, plotOutput("map3.1")), 
        column(7, plotOutput("plot3.1")
        ) #close columns
   )#close fluidPage
) #close 3. view census

) # close ui 

################################################################################
# Create the outputs to show in the app 
################################################################################
server <- function(input, output) {
  
#-------------------------------------------------------------------------------
# 1. EXPLORE DATA 
#------------------------------------------------------------------------------- 
observe({print(input$in.compare.var1)})
observe({print(input$in.compare.var2)})

#plot 1.1.1
output$plot1.1.1 <- renderPlot({
   select1<- dat[ ,input$in.compare.var1]
   select2<- dat[ ,input$in.compare.var2]
   ggplot(dat, aes(x=select1, y=select2))+geom_point(shape=1)
}) #close plot1.1.1
  
#define the click, brush, and hover output
output$plot1.1.1_info <- renderText({
   xy_str <- function(e) {
      if(is.null(e)) return("NULL\n")
      paste0("x=", round(e$x, 1), " y=", round(e$y, 1), "\n")
   }
   
   xy_range_str <- function(e) {
      if(is.null(e)) return("NULL\n")
      paste0("xmin=", round(e$xmin, 1), " xmax=", round(e$xmax, 1), 
      " ymin=", round(e$ymin, 1), " ymax=", round(e$ymax, 1))
   }
   
   paste0(
   "click: ", xy_str(input$plot1.1.1_click),
   "hover: ", xy_str(input$plot1.1.1_hover),
   "brush: ", xy_range_str(input$plot1.1.1_brush)
   )
}) #close renderText

#-------------------------------------------------------------------------------
# 2. VISUALIZE RESULTS
#-------------------------------------------------------------------------------

#plot 2.1
output$plot2.1 <- renderPlot({
  data(lung)
  lung.surv <- survfit(Surv(time,status) ~ 1, data = lung)
  lung.surv2 <- survfit(Surv(time,status) ~ sex, data = lung)
   ggsurv(lung.surv2)
}) #close plot1.1.1

#-------------------------------------------------------------------------------
# 3. VIEW CENSUS DATA
#-------------------------------------------------------------------------------


} #close server 

shinyApp(ui = ui, server = server)


