#This R script will contain codes for the UI of our final project app.
#May merge with server.R if possible.
library(shiny)

#By Shiyuan (Jonathan) Miao: Here's my proposed UI framework: 
#The UI will include two tabs, one for entering inputs and one for reviewing outputs
#The tab for entering inputs will be displayed first upon opening the app
#**Still arranging codes for setting up a second tab (below)**

#tabsetPanel(id = "inTabset",
#            tabPanel("panel1", h2("This is the first panel.")),
#            tabPanel("panel2", h2("This is the second panel."))
#)

library(shinythemes)
library(shinydashboard)



#Main UI
shinyUI(fluidPage(theme = shinytheme("superhero"),
        
                  
  
  titlePanel("Which U.S Cities Should You Live In?"),
  
  #Provide a brief description about our app first
  fluidRow(
    
    column(5,
           tags$em(h4("ABOUT THIS APP")),
           h5("Graduate school was fun, we get it. But now it's time to decide where you should move to after graduation.. Worry not, 
              this app predicts the top 5 cities in the U.S that you should be living in and a couple of jobs that you could apply too"))
    
  ),
  
  #Entering inputs, one row per input
  fluidRow(
    
    column(5,
           h4("WEATHER"),
           selectInput("weather","What kind of weather do you like?", c("Snow all day everyday","Sunny all year", "Mild Winters"))
          )
    
  ),
  
  fluidRow(
    
    column(5,
           h4("SIZE OF CITY"),
           selectInput("city_size","City size preferences", c("I want to know my neighbors","I'd rather not see anyone for days", "A few people"))
          )
  ),
  
  fluidRow(
    
    column(5,
           h4("INDUSTRY"),
           selectInput("job_industry","What type of industry do you want to work in?", c("Education","Medicine", "Business/Finance"))
          )
    
  ),
  
  fluidRow(
    
    column(5,
           h4("POLITICAL PREFERENCES"),
           selectInput("politics","What best describes your political affilitions?", c("Conservative","Liberal", "Independent"))
          )
    
  ),
  
  # fluidRow(
  #   
  #   column(5,
  #          h4("...Input parameter n"),
  #          h5("...Options for parameter n")
  #          )
  #   
  # ),
  
  #Ranking the priorities of the parameters after finishing the input
  fluidRow(
    
    column(2,
           h4("Rank of preferences"),
           h5("Please all the preferences in order from most to least important  ")),
    
    column(2, 
           numericInput("indicator_input1", 
                        label = h5("Weather"), 
                        value = NULL)),
    
    column(2, 
           numericInput("indicator_input2", 
                        label = h5("Size of city"), 
                        value = NULL)),
    
    column(2, 
           numericInput("indicator_input3", 
                        label = h5("Industry"), 
                        value = NULL)),
    
    column(2, 
           numericInput("indicator_input4", 
                        label = h5("Political Preferences"), 
                        value = NULL))
    
    # column(2, 
    #        numericInput("indicator_inputN", 
    #                     label = h5("...Input parameter n"), 
    #                     value = NULL))
    
  ),
  
  #The final submit button
  fluidRow(
    
    column(5, h5("When finished entering information, click \"Submit\", then click on \"the second tab\"")),
    column(5, submitButton("Submit"))
    
  ),
  
  #Test output print that demonstrates ranking; delete when no longer needed
  fluidRow(
    
    column(10, plotOutput("myplot"))
    
  )
  
))
