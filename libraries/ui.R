library(shiny)
library(DT)
library(dplyr)

# set default information and load default dataset (year 2014)

years <- c(2014,2013,2012)

tf <- tempfile()
td <- tempdir()
xpt2014 <- "https://www.imls.gov/sites/default/files/pls_fy2014_data_files_csv.zip"

download.file(url = xpt2014, destfile = tf, mode = "wb")
local.fn <- unzip(tf, exdir = td)

pupld <- read.csv(local.fn[1])
pusum <- read.csv(local.fn[3])

# define variable lists

states <- c(paste(unique(pupld$STABR)))
library.default <- c(paste(pupld$LIBNAME))

variables <- c("STABR", "LIBNAME", "POPU_LSA", "CENTLIB", "BRANLIB", "LIBRARIA", "TOTSTAFF", "TOTINCM", "STAFFEXP", "PRMATEXP", "ELMATEXP", "OTHMATEX", "TOTEXPCO", "OTHOPEXP", "TOTOPEXP", "BKVOL", "EBOOK", "AUDIO_PH", "AUDIO_DL", "VIDEO_PH", "VIDEO_DL", "DATABASE", "HRS_OPEN", "VISITS", "REGBOR", "TOTCIR", "LOANTO", "LOANFM", "GPTERMS", "PITUSR", "OBEREG", "LOCALE", "MICROF")

# UI START

fluidPage(theme = "bootstrap.css",
    titlePanel("Public Libraries Survey Data Exploration"),
    sidebarLayout(
    sidebarPanel(

# ABOUT SIDEBAR DISPLAY
      
      conditionalPanel(condition="input.conditionedPanels==1",
                       tags$i(h4("Created by Olivia King, Menghan Hu, and Marisa Millenson for PHP 2560 in fall 2016."))
      ),
      
# SUMMARY SIDEBAR DISPLAY

      conditionalPanel(condition="input.conditionedPanels==2",
                       selectInput(inputId = "year", label = "YEAR", choices = years, selected = 2014),
                       br(),
                       selectInput(inputId = "state", label = "STATE", selected = "RI", 
                                   choices = states ),
                       br(),
                       selectInput(inputId = "lib", label = "LIBRARY", selected = "PROVIDENCE PUBLIC LIBRARY", choices = library.default)
      ),
      
# REGRESSION SIDEBAR DISPLAY

conditionalPanel(condition="input.conditionedPanels==3",
                                        helpText("Regression")
                 #                       selectInput(inputID = "predictor_vars", label = "PREDICTOR VARIABLES", choices = RegressionVariableList$name, multiple = TRUE, selected = RegressionVariableList$name[1]),
                 #                       br(),
                 #                       selectInput(inputID = "outcome_var", label = "OUTCOME VARIABLE", choices = RegressionVariableList$name, multiple = FALSE, selected = RegressionVariableList$name[8])
) 
    ),

mainPanel(
  tabsetPanel(

# ABOUT OUTPUT DISPLAY
        
        tabPanel(title="About", value=1,
                 h2("What can this app be used for?"),
                 "With this application you can explore the results of the Public Library Survey from 2012 to 2014. Summary data of general interest is provided at the state and individual library level; simple regression capabilities are provided at the national level. Each of the three years can be specified for any of these operations.",
                 p(),
                 "Examples of regressions that might be performed include examining the association between government-provided funding and amount of circulation materials, number of publicly available computers and number of visits, and so on. A sample regression is provided.",
                 p(),
                 h2("Where can I find the source data?"),
                "The data used for this application was provided by the Institute of Museum and Library Services and is publicly available at the", a("Public Library Survey data home page.", href="https://www.imls.gov/research-evaluation/data-collection/public-libraries-survey/explore-pls-data/pls-data"), 
                    "Full documentation and supplementary information is available there."
                 ), 

# SUMMARY OUTPUT DISPLAY

        tabPanel(title="Summary", value=2,
                 h2(textOutput("state"), style="text-transform:uppercase;"),
                 p(),
                 fluidRow(
                   column(12,
                          # short table
                          tags$div(tableOutput("tableState"), style="background-color:#ECF0F1;border-radius:15px;")
                   )
                 ),
                 fluidRow(
                   column(6,
                          # collection pie chart
                          plotOutput("pieState_material"),
                          tags$div(h4("Collection Materials"), style="text-align:center;color:#18BC9C;margin-top:-400px;margin-left:20px;")
                   ),
                   column(6,
                          # expenses pie chart
                          plotOutput("pieState_expense"),
                          tags$div(h4("Expenses"), style="text-align:center;color:#18BC9C;margin-top:-400px;margin-left:20px;")
                   )
                 ),
                fluidRow(
                  column(12,
                 tags$div(h2(textOutput("lib")), style="margin-top:350px;"),
                 p()
                  )
                ),
                 fluidRow(
                   column(12,
                          # short table
                          tags$div(tableOutput("tableLib"), style="background-color:#ECF0F1;border-radius:15px;")
                   )
                 ),
                 fluidRow(
                   column(6,
                          # collection pie chart
                          plotOutput("pieLib_material"),
                          tags$div(h4("Collection Materials"), style="text-align:center;color:#18BC9C;margin-top:-400px;margin-left:20px;")
                   ),
                   column(6,
                          # expenses pie chart
                          plotOutput("pieLib_expense"),
                          tags$div(h4("Expenses"), style="text-align:center;color:#18BC9C;margin-top:-400px;margin-left:20px;")
                   )
                 )
        ),

# REGRESSION OUTPUT DISPLAY
# see DT package for options in output table formatting

        tabPanel(title="Regression", "contents3", value=3,
         dataTableOutput('regsummary')
         ),

# other output for regression goes here

        id = "conditionedPanels"
      )
    )
   )
  )